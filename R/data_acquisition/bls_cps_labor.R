#' CPS Labor Force Data Acquisition via IPUMS
#'
#' @description
#' Fetches historical CPS labor force data from IPUMS CPS for USEMP calibration.
#' Computes unemployment rates, LFPRs, labor force levels, and employment
#' at USEMP's required disaggregation:
#' - 14 age groups (16-17, 18-19, 20-24, 25-29, ..., 70-74, 75+) x 2 sexes
#' - Single-year-of-age LFPRs for ages 55-79 (replaces BLS Input #29)
#' - By marital status (never married, married spouse present, married absent)
#' - By presence of children under 6 (females 20-44)
#' - By educational attainment (ages 50+)
#'
#' Uses CPS ASEC (March Supplement) microdata, which provides the detailed
#' disaggregation BLS published tables lack. This replaces the BLS API approach
#' because BLS only publishes broad age groups (25-34, 35-44, etc.) while
#' USEMP requires 5-year groups and single-year-of-age detail.
#'
#' @references
#' - Input #24: CPS March Supplement (age 16-85+, sex, marital status, children)
#' - Input #25: CPS educational attainment by age/sex
#' - Input #26/#27: CPS civilian employment/labor force by age/sex
#' - Input #29: Older worker LFPRs by single year of age 55-79 (BLS unpublished)
#'
#' @name cps_labor_data
NULL

# =============================================================================
# Constants
# =============================================================================

#' USEMP 14 age groups for unemployment rate equations
#' @keywords internal
USEMP_AGE_GROUP_RANGES <- list(
  "16-17" = 16:17, "18-19" = 18:19, "20-24" = 20:24,
  "25-29" = 25:29, "30-34" = 30:34, "35-39" = 35:39,
  "40-44" = 40:44, "45-49" = 45:49, "50-54" = 50:54,
  "55-59" = 55:59, "60-64" = 60:64, "65-69" = 65:69,
  "70-74" = 70:74, "75+"   = 75:120
)

#' IPUMS CPS marital status code mapping
#' @keywords internal
IPUMS_MARST_MAP <- list(
  married_present = 1L,   # Married, spouse present
  married_absent  = 2L,   # Married, spouse absent
  separated       = 3L,
  divorced        = 4L,
  widowed         = 5L,
  never_married   = 6L
)

# =============================================================================
# Main Fetch Function
# =============================================================================

#' Fetch CPS labor force data via IPUMS
#'
#' @description
#' Downloads CPS ASEC microdata from IPUMS and computes labor force statistics
#' at USEMP's required disaggregation levels. Provides all CPS-based inputs
#' needed by the employment subprocess (Inputs #24, #25, #26/#27, #29).
#'
#' @param config Full ARTEMIS config
#' @param start_year First year (default: from config historical_start_year)
#' @param end_year Last year (default: base_year from config)
#' @param cache_dir Cache directory for IPUMS extracts
#' @param wait_for_extract If TRUE, wait for IPUMS extract completion
#' @param timeout_hours Max hours to wait for extract
#'
#' @return data.table with columns:
#'   year, age_group, age (single year for 55-79), sex, marital_status,
#'   child_status, concept, value
#'
#' @export
fetch_cps_labor_force <- function(config,
                                   start_year = NULL,
                                   end_year = NULL,
                                   cache_dir = here::here("data/cache/ipums_cps"),
                                   wait_for_extract = TRUE,
                                   timeout_hours = 4) {
  emp_config <- config$economics$employment
  if (is.null(start_year)) start_year <- emp_config$historical_start_year %||% 1968
  if (is.null(end_year)) end_year <- emp_config$base_year %||% 2024

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Check for cached processed data
  cache_file <- file.path(cache_dir, sprintf("cps_labor_force_%d_%d.rds", start_year, end_year))
  if (file.exists(cache_file)) {
    cli::cli_alert_success("Loading cached CPS labor force data")
    return(readRDS(cache_file))
  }

  years <- start_year:end_year

  cli::cli_alert_info("Fetching CPS labor force data via IPUMS ({start_year}-{end_year})")

  # Check for ipumsr package
  if (!requireNamespace("ipumsr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ipumsr} is required for CPS microdata",
      "i" = "Install with: renv::install('ipumsr')"
    ))
  }

  # Check for existing IPUMS extract before submitting a new one
  # Look for extract directories with the required variables
  existing_extracts <- list.dirs(cache_dir, recursive = FALSE,
                                 full.names = TRUE)
  existing_extracts <- existing_extracts[grepl("usemp_extract", existing_extracts)]

  dt <- NULL
  for (ext_dir in existing_extracts) {
    xml_files <- list.files(ext_dir, pattern = "\\.xml$", full.names = TRUE)
    if (length(xml_files) > 0) {
      tryCatch({
        ddi <- ipumsr::read_ipums_ddi(xml_files[1])
        vars <- ddi$var_info$var_name
        required <- c("AGE", "SEX", "MARST", "EMPSTAT", "LABFORCE", "ASECWT")
        if (all(required %in% vars)) {
          cli::cli_alert_success("Found existing IPUMS extract in {.path {ext_dir}}")
          raw <- ipumsr::read_ipums_micro(ddi)
          dt <- data.table::as.data.table(raw)
          break
        }
      }, error = function(e) {
        cli::cli_alert_warning("Could not read extract in {ext_dir}: {e$message}")
      })
    }
  }

  # If no existing extract, submit a new one
  if (is.null(dt)) {
    api_key <- Sys.getenv("IPUMS_API_KEY")
    if (nchar(api_key) == 0) {
      cli::cli_abort(c(
        "IPUMS_API_KEY not found in .Renviron",
        "i" = "Required for fetching CPS ASEC microdata",
        "i" = "Get a key at: https://account.ipums.org/api_keys"
      ))
    }

    sample_ids <- sprintf("cps%d_03s", years)

    extract <- ipumsr::define_extract_micro(
      collection = "cps",
      description = sprintf("ARTEMIS USEMP: CPS labor force %d-%d", start_year, end_year),
      samples = sample_ids,
      variables = c("AGE", "SEX", "MARST", "EMPSTAT", "LABFORCE",
                    "NCHLT5", "EDUC", "ASECWT", "POPSTAT")
    )

    cli::cli_alert("Submitting IPUMS CPS extract request...")
    submitted <- ipumsr::submit_extract(extract)
    extract_id <- submitted$number
    cli::cli_alert_success("Extract submitted: ID {extract_id}")

    if (!wait_for_extract) {
      cli::cli_alert_info("Extract processing in background. Check IPUMS dashboard.")
      return(invisible(list(extract_id = extract_id, collection = "cps")))
    }

    cli::cli_alert("Waiting for IPUMS CPS extract (this may take a while)...")
    ready <- ipumsr::wait_for_extract(submitted, timeout = timeout_hours * 3600, verbose = TRUE)

    if (!ipumsr::is_extract_ready(ready)) {
      cli::cli_abort(c(
        "IPUMS CPS extract timed out after {timeout_hours} hours",
        "i" = "Extract ID: {extract_id}"
      ))
    }

    extract_path <- file.path(cache_dir, sprintf("usemp_extract_%d", extract_id))
    dir.create(extract_path, showWarnings = FALSE)
    downloaded <- ipumsr::download_extract(ready, download_dir = extract_path)

    cli::cli_alert("Reading IPUMS CPS microdata...")
    raw <- ipumsr::read_ipums_micro(downloaded)
    dt <- data.table::as.data.table(raw)
  }

  result <- process_cps_labor_force_data(dt, years)

  saveRDS(result, cache_file)
  cli::cli_alert_success("Cached CPS labor force data: {nrow(result)} rows")

  result
}

# =============================================================================
# Processing Functions
# =============================================================================

#' Process CPS microdata into USEMP labor force statistics
#'
#' @param dt data.table of CPS ASEC microdata from IPUMS
#' @param years Years to process
#' @return data.table with labor force statistics at required disaggregation
#' @keywords internal
process_cps_labor_force_data <- function(dt, years) {
  cli::cli_alert_info("Processing CPS microdata for USEMP...")

  # Standard variable recoding
  dt[, year := as.integer(YEAR)]
  dt <- dt[year %in% years]

  # Sex
  dt[SEX == 1, sex := "male"]
  dt[SEX == 2, sex := "female"]

  # Labor force status from LABFORCE
  # IPUMS LABFORCE: 0=NIU, 1=Not in LF, 2=In LF
  dt[, in_labor_force := (LABFORCE == 2)]

  # Employment status from EMPSTAT
  # IPUMS EMPSTAT: 0=NIU, 1=Armed Forces, 10=At work, 12=Has job not at work,
  #                20=Unemployed (new), 21=Unemp (experienced), 22=Unemp (not classified),
  #                30-36=Not in labor force
  dt[, employed := EMPSTAT %in% c(10L, 12L)]
  dt[, unemployed := EMPSTAT %in% c(20L, 21L, 22L)]

  # Marital status
  dt[MARST == 1L, marital_status := "married_present"]
  dt[MARST == 2L, marital_status := "married_absent"]
  dt[MARST %in% 3:5, marital_status := "married_absent"]  # Separated = married absent
  dt[MARST == 6L, marital_status := "never_married"]

  # Children under 6 (for females 16-44)
  # NCHLT5: number of own children under 5 in household
  # Approximation: children under 5 ≈ children under 6
  dt[, has_child_under6 := (NCHLT5 > 0)]

  # Educational attainment (4 categories)
  # IPUMS EDUC codes vary — use broad groupings
  dt[, education_level := fcase(
    EDUC < 73, "less_than_hs",     # Less than high school diploma
    EDUC %in% 73:73, "high_school", # High school diploma/GED
    EDUC %in% 80:100, "some_college", # Some college
    EDUC >= 111, "bachelors_plus",  # Bachelor's degree or higher
    default = "high_school"         # Default for edge cases
  )]

  # Age groups
  dt[, age_group := NA_character_]
  for (grp in names(USEMP_AGE_GROUP_RANGES)) {
    ages <- USEMP_AGE_GROUP_RANGES[[grp]]
    dt[AGE %in% ages, age_group := grp]
  }
  dt <- dt[!is.na(age_group) & !is.na(sex)]

  # Population filter: civilian adults only
  # IPUMS POPSTAT: 1=Adult civilian, 2=Armed Forces, 3=Child/under 15
  # Keep adult civilians (POPSTAT == 1). LABFORCE already encodes civilian
  # labor force status, so this mainly excludes Armed Forces members.
  if ("POPSTAT" %in% names(dt)) {
    dt <- dt[as.integer(POPSTAT) == 1L | is.na(POPSTAT)]
  }

  # Weight column
  wt_col <- "ASECWT"
  if (!wt_col %in% names(dt)) {
    cli::cli_abort("Weight column {wt_col} not found in CPS data")
  }
  dt[, wt := as.numeric(get(wt_col))]
  dt <- dt[wt > 0]

  results <- list()

  # ── 1. By age group and sex (14 groups × 2 sexes) ──────────────
  cli::cli_alert("Computing labor force stats by age group × sex...")
  by_age_sex <- dt[, .(
    population = sum(wt),
    labor_force = sum(wt * in_labor_force),
    employment = sum(wt * employed),
    unemployment = sum(wt * unemployed)
  ), by = .(year, age_group, sex)]

  by_age_sex[, unemployment_rate := fifelse(labor_force > 0, unemployment / labor_force * 100, 0)]
  by_age_sex[, lfpr := fifelse(population > 0, labor_force / population, 0)]

  # Melt to long format
  results$age_sex <- data.table::melt(
    by_age_sex,
    id.vars = c("year", "age_group", "sex"),
    measure.vars = c("population", "labor_force", "employment",
                     "unemployment", "unemployment_rate", "lfpr"),
    variable.name = "concept",
    value.name = "value"
  )
  results$age_sex[, `:=`(marital_status = "all", child_status = "all", age = NA_integer_)]

  # ── 2. Single-year-of-age LFPRs for ages 55-79 (Input #29) ────
  cli::cli_alert("Computing single-year-of-age LFPRs for ages 55-79...")
  older <- dt[AGE >= 55 & AGE <= 79]
  by_single_age <- older[, .(
    population = sum(wt),
    labor_force = sum(wt * in_labor_force)
  ), by = .(year, age = as.integer(AGE), sex)]

  by_single_age[, lfpr := fifelse(population > 0, labor_force / population, 0)]

  results$single_age <- data.table::data.table(
    year = by_single_age$year,
    age_group = as.character(by_single_age$age),
    sex = by_single_age$sex,
    marital_status = "all",
    child_status = "all",
    age = by_single_age$age,
    concept = "lfpr",
    value = by_single_age$lfpr
  )

  # ── 3. By age group × sex × marital status (Input #24/#27) ─────
  cli::cli_alert("Computing labor force stats by marital status...")
  by_marital <- dt[!is.na(marital_status), .(
    population = sum(wt),
    labor_force = sum(wt * in_labor_force),
    employment = sum(wt * employed)
  ), by = .(year, age_group, sex, marital_status)]

  by_marital[, lfpr := fifelse(population > 0, labor_force / population, 0)]

  marital_long <- data.table::melt(
    by_marital,
    id.vars = c("year", "age_group", "sex", "marital_status"),
    measure.vars = c("population", "labor_force", "employment", "lfpr"),
    variable.name = "concept",
    value.name = "value"
  )
  marital_long[, `:=`(child_status = "all", age = NA_integer_)]
  results$marital <- marital_long

  # ── 4. Female 16-44 by marital status × children under 6 ───────
  cli::cli_alert("Computing female labor force by marital × children...")
  fem_young <- dt[sex == "female" & AGE >= 16 & AGE <= 44 & !is.na(marital_status)]
  by_children <- fem_young[, .(
    population = sum(wt),
    labor_force = sum(wt * in_labor_force)
  ), by = .(year, age_group, marital_status,
            child_status = fifelse(has_child_under6, "child_under6", "no_child"))]

  by_children[, lfpr := fifelse(population > 0, labor_force / population, 0)]

  children_long <- data.table::melt(
    by_children,
    id.vars = c("year", "age_group", "marital_status", "child_status"),
    measure.vars = c("population", "labor_force", "lfpr"),
    variable.name = "concept",
    value.name = "value"
  )
  children_long[, `:=`(sex = "female", age = NA_integer_)]
  results$children <- children_long

  # ── 5. Educational attainment proportions (Input #25) ───────────
  cli::cli_alert("Computing educational attainment distributions...")
  edu_data <- dt[AGE >= 25, .(
    population = sum(wt)
  ), by = .(year, age_group, sex, education_level)]

  edu_totals <- edu_data[, .(total = sum(population)), by = .(year, age_group, sex)]
  edu_data <- merge(edu_data, edu_totals, by = c("year", "age_group", "sex"))
  edu_data[, proportion := population / total]

  results$education <- data.table::data.table(
    year = edu_data$year,
    age_group = edu_data$age_group,
    sex = edu_data$sex,
    marital_status = "all",
    child_status = edu_data$education_level,  # Reuse column for education
    age = NA_integer_,
    concept = "education_proportion",
    value = edu_data$proportion
  )

  # Combine all results
  combined <- data.table::rbindlist(results, fill = TRUE, use.names = TRUE)
  data.table::setorder(combined, year, sex, age_group, concept)

  cli::cli_alert_success(
    "Processed CPS data: {nrow(combined)} rows, {length(unique(combined$year))} years"
  )

  combined
}
