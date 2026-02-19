# Configuration Editor Module
# =============================================================================
# Allows users to modify projection assumptions

#' Configuration Editor UI
#' @param id Module namespace ID
mod_config_editor_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Projection End Year - Always visible
    numericInput(
      ns("projection_end_year"),
      "Projection End Year",
      value = MAX_YEAR,
      min = 2030, max = 2150
    ),

    hr(),

    # All parameter panels in a single accordion
    accordion(
      id = ns("params"),
      open = FALSE,

      # --- Fertility ---
      accordion_panel(
        title = "Fertility Options",
        icon = icon("baby"),

        sliderInput(
          ns("ultimate_tfr"),
          "Ultimate TFR",
          min = 1.0, max = 3.0,
          value = 1.90,
          step = 0.05
        ),

        numericInput(
          ns("fertility_ultimate_year"),
          "Ultimate Year",
          value = MID_YEAR,
          min = 2030, max = MAX_YEAR
        ),

        numericInput(
          ns("weight_exponent"),
          "Interpolation Weight",
          value = 1.5,
          min = 1.0, max = 3.0,
          step = 0.1
        ),

        checkboxInput(
          ns("custom_recent_tfr"),
          "Override Recent TFR (TR year -1/-2)",
          value = FALSE
        ),

        conditionalPanel(
          condition = sprintf("input['%s']", ns("custom_recent_tfr")),
          numericInput(
            ns("custom_tfr_year1"),
            "TFR (2 years prior)",
            value = 1.62,
            min = 0.5, max = 4.0,
            step = 0.01
          ),
          numericInput(
            ns("custom_tfr_year2"),
            "TFR (1 year prior)",
            value = 1.62,
            min = 0.5, max = 4.0,
            step = 0.01
          )
        )
      ),

      # --- Mortality Options ---
      accordion_panel(
        title = "Mortality Options",
        icon = icon("heartbeat"),

        numericInput(
          ns("mortality_ultimate_year"),
          "Ultimate Year",
          value = 2043,
          min = 2030, max = 2080
        ),

        selectInput(
          ns("starting_aax_method"),
          "Mortality Projection Source",
          choices = c(
            "ARTEMIS (Regression)" = "regression",
            "TR2025 Official" = "tr_qx"
          ),
          selected = "regression"
        ),
        helpText(
          "Regression uses NCHS-derived mortality for 1980+",
          "and SSA/TR2025 historical data for pre-1980 years.",
          "TR2025 Official uses SSA data for all years."
        ),

        checkboxInput(
          ns("apply_covid_adjustments"),
          "Apply COVID-19 Adjustments",
          value = TRUE
        ),

        checkboxInput(
          ns("use_wonder_provisional"),
          "Use CDC WONDER Provisional Data",
          value = TRUE
        ),

        checkboxInput(
          ns("hmd_calibration_enabled"),
          "Apply HMD Calibration for Ages 85+",
          value = TRUE
        ),

        checkboxInput(
          ns("elderly_aax_cap_enabled"),
          "Cap AAx at Ages 85+ (~0.55%)",
          value = FALSE
        ),

        conditionalPanel(
          condition = sprintf("input['%s']", ns("elderly_aax_cap_enabled")),
          sliderInput(
            ns("elderly_aax_cap_value"),
            "Max AAx (Ages 85+)",
            min = 0.003, max = 0.010,
            value = 0.0055,
            step = 0.0005,
            post = " /yr"
          )
        )
      ),

      # --- Mortality Improvement Rates ---
      accordion_panel(
        title = "Mortality Improvement Rates",
        icon = icon("chart-line"),

        helpText(
          "Multiplier on ultimate annual improvement rates.",
          "1.0 = baseline, 1.5 = 50% faster improvement, 0.5 = 50% slower."
        ),

        sliderInput(
          ns("aax_mult_under_15"),
          "Under 15 (0-14)",
          min = 0.0, max = 3.0,
          value = 1.0,
          step = 0.05
        ),
        sliderInput(
          ns("aax_mult_age_15_49"),
          "Ages 15-49",
          min = 0.0, max = 3.0,
          value = 1.0,
          step = 0.05
        ),
        sliderInput(
          ns("aax_mult_age_50_64"),
          "Ages 50-64",
          min = 0.0, max = 3.0,
          value = 1.0,
          step = 0.05
        ),
        sliderInput(
          ns("aax_mult_age_65_84"),
          "Ages 65-84",
          min = 0.0, max = 3.0,
          value = 1.0,
          step = 0.05
        ),
        sliderInput(
          ns("aax_mult_age_85_plus"),
          "Ages 85+",
          min = 0.0, max = 3.0,
          value = 1.0,
          step = 0.05
        ),

        conditionalPanel(
          condition = sprintf("input['%s']", ns("apply_covid_adjustments")),
          hr(),
          h6("2024 COVID Factors"),
          numericInput(ns("covid_2024_age_0"), "Age 0", value = 1.01, min = 0.80, max = 1.50, step = 0.01),
          numericInput(ns("covid_2024_age_1_14"), "Ages 1-14", value = 1.17, min = 0.80, max = 1.50, step = 0.01),
          numericInput(ns("covid_2024_age_15_64"), "Ages 15-64", value = 0.99, min = 0.80, max = 1.50, step = 0.01),
          numericInput(ns("covid_2024_age_65_84"), "Ages 65-84", value = 1.02, min = 0.80, max = 1.50, step = 0.01),
          numericInput(ns("covid_2024_age_85_plus"), "Ages 85+", value = 0.98, min = 0.80, max = 1.50, step = 0.01),

          h6("2025 COVID Factors"),
          numericInput(ns("covid_2025_age_0"), "Age 0", value = 1.00, min = 0.80, max = 1.50, step = 0.01),
          numericInput(ns("covid_2025_age_1_14"), "Ages 1-14", value = 1.09, min = 0.80, max = 1.50, step = 0.01),
          numericInput(ns("covid_2025_age_15_64"), "Ages 15-64", value = 1.00, min = 0.80, max = 1.50, step = 0.01),
          numericInput(ns("covid_2025_age_65_84"), "Ages 65-84", value = 1.00, min = 0.80, max = 1.50, step = 0.01),
          numericInput(ns("covid_2025_age_85_plus"), "Ages 85+", value = 1.00, min = 0.80, max = 1.50, step = 0.01)
        )
      ),

      # --- Immigration Options ---
      accordion_panel(
        title = "Immigration Options",
        icon = icon("plane"),

        selectInput(
          ns("immigration_scenario"),
          "Immigration Scenario",
          choices = c(
            "Low Cost" = "low",
            "Intermediate" = "intermediate",
            "High Cost" = "high"
          ),
          selected = "intermediate"
        ),

        selectInput(
          ns("lpr_assumptions_source"),
          "LPR Assumptions Source",
          choices = c(
            "TR2025 V.A2 (Official)" = "va2",
            "CBO Demographic Outlook" = "cbo"
          ),
          selected = "va2"
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'cbo'", ns("lpr_assumptions_source")),
          tags$div(
            class = "alert alert-warning py-1 px-2 small mb-2",
            "CBO provides a single projection. The Immigration Scenario",
            " dropdown (Low/Intermediate/High) has no effect on LPR totals",
            " when this source is selected."
          )
        ),

        selectInput(
          ns("distribution_method"),
          "Age-Sex Distribution",
          choices = c(
            "TR2025-derived" = "tr_derived",
            "DHS Historical" = "dhs"
          ),
          selected = "tr_derived"
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'va2'", ns("lpr_assumptions_source")),
          sliderInput(
            ns("emigration_ratio"),
            "Emigration Ratio",
            min = 0.10, max = 0.50,
            value = 0.25,
            step = 0.01
          )
        ),

        selectInput(
          ns("o_population_method"),
          "O-Population Method",
          choices = c(
            "Residual + V.A2 levels (default)" = "residual",
            "V.A2 flows only" = "va2_flows"
          ),
          selected = "residual"
        ),

        selectInput(
          ns("net_o_source"),
          "Net O Immigration Source",
          choices = c(
            "V.A2 Totals (Official)" = "va2",
            "ARTEMIS Computed" = "artemis"
          ),
          selected = "va2"
        )
      ),

      # --- O-Immigration ---
      accordion_panel(
        title = "O-Immigration",
        icon = icon("globe"),

        sliderInput(
          ns("ultimate_gross_o"),
          "Ultimate Gross O (2026+)",
          min = 500000, max = 3000000,
          value = 1350000,
          step = 50000,
          pre = "",
          post = ""
        ),

        sliderInput(
          ns("transition_gross_o"),
          "Transition Year Gross O (2025)",
          min = 500000, max = 3000000,
          value = 2000000,
          step = 50000
        ),

        selectInput(
          ns("emigration_dist_source"),
          "Emigration Distribution Source",
          choices = c(
            "CBO (Recommended)" = "cbo",
            "DHS (Legacy)" = "dhs"
          ),
          selected = "cbo"
        ),

        hr(),
        h6("Departure Rate Multipliers"),
        helpText("Multipliers on base departure rate schedule. 1.0 = baseline."),

        sliderInput(ns("dr_n_pre_2015"), "N pre-2015", min = 0.10, max = 3.00, value = 1.20, step = 0.05),
        sliderInput(ns("dr_n_post_2015"), "N post-2015", min = 0.10, max = 3.00, value = 0.80, step = 0.05),
        sliderInput(ns("dr_n_recent"), "N recent arrival", min = 0.10, max = 3.00, value = 2.00, step = 0.05),
        sliderInput(ns("dr_ni_initial"), "NI initial", min = 0.10, max = 3.00, value = 0.70, step = 0.05),
        sliderInput(ns("dr_v_pre_2015"), "V pre-2015", min = 0.10, max = 3.00, value = 1.10, step = 0.05),
        sliderInput(ns("dr_v_post_2015"), "V post-2015", min = 0.10, max = 3.00, value = 0.85, step = 0.05),

        hr(),
        sliderInput(
          ns("daca_rate_reduction"),
          "DACA Rate Reduction",
          min = 0.00, max = 1.00,
          value = 0.50,
          step = 0.05
        )
      ),

      # --- LPR Elderly Immigration ---
      accordion_panel(
        title = "LPR Elderly Immigration",
        icon = icon("user-plus"),

        checkboxInput(
          ns("elderly_override_enabled"),
          "Enable Elderly Override (Ages 65+)",
          value = FALSE
        ),

        conditionalPanel(
          condition = sprintf("input['%s']", ns("elderly_override_enabled")),

          h6("Ages 65-84"),
          numericInput(ns("elderly_65_84_total"), "Annual Total", value = 60000, min = 0, max = 200000, step = 1000),
          sliderInput(ns("elderly_65_84_female"), "Female Share", min = 0.30, max = 0.80, value = 0.55, step = 0.01),

          h6("Ages 85-99"),
          numericInput(ns("elderly_85_99_total"), "Annual Total", value = 8000, min = 0, max = 200000, step = 1000),
          sliderInput(ns("elderly_85_99_female"), "Female Share", min = 0.30, max = 0.80, value = 0.54, step = 0.01),

          h6("Age 100+"),
          numericInput(ns("elderly_100_plus_total"), "Annual Total", value = 350, min = 0, max = 200000, step = 1000),
          sliderInput(ns("elderly_100_plus_female"), "Female Share", min = 0.30, max = 0.80, value = 0.65, step = 0.01)
        )
      ),

      # --- Marriage & Divorce ---
      accordion_panel(
        title = "Marriage & Divorce",
        icon = icon("heart"),

        sliderInput(
          ns("ultimate_amr"),
          "Ultimate Marriage Rate (AMR)",
          min = 2000, max = 6000,
          value = 4000,
          step = 100,
          post = " per 100K"
        ),

        sliderInput(
          ns("ultimate_adr"),
          "Ultimate Divorce Rate (ADR)",
          min = 1000, max = 2500,
          value = 1700,
          step = 50,
          post = " per 100K"
        )
      ),

      # --- Population Composition ---
      accordion_panel(
        title = "Population Composition",
        icon = icon("users"),

        sliderInput(
          ns("sex_ratio_at_birth"),
          "Sex Ratio at Birth",
          min = 1020, max = 1070,
          value = 1048,
          step = 1,
          post = " per 1000 F"
        ),

        sliderInput(
          ns("gay_percent"),
          "Gay Population %",
          min = 0.0, max = 10.0,
          value = 2.5,
          step = 0.5,
          post = "%"
        ),

        sliderInput(
          ns("lesbian_percent"),
          "Lesbian Population %",
          min = 0.0, max = 10.0,
          value = 4.5,
          step = 0.5,
          post = "%"
        ),

        sliderInput(
          ns("same_sex_fraction"),
          "Same-Sex Marriage Fraction",
          min = 0.0, max = 15.0,
          value = 4.5,
          step = 0.5,
          post = "%"
        )
      )
    ),

    hr(),

    # Data Source Options
    accordion(
      id = ns("data_source_params"),
      open = FALSE,

      accordion_panel(
        title = "Data Sources",
        icon = icon("database"),
        class = "text-secondary",

        selectInput(
          ns("historical_pop_source"),
          "Historical Population Source",
          choices = c(
            "ARTEMIS (Census all ages)" = "census",
            "ARTEMIS Hybrid (Census 0-84, SSA 85+)" = "hybrid",
            "SSA TR2025 (all ages)" = "ssa"
          ),
          selected = "census"
        )
      )
    ),

    hr(),

    # Action buttons
    div(
      class = "d-grid gap-2",
      actionButton(
        ns("apply_config"),
        "Apply Changes",
        icon = icon("check"),
        class = "btn-primary"
      ),
      actionButton(
        ns("reset_config"),
        "Reset to Baseline",
        icon = icon("undo"),
        class = "btn-outline-secondary"
      ),
      actionButton(
        ns("export_config"),
        "Export as YAML",
        icon = icon("download"),
        class = "btn-outline-info"
      )
    )
  )
}

#' Configuration Editor Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
#' @return Reactive with modified configuration
mod_config_editor_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Track config modifications
    modified_config <- reactiveVal(NULL)

    # Sync all UI inputs to match a config list
    sync_inputs_to_config <- function(config) {
      updateNumericInput(session, "projection_end_year",
        value = config$metadata$projection_period$end_year %||% MAX_YEAR)

      updateSliderInput(session, "ultimate_tfr",
        value = config$fertility$ultimate_ctfr %||% 1.90)

      updateSelectInput(session, "immigration_scenario",
        selected = config$immigration$va2_alternative %||% "intermediate")

      updateSliderInput(session, "ultimate_amr",
        value = config$marriage$ultimate_amr %||% 4000)

      updateSliderInput(session, "ultimate_adr",
        value = config$divorce$ultimate_adr %||% 1700)

      updateNumericInput(session, "fertility_ultimate_year",
        value = config$fertility$ultimate_year %||% MID_YEAR)

      updateNumericInput(session, "weight_exponent",
        value = config$fertility$weight_exponent %||% 1.5)

      updateNumericInput(session, "mortality_ultimate_year",
        value = config$mortality$ultimate_year %||% 2043)

      updateSelectInput(session, "starting_aax_method",
        selected = config$mortality$starting_aax_method)

      updateCheckboxInput(session, "apply_covid_adjustments",
        value = config$mortality$apply_covid_adjustments %||% TRUE)

      updateCheckboxInput(session, "use_wonder_provisional",
        value = config$mortality$use_wonder_provisional %||% TRUE)

      updateCheckboxInput(session, "hmd_calibration_enabled",
        value = config$mortality$hmd_calibration$enabled %||% TRUE)

      updateCheckboxInput(session, "elderly_aax_cap_enabled",
        value = config$mortality$elderly_aax_cap$enabled %||% FALSE)

      updateSliderInput(session, "elderly_aax_cap_value",
        value = config$mortality$elderly_aax_cap$max_aax %||% 0.0055)

      updateSelectInput(session, "lpr_assumptions_source",
        selected = config$immigration$lpr$assumptions_source %||% "va2")

      updateSelectInput(session, "distribution_method",
        selected = config$immigration$lpr$distribution_method %||% "tr_derived")

      updateSliderInput(session, "emigration_ratio",
        value = config$immigration$emigration$ratio %||% 0.25)

      updateSelectInput(session, "historical_pop_source",
        selected = config$historical_population$population_source %||% "census")

      updateSelectInput(session, "o_population_method",
        selected = config$historical_population$o_population$method %||% "residual")

      updateSelectInput(session, "net_o_source",
        selected = config$projected_population$net_o_source %||% "va2")

      # Check if custom recent TFR is enabled and populate values
      has_custom_tfr <- !is.null(config$fertility$custom_recent_tfr) &&
                        length(config$fertility$custom_recent_tfr) > 0
      updateCheckboxInput(session, "custom_recent_tfr", value = has_custom_tfr)

      if (has_custom_tfr) {
        tfr_values <- unlist(config$fertility$custom_recent_tfr)
        if (length(tfr_values) >= 1) {
          updateNumericInput(session, "custom_tfr_year1", value = tfr_values[1])
        }
        if (length(tfr_values) >= 2) {
          updateNumericInput(session, "custom_tfr_year2", value = tfr_values[2])
        }
      }

      # --- Advanced: Mortality Improvement Rates ---
      # AAx multipliers: compute from scenario vs baseline (mean across causes)
      # On fresh load or baseline reset, these stay at 1.0
      baseline_aax <- rv$config$mortality$ultimate_aax
      scenario_aax <- config$mortality$ultimate_aax
      for (grp in c("under_15", "age_15_49", "age_50_64", "age_65_84", "age_85_plus")) {
        mult <- 1.0
        if (!is.null(baseline_aax[[grp]]) && !is.null(scenario_aax[[grp]])) {
          base_vals <- unlist(baseline_aax[[grp]])
          scen_vals <- unlist(scenario_aax[[grp]])
          if (length(base_vals) > 0 && all(base_vals != 0)) {
            mult <- round(mean(scen_vals / base_vals), 2)
          }
        }
        updateSliderInput(session, paste0("aax_mult_", grp), value = mult)
      }

      # COVID adjustment factors
      covid <- config$mortality$covid_adjustments
      if (!is.null(covid)) {
        if (!is.null(covid[["2024"]])) {
          updateNumericInput(session, "covid_2024_age_0", value = covid[["2024"]]$age_0 %||% 1.01)
          updateNumericInput(session, "covid_2024_age_1_14", value = covid[["2024"]]$age_1_14 %||% 1.17)
          updateNumericInput(session, "covid_2024_age_15_64", value = covid[["2024"]]$age_15_64 %||% 0.99)
          updateNumericInput(session, "covid_2024_age_65_84", value = covid[["2024"]]$age_65_84 %||% 1.02)
          updateNumericInput(session, "covid_2024_age_85_plus", value = covid[["2024"]]$age_85_plus %||% 0.98)
        }
        if (!is.null(covid[["2025"]])) {
          updateNumericInput(session, "covid_2025_age_0", value = covid[["2025"]]$age_0 %||% 1.00)
          updateNumericInput(session, "covid_2025_age_1_14", value = covid[["2025"]]$age_1_14 %||% 1.09)
          updateNumericInput(session, "covid_2025_age_15_64", value = covid[["2025"]]$age_15_64 %||% 1.00)
          updateNumericInput(session, "covid_2025_age_65_84", value = covid[["2025"]]$age_65_84 %||% 1.00)
          updateNumericInput(session, "covid_2025_age_85_plus", value = covid[["2025"]]$age_85_plus %||% 1.00)
        }
      }

      # --- Advanced: O-Immigration ---
      updateSliderInput(session, "ultimate_gross_o",
        value = config$immigration$o_immigration$ultimate_gross_o %||% 1350000)
      updateSliderInput(session, "transition_gross_o",
        value = config$immigration$o_immigration$total_by_year[["2025"]] %||% 2000000)
      updateSelectInput(session, "emigration_dist_source",
        selected = config$immigration$emigration$distribution_source %||% "cbo")

      dr <- config$immigration$o_immigration$departure_rates
      if (!is.null(dr)) {
        updateSliderInput(session, "dr_n_pre_2015", value = dr$n_pre_2015_multiplier %||% 1.20)
        updateSliderInput(session, "dr_n_post_2015", value = dr$n_post_2015_multiplier %||% 0.80)
        updateSliderInput(session, "dr_n_recent", value = dr$n_recent_arrival_multiplier %||% 2.00)
        updateSliderInput(session, "dr_ni_initial", value = dr$ni_initial_multiplier %||% 0.70)
        updateSliderInput(session, "dr_v_pre_2015", value = dr$v_pre_2015_multiplier %||% 1.10)
        updateSliderInput(session, "dr_v_post_2015", value = dr$v_post_2015_multiplier %||% 0.85)
      }
      updateSliderInput(session, "daca_rate_reduction",
        value = config$immigration$o_immigration$departure_rates$daca_rate_reduction %||% 0.50)

      # --- Advanced: LPR Elderly Immigration ---
      eld <- config$immigration$lpr$elderly_override_tr_derived
      updateCheckboxInput(session, "elderly_override_enabled",
        value = if (!is.null(eld$enabled)) eld$enabled else FALSE)

      if (!is.null(eld)) {
        updateNumericInput(session, "elderly_65_84_total",
          value = eld$ages_65_84$annual_total %||% 60000)
        updateSliderInput(session, "elderly_65_84_female",
          value = eld$ages_65_84$female_share %||% 0.55)
        updateNumericInput(session, "elderly_85_99_total",
          value = eld$ages_85_99$annual_total %||% 8000)
        updateSliderInput(session, "elderly_85_99_female",
          value = eld$ages_85_99$female_share %||% 0.54)
        updateNumericInput(session, "elderly_100_plus_total",
          value = eld$age_100_plus$annual_total %||% 350)
        updateSliderInput(session, "elderly_100_plus_female",
          value = eld$age_100_plus$female_share %||% 0.65)
      }

      # --- Advanced: Population Composition ---
      updateSliderInput(session, "sex_ratio_at_birth",
        value = config$projected_population$sex_ratio_at_birth %||% 1048)
      updateSliderInput(session, "gay_percent",
        value = (config$projected_population$population_status$gay_percent %||% 0.025) * 100)
      updateSliderInput(session, "lesbian_percent",
        value = (config$projected_population$population_status$lesbian_percent %||% 0.045) * 100)
      updateSliderInput(session, "same_sex_fraction",
        value = (config$marriage$same_sex$default_fraction %||% 0.045) * 100)
    }

    # Initialize from baseline config
    observe({
      req(rv$config)
      sync_inputs_to_config(rv$config)
    })

    # Auto-enable elderly override when user manually selects tr_derived
    # ignoreInit prevents this from firing on startup (which would override
    # the actual config value loaded by sync_inputs_to_config)
    observeEvent(input$distribution_method, {
      if (isTRUE(input$distribution_method == "tr_derived")) {
        updateCheckboxInput(session, "elderly_override_enabled", value = TRUE)
      }
    }, ignoreInit = TRUE)

    # Helper: update a nested config value only if it actually changed.
    # Coerces Shiny input types (double from numericInput/sliderInput) to match
    # the original yaml::read_yaml types (integer), preserving identical hashes
    # for unchanged domain config sections (enables targets early cutoff).
    set_config <- function(config, path, new_val) {
      orig <- get_nested(config, path)

      # Coerce type to match original
      if (!is.null(orig) && !is.null(new_val)) {
        if (is.integer(orig) && is.numeric(new_val)) {
          new_val <- as.integer(new_val)
        }
      }

      # Skip if unchanged
      if (identical(new_val, orig)) return(config)

      set_nested(config, path, new_val)
    }

    # Build modified config when Apply is clicked
    observeEvent(input$apply_config, {
      req(rv$config)

      # Start with baseline config — only fields that actually changed
      # get written back, preserving original types for unchanged sections
      config <- rv$config

      # Metadata
      config <- set_config(config, c("metadata", "projection_period", "end_year"), input$projection_end_year)

      # Fertility
      config <- set_config(config, c("fertility", "ultimate_ctfr"), input$ultimate_tfr)
      config <- set_config(config, c("fertility", "ultimate_year"), input$fertility_ultimate_year)
      config <- set_config(config, c("fertility", "weight_exponent"), input$weight_exponent)

      # Custom recent TFR (special case: list or NULL)
      if (input$custom_recent_tfr) {
        tr_year <- config$metadata$trustees_report_year %||% 2025
        new_custom_tfr <- stats::setNames(
          as.list(c(input$custom_tfr_year1, input$custom_tfr_year2)),
          as.character(c(tr_year - 2, tr_year - 1))
        )
        if (!identical(new_custom_tfr, config$fertility$custom_recent_tfr)) {
          config$fertility$custom_recent_tfr <- new_custom_tfr
        }
      } else if (!is.null(config$fertility$custom_recent_tfr)) {
        config$fertility$custom_recent_tfr <- NULL
      }

      # Mortality
      config <- set_config(config, c("mortality", "ultimate_year"), input$mortality_ultimate_year)
      config <- set_config(config, c("mortality", "starting_aax_method"), input$starting_aax_method)
      config <- set_config(config, c("mortality", "apply_covid_adjustments"), input$apply_covid_adjustments)
      config <- set_config(config, c("mortality", "use_wonder_provisional"), input$use_wonder_provisional)
      config <- set_config(config, c("mortality", "hmd_calibration", "enabled"), input$hmd_calibration_enabled)
      config <- set_config(config, c("mortality", "elderly_aax_cap", "enabled"), input$elderly_aax_cap_enabled)
      config <- set_config(config, c("mortality", "elderly_aax_cap", "max_aax"), input$elderly_aax_cap_value)

      # Immigration
      config <- set_config(config, c("immigration", "va2_alternative"), input$immigration_scenario)
      config <- set_config(config, c("immigration", "lpr", "assumptions_source"), input$lpr_assumptions_source)
      config <- set_config(config, c("immigration", "lpr", "distribution_method"), input$distribution_method)
      config <- set_config(config, c("immigration", "emigration", "ratio"), input$emigration_ratio)

      # Historical Population
      config <- set_config(config, c("historical_population", "population_source"), input$historical_pop_source)
      config <- set_config(config, c("projected_population", "use_tr_historical_population"),
                           input$historical_pop_source == "ssa")
      config <- set_config(config, c("historical_population", "o_population", "method"), input$o_population_method)
      config <- set_config(config, c("projected_population", "net_o_source"), input$net_o_source)

      # --- Advanced: AAx multipliers ---
      # Scale all cause values within each age group by the multiplier
      baseline_aax <- rv$config$mortality$ultimate_aax
      for (grp in c("under_15", "age_15_49", "age_50_64", "age_65_84", "age_85_plus")) {
        mult <- input[[paste0("aax_mult_", grp)]]
        if (!is.null(mult) && mult != 1.0 && !is.null(baseline_aax[[grp]])) {
          for (cause in names(baseline_aax[[grp]])) {
            config$mortality$ultimate_aax[[grp]][[cause]] <-
              round(baseline_aax[[grp]][[cause]] * mult, 4)
          }
        }
      }

      # --- Advanced: COVID adjustment factors ---
      config <- set_config(config, c("mortality", "covid_adjustments", "2024", "age_0"), input$covid_2024_age_0)
      config <- set_config(config, c("mortality", "covid_adjustments", "2024", "age_1_14"), input$covid_2024_age_1_14)
      config <- set_config(config, c("mortality", "covid_adjustments", "2024", "age_15_64"), input$covid_2024_age_15_64)
      config <- set_config(config, c("mortality", "covid_adjustments", "2024", "age_65_84"), input$covid_2024_age_65_84)
      config <- set_config(config, c("mortality", "covid_adjustments", "2024", "age_85_plus"), input$covid_2024_age_85_plus)
      config <- set_config(config, c("mortality", "covid_adjustments", "2025", "age_0"), input$covid_2025_age_0)
      config <- set_config(config, c("mortality", "covid_adjustments", "2025", "age_1_14"), input$covid_2025_age_1_14)
      config <- set_config(config, c("mortality", "covid_adjustments", "2025", "age_15_64"), input$covid_2025_age_15_64)
      config <- set_config(config, c("mortality", "covid_adjustments", "2025", "age_65_84"), input$covid_2025_age_65_84)
      config <- set_config(config, c("mortality", "covid_adjustments", "2025", "age_85_plus"), input$covid_2025_age_85_plus)

      # --- Advanced: O-Immigration ---
      config <- set_config(config, c("immigration", "o_immigration", "ultimate_gross_o"), input$ultimate_gross_o)
      # transition_gross_o maps to total_by_year$2025 (the path the code reads)
      config <- set_config(config, c("immigration", "o_immigration", "total_by_year", "2025"), as.integer(input$transition_gross_o))
      config <- set_config(config, c("immigration", "emigration", "distribution_source"), input$emigration_dist_source)

      # Departure rate multipliers
      config <- set_config(config, c("immigration", "o_immigration", "departure_rates", "n_pre_2015_multiplier"), input$dr_n_pre_2015)
      config <- set_config(config, c("immigration", "o_immigration", "departure_rates", "n_post_2015_multiplier"), input$dr_n_post_2015)
      config <- set_config(config, c("immigration", "o_immigration", "departure_rates", "n_recent_arrival_multiplier"), input$dr_n_recent)
      config <- set_config(config, c("immigration", "o_immigration", "departure_rates", "ni_initial_multiplier"), input$dr_ni_initial)
      config <- set_config(config, c("immigration", "o_immigration", "departure_rates", "v_pre_2015_multiplier"), input$dr_v_pre_2015)
      config <- set_config(config, c("immigration", "o_immigration", "departure_rates", "v_post_2015_multiplier"), input$dr_v_post_2015)
      config <- set_config(config, c("immigration", "o_immigration", "departure_rates", "daca_rate_reduction"), input$daca_rate_reduction)

      # Auto-switch net_o_source to "artemis" when O-immigration params differ
      # from baseline, so the population projection uses ARTEMIS-computed values
      baseline_o <- rv$config$immigration$o_immigration
      ne <- function(a, b) !isTRUE(all.equal(as.numeric(a), as.numeric(b)))
      o_changed <- ne(input$ultimate_gross_o, baseline_o$ultimate_gross_o) ||
        ne(input$transition_gross_o, baseline_o$total_by_year[["2025"]]) ||
        ne(input$dr_n_pre_2015, baseline_o$departure_rates$n_pre_2015_multiplier) ||
        ne(input$dr_n_post_2015, baseline_o$departure_rates$n_post_2015_multiplier) ||
        ne(input$dr_n_recent, baseline_o$departure_rates$n_recent_arrival_multiplier) ||
        ne(input$dr_ni_initial, baseline_o$departure_rates$ni_initial_multiplier) ||
        ne(input$dr_v_pre_2015, baseline_o$departure_rates$v_pre_2015_multiplier) ||
        ne(input$dr_v_post_2015, baseline_o$departure_rates$v_post_2015_multiplier) ||
        ne(input$daca_rate_reduction, baseline_o$departure_rates$daca_rate_reduction)
      if (o_changed && input$net_o_source != "artemis") {
        config$projected_population$net_o_source <- "artemis"
        updateSelectInput(session, "net_o_source", selected = "artemis")
        showNotification(
          "Switched Net O source to 'ARTEMIS Computed' to apply O-immigration changes.",
          type = "warning"
        )
      }

      # --- Advanced: LPR Elderly Immigration ---
      config <- set_config(config, c("immigration", "lpr", "elderly_override_tr_derived", "enabled"), input$elderly_override_enabled)
      config$immigration$lpr$elderly_override_tr_derived$ages_65_84$annual_total <- as.integer(input$elderly_65_84_total)
      config$immigration$lpr$elderly_override_tr_derived$ages_65_84$female_share <- input$elderly_65_84_female
      config$immigration$lpr$elderly_override_tr_derived$ages_85_99$annual_total <- as.integer(input$elderly_85_99_total)
      config$immigration$lpr$elderly_override_tr_derived$ages_85_99$female_share <- input$elderly_85_99_female
      config$immigration$lpr$elderly_override_tr_derived$age_100_plus$annual_total <- as.integer(input$elderly_100_plus_total)
      config$immigration$lpr$elderly_override_tr_derived$age_100_plus$female_share <- input$elderly_100_plus_female

      # --- Advanced: Population Composition ---
      config <- set_config(config, c("projected_population", "sex_ratio_at_birth"), input$sex_ratio_at_birth)
      config <- set_config(config, c("projected_population", "population_status", "gay_percent"), input$gay_percent / 100)
      config <- set_config(config, c("projected_population", "population_status", "lesbian_percent"), input$lesbian_percent / 100)
      config <- set_config(config, c("marriage", "same_sex", "default_fraction"), input$same_sex_fraction / 100)

      # Store modified config
      modified_config(config)

      showNotification(
        "Configuration updated. Click 'Run Projection' to generate new results.",
        type = "message"
      )
    })

    # Reset to baseline
    observeEvent(input$reset_config, {
      tryCatch({
        config <- load_config()
        rv$config <- config
        modified_config(NULL)

        # Directly update all inputs (don't rely on observer reactivity,
        # since rv$config content may be identical to current value)
        sync_inputs_to_config(config)

        # AAx multipliers are a UI abstraction (not stored in config), force to 1.0
        for (grp in c("under_15", "age_15_49", "age_50_64", "age_65_84", "age_85_plus")) {
          updateSliderInput(session, paste0("aax_mult_", grp), value = 1.0)
        }

        showNotification("Reset to baseline", type = "message")
      }, error = function(e) {
        showNotification(paste("Reset failed:", e$message), type = "error")
      })
    })

    # Export as YAML
    observeEvent(input$export_config, {
      config <- modified_config() %||% rv$config

      if (is.null(config)) {
        showNotification("No configuration to export", type = "warning")
        return()
      }

      # Create download
      showModal(modalDialog(
        title = "Export Configuration",
        textAreaInput(
          session$ns("yaml_output"),
          "YAML Configuration",
          value = yaml::as.yaml(config),
          rows = 20,
          width = "100%"
        ),
        footer = tagList(
          downloadButton(session$ns("download_yaml"), "Download", class = "btn-primary"),
          modalButton("Close")
        ),
        size = "l"
      ))
    })

    # Download handler
    output$download_yaml <- downloadHandler(
      filename = function() {
        paste0("artemis_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yaml")
      },
      content = function(file) {
        config <- modified_config() %||% rv$config
        yaml::write_yaml(config, file)
      }
    )

    # Return modified config
    return(reactive({
      modified_config()
    }))
  })
}
