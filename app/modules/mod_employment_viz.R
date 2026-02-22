# US Employment Visualization Module
# =============================================================================
# Unemployment rates, LFPR, labor force, and employment projections

# CPS historical data starts at 1968; default chart view from 2022
USEMP_HIST_MIN_YEAR <- 1968L
USEMP_DEFAULT_START <- 2022L

# Age groups for unemployment/employment charts (14 groups)
USEMP_CHART_UR_GROUPS <- c(
  "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"
)

# LFPR chart uses same 14 five-year groups as UR/employment
# (single-year ages 55+ from lfpr_projection$aggregate are aggregated at plot time)
USEMP_CHART_LFPR_GROUPS <- USEMP_CHART_UR_GROUPS

#' US Employment Visualization UI
#' @param id Module namespace ID
mod_employment_viz_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 250,

      selectInput(
        ns("chart_type"),
        "Chart Type",
        choices = c(
          "Labor Force Growth Rate" = "lf_growth",
          "Labor Force Level" = "lf_level",
          "Employment by Age Group" = "emp_ts",
          "LFPR by Age Group" = "lfpr_ts",
          "Detailed LFPR" = "lfpr_detail",
          "Unemployment Rate" = "ur_ts"
        ),
        selected = "lf_growth"
      ),

      hr(),

      sliderInput(
        ns("year_range"),
        "Year Range",
        min = USEMP_HIST_MIN_YEAR, max = MAX_YEAR,
        value = c(USEMP_DEFAULT_START, MAX_YEAR),
        step = 1,
        sep = ""
      ),

      # Controls for ur_ts / lfpr_ts / emp_ts
      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'ur_ts' || input['%s'] == 'lfpr_ts' || input['%s'] == 'emp_ts'",
          ns("chart_type"), ns("chart_type"), ns("chart_type")
        ),
        checkboxGroupInput(
          ns("age_groups"),
          "Age Groups",
          choices = USEMP_CHART_UR_GROUPS,
          selected = c("16-17", "25-29", "45-49", "55-59", "65-69")
        ),
        radioButtons(
          ns("sex_filter"),
          "Sex",
          choices = c("Both" = "both", "Male" = "male", "Female" = "female"),
          selected = "both",
          inline = TRUE
        )
      ),

      # Controls for lfpr_detail
      conditionalPanel(
        condition = sprintf("input['%s'] == 'lfpr_detail'", ns("chart_type")),
        selectInput(
          ns("detail_age_group"),
          "Age Group",
          choices = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54"),
          selected = "30-34"
        ),
        radioButtons(
          ns("detail_sex"),
          "Sex",
          choices = c("Male" = "male", "Female" = "female"),
          selected = "female",
          inline = TRUE
        )
      ),

      hr(),

      h6("Scenarios"),
      checkboxGroupInput(
        ns("compare_scenarios"),
        NULL,
        choices = c("Baseline" = "baseline"),
        selected = "baseline"
      )
    ),

    # Main content
    navset_card_tab(
      nav_panel(
        "Chart",
        card_body(
          plotlyOutput(ns("main_chart"), height = "550px")
        )
      ),
      nav_panel(
        "Data",
        card_body(
          DTOutput(ns("data_table"))
        )
      )
    )
  )
}

#' US Employment Visualization Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
mod_employment_viz_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Generate viridis palettes for age groups
    ur_age_colors <- setNames(
      viridis::viridis(length(USEMP_CHART_UR_GROUPS)),
      USEMP_CHART_UR_GROUPS
    )
    lfpr_age_colors <- ur_age_colors

    # =========================================================================
    # Historical CPS data helpers
    # =========================================================================

    # Extract historical UR from CPS (annual, by age_group and sex)
    get_historical_ur <- function() {
      cps <- rv$active_data$cps_labor_force
      if (is.null(cps)) return(NULL)
      cps[concept == "unemployment_rate" & marital_status == "all" & is.na(age),
          .(year, age_group, sex, rate = value)]
    }

    # Extract historical LFPR from CPS (annual, by age_group and sex)
    # Returns 5-year groups and single-year ages (55+)
    get_historical_lfpr <- function() {
      cps <- rv$active_data$cps_labor_force
      if (is.null(cps)) return(NULL)
      cps[concept == "lfpr",
          .(year, age_group, sex, marital_status, child_status, lfpr = value)]
    }

    # Extract historical labor force totals from CPS
    get_historical_lf <- function() {
      cps <- rv$active_data$cps_labor_force
      if (is.null(cps)) return(NULL)
      cps[concept == "labor_force" & marital_status == "all" & is.na(age),
          .(year, age_group, sex, labor_force = value)]
    }

    # Extract historical employment totals from CPS
    get_historical_emp <- function() {
      cps <- rv$active_data$cps_labor_force
      if (is.null(cps)) return(NULL)
      cps[concept == "employment" & marital_status == "all" & is.na(age),
          .(year, age_group, sex, employment = value)]
    }

    # =========================================================================
    # Reactive data helpers
    # =========================================================================

    # Update year range slider when active data or selected scenarios change
    observe({
      max_year <- MAX_YEAR

      # Check all selected scenarios for max projection year
      selected <- input$compare_scenarios
      if (is.null(selected)) selected <- "baseline"

      for (sid in selected) {
        sdata <- get_scenario_data(sid)
        if (!is.null(sdata)) {
          lf <- sdata$labor_force_employment
          if (!is.null(lf) && !is.null(lf$labor_force)) {
            max_year <- max(max_year, max(lf$labor_force$year, na.rm = TRUE))
          } else {
            ur <- sdata$unemployment_projection
            if (!is.null(ur) && !is.null(ur$actual)) {
              max_year <- max(max_year, max(ur$actual$year, na.rm = TRUE))
            }
          }
        }
      }

      current_range <- isolate(input$year_range)
      if (max_year != current_range[2]) {
        updateSliderInput(
          session, "year_range",
          max = max_year,
          value = c(current_range[1], max_year)
        )
      }
    })

    # Populate scenario selector from rv$scenarios
    observe({
      choices <- c("Active" = "active", "Baseline" = "baseline")
      if (length(rv$scenarios) > 0) {
        scenario_choices <- setNames(names(rv$scenarios), names(rv$scenarios))
        choices <- c(choices, scenario_choices)
      }
      selected <- isolate(input$compare_scenarios)
      if (is.null(selected)) selected <- "baseline"
      updateCheckboxGroupInput(
        session, "compare_scenarios",
        choices = choices,
        selected = selected
      )
    })

    # Helper: get employment data for a given scenario ID
    get_scenario_data <- function(scenario_id) {
      if (scenario_id == "active") {
        rv$active_data
      } else if (scenario_id == "baseline") {
        rv$baseline
      } else if (scenario_id %in% names(rv$scenarios)) {
        rv$scenarios[[scenario_id]]$results
      } else {
        NULL
      }
    }

    # Update age group choices when chart type switches to LFPR
    observeEvent(input$chart_type, {
      if (input$chart_type %in% c("ur_ts", "lfpr_ts", "emp_ts")) {
        updateCheckboxGroupInput(
          session, "age_groups",
          choices = USEMP_CHART_UR_GROUPS,
          selected = c("16-17", "25-29", "45-49", "55-59", "65-69")
        )
      }
    })

    # =========================================================================
    # Main chart
    # =========================================================================

    output$main_chart <- renderPlotly({
      chart_type <- input$chart_type

      if (chart_type == "lf_growth") {
        render_lf_growth_chart()
      } else if (chart_type == "lf_level") {
        render_lf_level_chart()
      } else if (chart_type == "emp_ts") {
        render_emp_chart()
      } else if (chart_type == "lfpr_ts") {
        render_lfpr_chart()
      } else if (chart_type == "lfpr_detail") {
        render_lfpr_detail_chart()
      } else if (chart_type == "ur_ts") {
        render_ur_chart()
      }
    })

    # =========================================================================
    # Chart 1: Unemployment Rate by Age Group
    # =========================================================================

    # Helper: prepare UR data for a given unemployment_projection result
    prepare_ur_data <- function(ur_actual, years, selected_groups, sex_filter) {
      proj <- copy(ur_actual)
      proj <- proj[age_group %in% selected_groups,
                   .(rate = mean(rate, na.rm = TRUE)),
                   by = .(year, age_group, sex)]
      proj <- proj[year %in% years]
      if (sex_filter == "both") {
        proj <- proj[, .(rate = mean(rate, na.rm = TRUE)), by = .(year, age_group)]
      } else {
        proj <- proj[sex == sex_filter]
      }
      proj[, age_group := factor(age_group, levels = USEMP_CHART_UR_GROUPS)]
      proj
    }

    render_ur_chart <- function() {
      selected <- input$compare_scenarios
      if (length(selected) == 0) {
        return(plotly_empty_message("Select at least one scenario"))
      }

      years <- seq(input$year_range[1], input$year_range[2])
      selected_groups <- input$age_groups
      sex_filter <- input$sex_filter

      if (is.null(selected_groups) || length(selected_groups) == 0) {
        return(plotly_empty_message("Select at least one age group"))
      }

      # Build data from all checked scenarios
      scenario_dts <- list()
      for (sid in selected) {
        sdata <- get_scenario_data(sid)
        if (!is.null(sdata$unemployment_projection$actual)) {
          s_dt <- prepare_ur_data(sdata$unemployment_projection$actual,
                                  years, selected_groups, sex_filter)
          s_dt[, scenario := sid]
          scenario_dts <- c(scenario_dts, list(s_dt))
        }
      }

      if (length(scenario_dts) == 0) {
        return(plotly_empty_message("No unemployment data in selected scenarios"))
      }

      # Single scenario: color by age group (no scenario legend)
      if (length(scenario_dts) == 1) {
        dt <- scenario_dts[[1]]
        p <- ggplot(dt, aes(x = year, y = rate, color = age_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = ur_age_colors, name = "Age Group") +
          labs(x = NULL, y = "Unemployment Rate (%)") +
          theme_artemis()
      } else {
        # Multi-scenario: first scenario solid, rest dashed
        first_sid <- selected[1]
        dt_first <- scenario_dts[[1]]
        p <- ggplot(dt_first, aes(x = year, y = rate, color = age_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = ur_age_colors, name = "Age Group") +
          labs(x = NULL, y = "Unemployment Rate (%)",
               caption = paste("Dashed:", paste(selected[-1], collapse = ", "))) +
          theme_artemis()
        for (s_dt in scenario_dts[-1]) {
          p <- p + geom_line(data = s_dt, aes(x = year, y = rate, color = age_group),
                             linetype = "dashed", linewidth = 0.6, show.legend = FALSE)
        }
      }

      if (sex_filter != "both") {
        p <- p + labs(subtitle = paste0(tools::toTitleCase(sex_filter), "s"))
      }

      ggplotly(p) |> layout_artemis(y_title = "Unemployment Rate (%)")
    }

    # =========================================================================
    # Chart 2: LFPR by Age Group
    # =========================================================================

    # Helper: prepare LFPR data for a given lfpr_projection$aggregate result
    prepare_lfpr_data <- function(lfpr_agg, years, selected_groups, sex_filter) {
      proj <- copy(lfpr_agg)
      proj[, display_group := map_to_5yr_group(age_group)]
      proj <- proj[!is.na(display_group)]
      proj <- proj[, .(lfpr = mean(lfpr, na.rm = TRUE)),
                   by = .(year, display_group, sex)]
      proj <- proj[year %in% years & display_group %in% selected_groups]
      if (sex_filter == "both") {
        proj <- proj[, .(lfpr = mean(lfpr, na.rm = TRUE)), by = .(year, display_group)]
      } else {
        proj <- proj[sex == sex_filter]
      }
      proj[, display_group := factor(display_group, levels = USEMP_CHART_LFPR_GROUPS)]
      proj
    }

    render_lfpr_chart <- function() {
      selected <- input$compare_scenarios
      if (length(selected) == 0) {
        return(plotly_empty_message("Select at least one scenario"))
      }

      years <- seq(input$year_range[1], input$year_range[2])
      selected_groups <- input$age_groups
      sex_filter <- input$sex_filter

      if (is.null(selected_groups) || length(selected_groups) == 0) {
        return(plotly_empty_message("Select at least one age group"))
      }

      # Build data from all checked scenarios
      scenario_dts <- list()
      for (sid in selected) {
        sdata <- get_scenario_data(sid)
        if (!is.null(sdata$lfpr_projection$aggregate)) {
          s_dt <- prepare_lfpr_data(sdata$lfpr_projection$aggregate,
                                    years, selected_groups, sex_filter)
          s_dt[, scenario := sid]
          scenario_dts <- c(scenario_dts, list(s_dt))
        }
      }

      if (length(scenario_dts) == 0) {
        return(plotly_empty_message("No LFPR data in selected scenarios"))
      }

      if (length(scenario_dts) == 1) {
        dt <- scenario_dts[[1]]
        p <- ggplot(dt, aes(x = year, y = lfpr, color = display_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = lfpr_age_colors, name = "Age Group") +
          scale_y_continuous(labels = scales::percent) +
          labs(x = NULL, y = "LFPR") +
          theme_artemis()
      } else {
        dt_first <- scenario_dts[[1]]
        p <- ggplot(dt_first, aes(x = year, y = lfpr, color = display_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = lfpr_age_colors, name = "Age Group") +
          scale_y_continuous(labels = scales::percent) +
          labs(x = NULL, y = "LFPR",
               caption = paste("Dashed:", paste(selected[-1], collapse = ", "))) +
          theme_artemis()
        for (s_dt in scenario_dts[-1]) {
          p <- p + geom_line(data = s_dt, aes(x = year, y = lfpr, color = display_group),
                             linetype = "dashed", linewidth = 0.6, show.legend = FALSE)
        }
      }

      if (sex_filter != "both") {
        p <- p + labs(subtitle = paste0(tools::toTitleCase(sex_filter), "s"))
      }

      ggplotly(p) |> layout_artemis(y_title = "LFPR")
    }

    # =========================================================================
    # Chart 3a: Labor Force Growth Rate
    # =========================================================================

    # Helper: map single-year ages to 5-year display groups
    map_to_5yr_group <- function(ag) {
      fcase(
        ag %in% c("16-17", "18-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54", "55-59",
                  "60-64", "65-69", "70-74", "75+"), ag,
        ag %in% as.character(55:59), "55-59",
        ag %in% as.character(60:64), "60-64",
        ag %in% as.character(65:69), "65-69",
        ag %in% as.character(70:74), "70-74",
        ag %in% c(as.character(75:100), "80+"), "75+",
        default = NA_character_
      )
    }

    # Helper to aggregate projected labor force (quarterly -> annual average)
    aggregate_proj_lf <- function(lf_dt) {
      # Sum across all age groups/sexes per quarter to get quarterly total
      agg <- lf_dt[, .(labor_force = sum(labor_force, na.rm = TRUE)),
                   by = .(year, quarter)]
      # Average across 4 quarters to get annual level
      agg[, .(labor_force = mean(labor_force, na.rm = TRUE)), by = year]
    }

    # Combine historical CPS + projected labor force, compute growth
    get_lf_series <- function(proj_lf_dt, years) {
      proj_agg <- aggregate_proj_lf(proj_lf_dt)

      hist_lf <- get_historical_lf()
      if (!is.null(hist_lf)) {
        hist_agg <- hist_lf[, .(labor_force = sum(labor_force, na.rm = TRUE)),
                            by = year]
        # Remove overlap years (prefer projection)
        hist_agg <- hist_agg[!year %in% proj_agg$year]
        agg <- rbindlist(list(hist_agg, proj_agg), use.names = TRUE)
      } else {
        agg <- proj_agg
      }

      agg <- agg[year %in% years]
      setorder(agg, year)
      agg[, growth_rate := (labor_force / shift(labor_force) - 1) * 100]
      agg
    }

    render_lf_growth_chart <- function() {
      selected <- input$compare_scenarios
      if (length(selected) == 0) {
        return(plotly_empty_message("Select at least one scenario"))
      }

      years <- seq(input$year_range[1], input$year_range[2])
      scenario_dts <- list()
      for (sid in selected) {
        sdata <- get_scenario_data(sid)
        if (!is.null(sdata$labor_force_employment$labor_force)) {
          s_agg <- get_lf_series(copy(sdata$labor_force_employment$labor_force), years)
          s_growth <- s_agg[!is.na(growth_rate)]
          s_growth[, scenario := sid]
          scenario_dts <- c(scenario_dts, list(s_growth))
        }
      }

      if (length(scenario_dts) == 0) {
        return(plotly_empty_message("No labor force data in selected scenarios"))
      }

      growth_dt <- rbindlist(scenario_dts, use.names = TRUE, fill = TRUE)

      if (length(unique(growth_dt$scenario)) > 1) {
        p <- ggplot(growth_dt, aes(x = year, y = growth_rate, color = scenario)) +
          geom_line(linewidth = 0.8) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#BDC3C7") +
          scale_color_manual(values = artemis_colors$scenarios[
            seq_along(unique(growth_dt$scenario))], name = "Scenario") +
          labs(x = NULL, y = "YoY Growth (%)") +
          theme_artemis()
      } else {
        p <- ggplot(growth_dt, aes(x = year, y = growth_rate)) +
          geom_line(linewidth = 1, color = "#E74C3C") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#BDC3C7") +
          labs(x = NULL, y = "YoY Growth (%)") +
          theme_artemis()
      }

      ggplotly(p) |> layout_artemis(y_title = "YoY Growth (%)")
    }

    # =========================================================================
    # Chart 3b: Labor Force Level
    # =========================================================================

    render_lf_level_chart <- function() {
      selected <- input$compare_scenarios
      if (length(selected) == 0) {
        return(plotly_empty_message("Select at least one scenario"))
      }

      years <- seq(input$year_range[1], input$year_range[2])
      scenario_dts <- list()
      for (sid in selected) {
        sdata <- get_scenario_data(sid)
        if (!is.null(sdata$labor_force_employment$labor_force)) {
          s_agg <- get_lf_series(copy(sdata$labor_force_employment$labor_force), years)
          s_agg[, scenario := sid]
          scenario_dts <- c(scenario_dts, list(s_agg))
        }
      }

      if (length(scenario_dts) == 0) {
        return(plotly_empty_message("No labor force data in selected scenarios"))
      }

      agg <- rbindlist(scenario_dts, use.names = TRUE, fill = TRUE)

      if (length(unique(agg$scenario)) > 1) {
        p <- ggplot(agg, aes(x = year, y = labor_force / 1e6, color = scenario)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = artemis_colors$scenarios[
            seq_along(unique(agg$scenario))], name = "Scenario") +
          labs(x = NULL, y = "Labor Force (millions)") +
          scale_y_continuous(labels = scales::comma) +
          theme_artemis()
      } else {
        p <- ggplot(agg, aes(x = year, y = labor_force / 1e6)) +
          geom_line(linewidth = 1, color = "#3498DB") +
          labs(x = NULL, y = "Labor Force (millions)") +
          scale_y_continuous(labels = scales::comma) +
          theme_artemis()
      }

      ggplotly(p) |> layout_artemis(y_title = "Labor Force (millions)")
    }

    # =========================================================================
    # Chart 4: Employment by Age Group
    # =========================================================================

    # Helper: prepare employment data for a given labor_force_employment$employment result
    prepare_emp_data <- function(emp_dt, years, selected_groups, sex_filter) {
      proj <- copy(emp_dt)
      proj[, display_group := map_to_5yr_group(age_group)]
      proj <- proj[!is.na(display_group)]
      proj <- proj[, .(employment = sum(employment, na.rm = TRUE) / 4),
                   by = .(year, display_group, sex)]
      proj <- proj[year %in% years & display_group %in% selected_groups]
      if (sex_filter == "both") {
        proj <- proj[, .(employment = sum(employment, na.rm = TRUE)), by = .(year, display_group)]
      } else {
        proj <- proj[sex == sex_filter]
      }
      proj[, display_group := factor(display_group, levels = USEMP_CHART_UR_GROUPS)]
      proj
    }

    render_emp_chart <- function() {
      selected <- input$compare_scenarios
      if (length(selected) == 0) {
        return(plotly_empty_message("Select at least one scenario"))
      }

      years <- seq(input$year_range[1], input$year_range[2])
      selected_groups <- input$age_groups
      sex_filter <- input$sex_filter

      if (is.null(selected_groups) || length(selected_groups) == 0) {
        return(plotly_empty_message("Select at least one age group"))
      }

      # Build data from all checked scenarios
      scenario_dts <- list()
      for (sid in selected) {
        sdata <- get_scenario_data(sid)
        if (!is.null(sdata$labor_force_employment$employment)) {
          s_dt <- prepare_emp_data(sdata$labor_force_employment$employment,
                                   years, selected_groups, sex_filter)
          s_dt[, scenario := sid]
          scenario_dts <- c(scenario_dts, list(s_dt))
        }
      }

      if (length(scenario_dts) == 0) {
        return(plotly_empty_message("No employment data in selected scenarios"))
      }

      if (length(scenario_dts) == 1) {
        dt <- scenario_dts[[1]]
        p <- ggplot(dt, aes(x = year, y = employment / 1e3, color = display_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = ur_age_colors, name = "Age Group") +
          scale_y_continuous(labels = scales::comma) +
          labs(x = NULL, y = "Employment (thousands)") +
          theme_artemis()
      } else {
        dt_first <- scenario_dts[[1]]
        p <- ggplot(dt_first, aes(x = year, y = employment / 1e3, color = display_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = ur_age_colors, name = "Age Group") +
          scale_y_continuous(labels = scales::comma) +
          labs(x = NULL, y = "Employment (thousands)",
               caption = paste("Dashed:", paste(selected[-1], collapse = ", "))) +
          theme_artemis()
        for (s_dt in scenario_dts[-1]) {
          p <- p + geom_line(data = s_dt, aes(x = year, y = employment / 1e3,
                                               color = display_group),
                             linetype = "dashed", linewidth = 0.6, show.legend = FALSE)
        }
      }

      if (sex_filter != "both") {
        p <- p + labs(subtitle = paste0(tools::toTitleCase(sex_filter), "s"))
      }

      ggplotly(p) |> layout_artemis(y_title = "Employment (thousands)")
    }

    # =========================================================================
    # Chart 5: Detailed LFPR (Marital/Children)
    # =========================================================================

    render_lfpr_detail_chart <- function() {
      selected <- input$compare_scenarios
      if (length(selected) == 0) {
        return(plotly_empty_message("Select at least one scenario"))
      }

      # Use first selected scenario for detailed view
      sdata <- get_scenario_data(selected[1])
      lfpr_data <- sdata$lfpr_projection
      if (is.null(lfpr_data) || is.null(lfpr_data$detailed)) {
        return(plotly_empty_message("Detailed LFPR data not available"))
      }

      years <- seq(input$year_range[1], input$year_range[2])
      sel_age <- input$detail_age_group
      sel_sex <- input$detail_sex

      # Projected detailed LFPR
      proj <- copy(lfpr_data$detailed)
      proj <- proj[age_group == sel_age & sex == sel_sex & marital_status != "all"]

      # Splice historical CPS detailed LFPR
      hist_lfpr <- get_historical_lfpr()
      if (!is.null(hist_lfpr)) {
        hist_detail <- hist_lfpr[age_group == sel_age & sex == sel_sex &
                                   marital_status != "all"]
        if (nrow(hist_detail) > 0) {
          hist_detail <- hist_detail[!year %in% proj$year]
          dt <- rbindlist(list(hist_detail, proj), use.names = TRUE, fill = TRUE)
        } else {
          dt <- proj
        }
      } else {
        dt <- proj
      }

      dt <- dt[year %in% years]

      # Skip "all" aggregates — show disaggregated series only
      dt <- dt[marital_status != "all"]

      if (nrow(dt) == 0) {
        return(plotly_empty_message(
          paste("No detailed LFPR data for", sel_age, sel_sex)
        ))
      }

      # Build display labels combining marital and child status
      dt[, series_label := fifelse(
        child_status == "all" | is.na(child_status),
        marital_status,
        paste0(marital_status, " (", gsub("_", " ", child_status), ")")
      )]

      # Color mapping using artemis marital colors
      marital_color_map <- c(
        "never_married" = artemis_colors$marital[["single"]],
        "married_present" = artemis_colors$marital[["married"]],
        "married_absent" = artemis_colors$marital[["divorced"]]
      )

      # Unique series
      series <- unique(dt$series_label)
      n_series <- length(series)

      # Build color palette: base color for each marital status
      series_colors <- character(n_series)
      names(series_colors) <- series
      for (s in series) {
        # Extract base marital status from label
        base_ms <- sub(" \\(.*\\)$", "", s)
        base_col <- marital_color_map[[base_ms]]
        if (is.null(base_col)) base_col <- "#7F8C8D"
        series_colors[s] <- base_col
      }

      # Determine linetype: child_under6 = dashed
      dt[, lt := fifelse(child_status == "child_under6", "dashed", "solid")]

      p <- ggplot(dt, aes(x = year, y = lfpr, color = series_label, linetype = lt,
                           group = series_label)) +
        geom_line(linewidth = 0.8) +
        scale_color_manual(values = series_colors, name = "Category") +
        scale_linetype_identity() +
        scale_y_continuous(labels = scales::percent) +
        labs(x = NULL, y = "LFPR",
             subtitle = paste0(sel_age, ", ", tools::toTitleCase(sel_sex))) +
        theme_artemis()

      ggplotly(p) |> layout_artemis(y_title = "LFPR")
    }

    # =========================================================================
    # Data table
    # =========================================================================

    output$data_table <- renderDT({
      chart_type <- input$chart_type
      years <- seq(input$year_range[1], input$year_range[2])

      if (chart_type == "ur_ts") {
        ur_data <- rv$active_data$unemployment_projection
        req(ur_data, ur_data$actual)
        proj <- copy(ur_data$actual)
        proj <- proj[, .(rate = round(mean(rate, na.rm = TRUE), 2)),
                     by = .(year, age_group, sex)]
        hist_ur <- get_historical_ur()
        if (!is.null(hist_ur)) {
          if (max(hist_ur$rate, na.rm = TRUE) <= 1) hist_ur[, rate := rate * 100]
          hist_ur[, rate := round(rate, 2)]
          hist_ur <- hist_ur[!year %in% proj$year]
          dt <- rbindlist(list(hist_ur, proj), use.names = TRUE, fill = TRUE)
        } else {
          dt <- proj
        }
        dt <- dt[year %in% years]
        wide <- dcast(dt, year + sex ~ age_group, value.var = "rate")
        datatable(wide, options = list(scrollX = TRUE, pageLength = 15)) |>
          formatRound(columns = 3:ncol(wide), digits = 2)

      } else if (chart_type == "lfpr_ts") {
        lfpr_data <- rv$active_data$lfpr_projection
        req(lfpr_data, lfpr_data$aggregate)
        proj <- copy(lfpr_data$aggregate)
        proj[, display_group := map_to_5yr_group(age_group)]
        proj <- proj[!is.na(display_group)]
        proj <- proj[, .(lfpr = mean(lfpr, na.rm = TRUE)),
                     by = .(year, display_group, sex)]
        hist_lfpr <- get_historical_lfpr()
        if (!is.null(hist_lfpr)) {
          hist_agg <- hist_lfpr[marital_status == "all" & child_status == "all"]
          hist_agg[, display_group := map_to_5yr_group(age_group)]
          hist_agg <- hist_agg[!is.na(display_group)]
          hist_agg <- hist_agg[, .(lfpr = mean(lfpr, na.rm = TRUE)),
                               by = .(year, display_group, sex)]
          hist_agg <- hist_agg[!year %in% proj$year]
          dt <- rbindlist(list(hist_agg, proj), use.names = TRUE, fill = TRUE)
        } else {
          dt <- proj
        }
        dt <- dt[year %in% years]
        dt[, lfpr := round(lfpr * 100, 2)]
        wide <- dcast(dt, year + sex ~ display_group, value.var = "lfpr")
        datatable(wide, options = list(scrollX = TRUE, pageLength = 15)) |>
          formatRound(columns = 3:ncol(wide), digits = 2)

      } else if (chart_type %in% c("lf_growth", "lf_level")) {
        lf_data <- rv$active_data$labor_force_employment
        req(lf_data, lf_data$labor_force)
        agg <- get_lf_series(copy(lf_data$labor_force), years)
        agg[, labor_force := round(labor_force)]
        agg[, growth_rate := round(growth_rate, 3)]
        setnames(agg, c("Year", "Labor Force", "YoY Growth (%)"))
        datatable(agg, options = list(pageLength = 15)) |>
          formatRound(columns = 2, digits = 0) |>
          formatRound(columns = 3, digits = 3)

      } else if (chart_type == "emp_ts") {
        emp_data <- rv$active_data$labor_force_employment
        req(emp_data, emp_data$employment)
        proj <- copy(emp_data$employment)
        proj[, display_group := map_to_5yr_group(age_group)]
        proj <- proj[!is.na(display_group)]
        proj <- proj[, .(employment = round(sum(employment, na.rm = TRUE) / 4)),
                     by = .(year, display_group, sex)]
        hist_emp <- get_historical_emp()
        if (!is.null(hist_emp)) {
          hist_emp[, display_group := map_to_5yr_group(age_group)]
          hist_emp <- hist_emp[!is.na(display_group)]
          hist_emp <- hist_emp[, .(employment = round(sum(employment, na.rm = TRUE))),
                               by = .(year, display_group, sex)]
          hist_emp <- hist_emp[!year %in% proj$year]
          dt <- rbindlist(list(hist_emp, proj), use.names = TRUE, fill = TRUE)
        } else {
          dt <- proj
        }
        dt <- dt[year %in% years]
        wide <- dcast(dt, year + sex ~ display_group, value.var = "employment")
        datatable(wide, options = list(scrollX = TRUE, pageLength = 15)) |>
          formatRound(columns = 3:ncol(wide), digits = 0)

      } else if (chart_type == "lfpr_detail") {
        lfpr_data <- rv$active_data$lfpr_projection
        req(lfpr_data, lfpr_data$detailed)
        sel_age <- input$detail_age_group
        sel_sex <- input$detail_sex
        proj <- copy(lfpr_data$detailed)
        proj <- proj[age_group == sel_age & sex == sel_sex & marital_status != "all"]
        hist_lfpr <- get_historical_lfpr()
        if (!is.null(hist_lfpr)) {
          hist_detail <- hist_lfpr[age_group == sel_age & sex == sel_sex &
                                     marital_status != "all"]
          if (nrow(hist_detail) > 0) {
            hist_detail <- hist_detail[!year %in% proj$year]
            dt <- rbindlist(list(hist_detail, proj), use.names = TRUE, fill = TRUE)
          } else {
            dt <- proj
          }
        } else {
          dt <- proj
        }
        dt <- dt[year %in% years]
        dt[, series := fifelse(
          child_status == "all" | is.na(child_status),
          marital_status,
          paste0(marital_status, " (", gsub("_", " ", child_status), ")")
        )]
        dt[, lfpr := round(lfpr * 100, 2)]
        wide <- dcast(dt, year ~ series, value.var = "lfpr")
        datatable(wide, options = list(scrollX = TRUE, pageLength = 15)) |>
          formatRound(columns = 2:ncol(wide), digits = 2)
      }
    })
  })
}
