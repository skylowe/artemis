# US Employment Visualization Module
# =============================================================================
# Unemployment rates, LFPR, labor force, and employment projections

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
        min = MIN_YEAR, max = MAX_YEAR,
        value = c(MIN_YEAR, MAX_YEAR),
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

      # Controls for lf_growth / lf_level
      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'lf_growth' || input['%s'] == 'lf_level'",
          ns("chart_type"), ns("chart_type")
        ),
        checkboxInput(
          ns("compare_baseline"),
          "Compare to baseline",
          value = FALSE
        )
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
    # Reactive data helpers
    # =========================================================================

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

    render_ur_chart <- function() {
      ur_data <- rv$active_data$unemployment_projection
      if (is.null(ur_data) || is.null(ur_data$actual)) {
        return(plotly_empty_message("Unemployment projection data not available"))
      }

      dt <- copy(ur_data$actual)
      years <- seq(input$year_range[1], input$year_range[2])
      selected_groups <- input$age_groups

      if (is.null(selected_groups) || length(selected_groups) == 0) {
        return(plotly_empty_message("Select at least one age group"))
      }

      # Quarterly -> annual average
      dt <- dt[year %in% years & age_group %in% selected_groups,
               .(rate = mean(rate, na.rm = TRUE)),
               by = .(year, age_group, sex)]

      sex_filter <- input$sex_filter
      if (sex_filter == "both") {
        # Average across sexes (weighted equally for display)
        dt <- dt[, .(rate = mean(rate, na.rm = TRUE)), by = .(year, age_group)]

        dt[, age_group := factor(age_group, levels = USEMP_CHART_UR_GROUPS)]

        p <- ggplot(dt, aes(x = year, y = rate, color = age_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = ur_age_colors, name = "Age Group") +
          labs(x = NULL, y = "Unemployment Rate (%)") +
          theme_artemis()
      } else {
        dt <- dt[sex == sex_filter]
        dt[, age_group := factor(age_group, levels = USEMP_CHART_UR_GROUPS)]

        p <- ggplot(dt, aes(x = year, y = rate, color = age_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = ur_age_colors, name = "Age Group") +
          labs(x = NULL, y = "Unemployment Rate (%)",
               subtitle = paste0(tools::toTitleCase(sex_filter), "s")) +
          theme_artemis()
      }

      ggplotly(p) |> layout_artemis(y_title = "Unemployment Rate (%)")
    }

    # =========================================================================
    # Chart 2: LFPR by Age Group
    # =========================================================================

    render_lfpr_chart <- function() {
      lfpr_data <- rv$active_data$lfpr_projection
      if (is.null(lfpr_data) || is.null(lfpr_data$aggregate)) {
        return(plotly_empty_message("LFPR projection data not available"))
      }

      dt <- copy(lfpr_data$aggregate)
      years <- seq(input$year_range[1], input$year_range[2])
      selected_groups <- input$age_groups

      if (is.null(selected_groups) || length(selected_groups) == 0) {
        return(plotly_empty_message("Select at least one age group"))
      }

      dt <- dt[year %in% years]

      # Map single-year ages 55+ to 5-year groups
      dt[, display_group := fcase(
        age_group %in% c("16-17", "18-19", "20-24", "25-29", "30-34",
                         "35-39", "40-44", "45-49", "50-54"), age_group,
        age_group %in% as.character(55:59), "55-59",
        age_group %in% as.character(60:64), "60-64",
        age_group %in% as.character(65:69), "65-69",
        age_group %in% as.character(70:74), "70-74",
        age_group %in% c(as.character(75:100), "75+", "80+"), "75+",
        default = NA_character_
      )]
      dt <- dt[!is.na(display_group) & display_group %in% selected_groups]

      # Aggregate single-year ages within each 5-year group (weighted mean by group size)
      dt <- dt[, .(lfpr = mean(lfpr, na.rm = TRUE)),
               by = .(year, display_group, sex)]

      sex_filter <- input$sex_filter
      if (sex_filter == "both") {
        dt <- dt[, .(lfpr = mean(lfpr, na.rm = TRUE)), by = .(year, display_group)]
      } else {
        dt <- dt[sex == sex_filter]
      }

      dt[, display_group := factor(display_group, levels = USEMP_CHART_LFPR_GROUPS)]

      p <- ggplot(dt, aes(x = year, y = lfpr, color = display_group)) +
        geom_line(linewidth = 0.8) +
        scale_color_manual(values = lfpr_age_colors, name = "Age Group") +
        scale_y_continuous(labels = scales::percent) +
        labs(x = NULL, y = "LFPR") +
        theme_artemis()

      if (sex_filter != "both") {
        p <- p + labs(subtitle = paste0(tools::toTitleCase(sex_filter), "s"))
      }

      ggplotly(p) |> layout_artemis(y_title = "LFPR")
    }

    # =========================================================================
    # Chart 3a: Labor Force Growth Rate
    # =========================================================================

    # Helper to aggregate labor force data
    aggregate_lf <- function(lf_dt, years) {
      agg <- lf_dt[year %in% years,
                   .(labor_force = mean(labor_force, na.rm = TRUE)),
                   by = .(year, quarter)]
      agg <- agg[, .(labor_force = sum(labor_force, na.rm = TRUE)), by = year]
      setorder(agg, year)
      agg[, growth_rate := (labor_force / shift(labor_force) - 1) * 100]
      agg
    }

    render_lf_growth_chart <- function() {
      lf_data <- rv$active_data$labor_force_employment
      if (is.null(lf_data) || is.null(lf_data$labor_force)) {
        return(plotly_empty_message("Labor force data not available"))
      }

      years <- seq(input$year_range[1], input$year_range[2])
      agg <- aggregate_lf(copy(lf_data$labor_force), years)
      growth_dt <- agg[!is.na(growth_rate)]

      p <- ggplot(growth_dt, aes(x = year, y = growth_rate)) +
        geom_line(linewidth = 1, color = "#E74C3C") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#BDC3C7") +
        labs(x = NULL, y = "YoY Growth (%)") +
        theme_artemis()

      if (isTRUE(input$compare_baseline) && !is.null(rv$baseline$labor_force_employment)) {
        bl_agg <- aggregate_lf(copy(rv$baseline$labor_force_employment$labor_force), years)
        bl_growth <- bl_agg[!is.na(growth_rate)]
        p <- p +
          geom_line(data = bl_growth, aes(x = year, y = growth_rate),
                    linetype = "dashed", color = "#7F8C8D", linewidth = 0.8)
      }

      ggplotly(p) |> layout_artemis(y_title = "YoY Growth (%)")
    }

    # =========================================================================
    # Chart 3b: Labor Force Level
    # =========================================================================

    render_lf_level_chart <- function() {
      lf_data <- rv$active_data$labor_force_employment
      if (is.null(lf_data) || is.null(lf_data$labor_force)) {
        return(plotly_empty_message("Labor force data not available"))
      }

      years <- seq(input$year_range[1], input$year_range[2])
      agg <- aggregate_lf(copy(lf_data$labor_force), years)

      p <- ggplot(agg, aes(x = year, y = labor_force / 1e6)) +
        geom_line(linewidth = 1, color = "#3498DB") +
        labs(x = NULL, y = "Labor Force (millions)") +
        scale_y_continuous(labels = scales::comma) +
        theme_artemis()

      if (isTRUE(input$compare_baseline) && !is.null(rv$baseline$labor_force_employment)) {
        bl_agg <- aggregate_lf(copy(rv$baseline$labor_force_employment$labor_force), years)
        p <- p +
          geom_line(data = bl_agg, aes(x = year, y = labor_force / 1e6),
                    linetype = "dashed", color = "#7F8C8D", linewidth = 0.8)
      }

      ggplotly(p) |> layout_artemis(y_title = "Labor Force (millions)")
    }

    # =========================================================================
    # Chart 4: Employment by Age Group
    # =========================================================================

    render_emp_chart <- function() {
      emp_data <- rv$active_data$labor_force_employment
      if (is.null(emp_data) || is.null(emp_data$employment)) {
        return(plotly_empty_message("Employment data not available"))
      }

      dt <- copy(emp_data$employment)
      years <- seq(input$year_range[1], input$year_range[2])
      selected_groups <- input$age_groups

      if (is.null(selected_groups) || length(selected_groups) == 0) {
        return(plotly_empty_message("Select at least one age group"))
      }

      # Map single-year ages 55+ to 5-year groups for display
      dt[, display_group := fcase(
        age_group %in% USEMP_CHART_UR_GROUPS, age_group,
        age_group %in% as.character(55:59), "55-59",
        age_group %in% as.character(60:64), "60-64",
        age_group %in% as.character(65:69), "65-69",
        age_group %in% as.character(70:74), "70-74",
        age_group %in% as.character(75:100), "75+",
        default = NA_character_
      )]
      dt <- dt[!is.na(display_group)]

      # Quarterly -> annual avg, aggregate to display groups
      dt <- dt[year %in% years & display_group %in% selected_groups,
               .(employment = sum(employment, na.rm = TRUE) / 4),
               by = .(year, display_group, sex)]

      sex_filter <- input$sex_filter
      if (sex_filter == "both") {
        dt <- dt[, .(employment = sum(employment, na.rm = TRUE)), by = .(year, display_group)]

        dt[, display_group := factor(display_group, levels = USEMP_CHART_UR_GROUPS)]

        p <- ggplot(dt, aes(x = year, y = employment / 1e3, color = display_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = ur_age_colors, name = "Age Group") +
          scale_y_continuous(labels = scales::comma) +
          labs(x = NULL, y = "Employment (thousands)") +
          theme_artemis()
      } else {
        dt <- dt[sex == sex_filter]
        dt[, display_group := factor(display_group, levels = USEMP_CHART_UR_GROUPS)]

        p <- ggplot(dt, aes(x = year, y = employment / 1e3, color = display_group)) +
          geom_line(linewidth = 0.8) +
          scale_color_manual(values = ur_age_colors, name = "Age Group") +
          scale_y_continuous(labels = scales::comma) +
          labs(x = NULL, y = "Employment (thousands)",
               subtitle = paste0(tools::toTitleCase(sex_filter), "s")) +
          theme_artemis()
      }

      ggplotly(p) |> layout_artemis(y_title = "Employment (thousands)")
    }

    # =========================================================================
    # Chart 5: Detailed LFPR (Marital/Children)
    # =========================================================================

    render_lfpr_detail_chart <- function() {
      lfpr_data <- rv$active_data$lfpr_projection
      if (is.null(lfpr_data) || is.null(lfpr_data$detailed)) {
        return(plotly_empty_message("Detailed LFPR data not available"))
      }

      dt <- copy(lfpr_data$detailed)
      years <- seq(input$year_range[1], input$year_range[2])
      sel_age <- input$detail_age_group
      sel_sex <- input$detail_sex

      dt <- dt[year %in% years & age_group == sel_age & sex == sel_sex]

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
        dt <- copy(ur_data$actual)
        dt <- dt[year %in% years,
                 .(rate = round(mean(rate, na.rm = TRUE), 2)),
                 by = .(year, age_group, sex)]
        wide <- dcast(dt, year + sex ~ age_group, value.var = "rate")
        datatable(wide, options = list(scrollX = TRUE, pageLength = 15)) |>
          formatRound(columns = 3:ncol(wide), digits = 2)

      } else if (chart_type == "lfpr_ts") {
        lfpr_data <- rv$active_data$lfpr_projection
        req(lfpr_data, lfpr_data$aggregate)
        dt <- copy(lfpr_data$aggregate)
        dt <- dt[year %in% years]
        # Map single-year ages 55+ to 5-year groups
        dt[, display_group := fcase(
          age_group %in% c("16-17", "18-19", "20-24", "25-29", "30-34",
                           "35-39", "40-44", "45-49", "50-54"), age_group,
          age_group %in% as.character(55:59), "55-59",
          age_group %in% as.character(60:64), "60-64",
          age_group %in% as.character(65:69), "65-69",
          age_group %in% as.character(70:74), "70-74",
          age_group %in% c(as.character(75:100), "75+", "80+"), "75+",
          default = NA_character_
        )]
        dt <- dt[!is.na(display_group)]
        dt <- dt[, .(lfpr = round(mean(lfpr, na.rm = TRUE) * 100, 2)),
                 by = .(year, display_group, sex)]
        wide <- dcast(dt, year + sex ~ display_group, value.var = "lfpr")
        datatable(wide, options = list(scrollX = TRUE, pageLength = 15)) |>
          formatRound(columns = 3:ncol(wide), digits = 2)

      } else if (chart_type %in% c("lf_growth", "lf_level")) {
        lf_data <- rv$active_data$labor_force_employment
        req(lf_data, lf_data$labor_force)
        dt <- copy(lf_data$labor_force)
        agg <- dt[year %in% years,
                  .(labor_force = mean(labor_force, na.rm = TRUE)),
                  by = .(year, quarter)]
        agg <- agg[, .(labor_force = round(sum(labor_force))), by = year]
        setorder(agg, year)
        agg[, growth_pct := round((labor_force / shift(labor_force) - 1) * 100, 3)]
        setnames(agg, c("Year", "Labor Force", "YoY Growth (%)"))
        datatable(agg, options = list(pageLength = 15)) |>
          formatRound(columns = 2, digits = 0) |>
          formatRound(columns = 3, digits = 3)

      } else if (chart_type == "emp_ts") {
        emp_data <- rv$active_data$labor_force_employment
        req(emp_data, emp_data$employment)
        dt <- copy(emp_data$employment)
        # Map to 5-year display groups
        dt[, display_group := fcase(
          age_group %in% USEMP_CHART_UR_GROUPS, age_group,
          age_group %in% as.character(55:59), "55-59",
          age_group %in% as.character(60:64), "60-64",
          age_group %in% as.character(65:69), "65-69",
          age_group %in% as.character(70:74), "70-74",
          age_group %in% as.character(75:100), "75+",
          default = NA_character_
        )]
        dt <- dt[!is.na(display_group) & year %in% years]
        dt <- dt[, .(employment = round(sum(employment, na.rm = TRUE) / 4)),
                 by = .(year, display_group, sex)]
        wide <- dcast(dt, year + sex ~ display_group, value.var = "employment")
        datatable(wide, options = list(scrollX = TRUE, pageLength = 15)) |>
          formatRound(columns = 3:ncol(wide), digits = 0)

      } else if (chart_type == "lfpr_detail") {
        lfpr_data <- rv$active_data$lfpr_projection
        req(lfpr_data, lfpr_data$detailed)
        dt <- copy(lfpr_data$detailed)
        sel_age <- input$detail_age_group
        sel_sex <- input$detail_sex
        dt <- dt[year %in% years & age_group == sel_age & sex == sel_sex &
                   marital_status != "all"]
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
