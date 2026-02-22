# Fertility Visualization Module
# =============================================================================
# Age-specific fertility rates and TFR trends

#' Fertility Visualization UI
#' @param id Module namespace ID
mod_fertility_viz_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 250,

      selectInput(
        ns("chart_type"),
        "Chart Type",
        choices = c(
          "TFR Time Series" = "tfr_ts",
          "Age-Specific Rates" = "asfr",
          "Rate Surface" = "surface",
          "Cohort TFR" = "cohort"
        ),
        selected = "tfr_ts"
      ),

      hr(),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'asfr'", ns("chart_type")),
        sliderInput(
          ns("asfr_year_range"),
          "Year Range",
          min = MIN_YEAR, max = MAX_YEAR,
          value = c(MIN_YEAR + 1, MAX_YEAR),
          step = 1,
          sep = ""
        ),
        helpText("Up to 5 evenly-spaced years will be shown")
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'tfr_ts'", ns("chart_type")),
        sliderInput(
          ns("tfr_year_range"),
          "Year Range",
          min = MIN_YEAR, max = MAX_YEAR,
          value = c(MIN_YEAR, MAX_YEAR),
          step = 1,
          sep = ""
        ),
        checkboxInput(
          ns("show_ultimate"),
          "Show ultimate TFR line",
          value = TRUE
        )
      ),

      hr(),

      # Comparison
      checkboxInput(
        ns("compare_baseline"),
        "Compare to baseline",
        value = FALSE
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

#' Fertility Visualization Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
mod_fertility_viz_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Dynamically update year sliders when projection data changes
    observe({
      data <- rv$active_data$fertility_rates_complete
      req(data)
      max_year <- max(data$year, na.rm = TRUE)
      for (sid in c("asfr_year_range", "tfr_year_range")) {
        current <- isolate(input[[sid]])
        if (!is.null(current) && max_year != current[2]) {
          updateSliderInput(session, sid,
            max = max_year,
            value = c(current[1], max_year))
        }
      }
    })

    # Main chart
    output$main_chart <- renderPlotly({
      req(rv$active_data$fertility_rates_complete)

      rates <- copy(rv$active_data$fertility_rates_complete)
      # Normalize column name (data has birth_rate, we use rate)
      if ("birth_rate" %in% names(rates) && !"rate" %in% names(rates)) {
        setnames(rates, "birth_rate", "rate")
      }
      chart_type <- input$chart_type

      if (chart_type == "tfr_ts") {
        # TFR time series
        years <- seq(input$tfr_year_range[1], input$tfr_year_range[2])
        tfr <- rates[year %in% years, .(tfr = sum(rate, na.rm = TRUE)), by = year]

        p <- ggplot(tfr, aes(x = year, y = tfr)) +
          geom_line(linewidth = 1.2, color = "#E74C3C") +
          labs(x = NULL, y = "Total Fertility Rate") +
          theme_artemis()

        if (input$show_ultimate && !is.null(rv$config$fertility$ultimate_ctfr)) {
          p <- p +
            geom_hline(
              yintercept = rv$config$fertility$ultimate_ctfr,
              linetype = "dashed",
              color = "#7F8C8D"
            ) +
            annotate(
              "text",
              x = max(years),
              y = rv$config$fertility$ultimate_ctfr,
              label = paste("Ultimate:", rv$config$fertility$ultimate_ctfr),
              hjust = 1,
              vjust = -0.5,
              size = 3.5,
              color = "#7F8C8D"
            )
        }

        if (input$compare_baseline && !is.null(rv$baseline$fertility_rates_complete)) {
          baseline_rates <- copy(rv$baseline$fertility_rates_complete)
          if ("birth_rate" %in% names(baseline_rates) && !"rate" %in% names(baseline_rates)) {
            setnames(baseline_rates, "birth_rate", "rate")
          }
          baseline_tfr <- baseline_rates[
            year %in% years,
            .(tfr = sum(rate, na.rm = TRUE)),
            by = year
          ]
          p <- p +
            geom_line(
              data = baseline_tfr,
              aes(x = year, y = tfr),
              linetype = "dashed",
              color = "#3498DB",
              linewidth = 0.8
            )
        }

        ggplotly(p) |> layout_artemis(y_title = "TFR")

      } else if (chart_type == "asfr") {
        # Age-specific fertility rates
        years <- unique(rates$year)
        selected_years <- years[years >= input$asfr_year_range[1] &
                                 years <= input$asfr_year_range[2]]

        # Take evenly spaced years
        if (length(selected_years) > 5) {
          idx <- round(seq(1, length(selected_years), length.out = 5))
          selected_years <- selected_years[idx]
        }

        asfr <- rates[year %in% selected_years]
        asfr[, year_factor := factor(year)]

        p <- ggplot(asfr, aes(x = age, y = rate * 1000, color = year_factor)) +
          geom_line(linewidth = 1) +
          labs(x = "Age", y = "Births per 1,000 women", color = "Year") +
          scale_color_viridis_d() +
          theme_artemis()

        ggplotly(p) |> layout_artemis()

      } else if (chart_type == "surface") {
        # 3D surface of rates over time and age
        years <- unique(rates$year)
        ages <- unique(rates$age)

        # Create matrix
        rate_matrix <- dcast(rates, age ~ year, value.var = "rate")
        z <- as.matrix(rate_matrix[, -1])
        rownames(z) <- rate_matrix$age

        plot_ly(
          x = as.numeric(colnames(z)),
          y = as.numeric(rownames(z)),
          z = z * 1000,
          type = "surface",
          colorscale = "Viridis"
        ) |>
          layout(
            scene = list(
              xaxis = list(title = "Year"),
              yaxis = list(title = "Age"),
              zaxis = list(title = "Births/1000")
            )
          )

      } else if (chart_type == "cohort") {
        # Cohort TFR (cumulative fertility by birth cohort)
        rates[, cohort := year - age]

        cohort_tfr <- rates[, .(
          cumulative_tfr = sum(rate, na.rm = TRUE),
          n_ages = .N
        ), by = cohort]

        # Only show cohorts with complete fertility history or recent ones
        cohort_tfr <- cohort_tfr[n_ages >= 30 | cohort >= 1980]

        p <- ggplot(cohort_tfr, aes(x = cohort, y = cumulative_tfr)) +
          geom_line(linewidth = 1, color = "#9B59B6") +
          geom_point(size = 1.5, color = "#9B59B6") +
          labs(x = "Birth Cohort", y = "Cohort TFR") +
          theme_artemis()

        ggplotly(p) |> layout_artemis()
      }
    })

    # Data table
    output$data_table <- renderDT({
      req(rv$active_data$fertility_rates_complete)

      rates <- copy(rv$active_data$fertility_rates_complete)
      if ("birth_rate" %in% names(rates) && !"rate" %in% names(rates)) {
        setnames(rates, "birth_rate", "rate")
      }

      if (input$chart_type == "tfr_ts") {
        tfr <- rates[, .(TFR = round(sum(rate, na.rm = TRUE), 3)), by = year]
        datatable(tfr, options = list(pageLength = 15))
      } else {
        # Wide format: ages as columns
        wide <- dcast(rates, year ~ age, value.var = "rate")
        datatable(
          wide,
          options = list(scrollX = TRUE, pageLength = 10)
        ) |> formatRound(columns = 2:ncol(wide), digits = 4)
      }
    })
  })
}
