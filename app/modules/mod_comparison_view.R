# Comparison View Module
# =============================================================================
# Compare multiple scenarios side by side

#' Comparison View UI
#' @param id Module namespace ID
mod_comparison_view_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 250,

      h5("Select Scenarios"),
      checkboxGroupInput(
        ns("scenarios"),
        NULL,
        choices = c("TR2025 Baseline" = "baseline"),
        selected = "baseline"
      ),

      hr(),

      selectInput(
        ns("metric"),
        "Metric to Compare",
        choices = c(
          "Total Population" = "population",
          "Annual Births" = "births",
          "Annual Deaths" = "deaths",
          "Net Immigration" = "immigration",
          "Life Expectancy (e0)" = "life_exp",
          "TFR" = "tfr",
          "Dependency Ratio" = "dependency"
        ),
        selected = "population"
      ),

      hr(),

      radioButtons(
        ns("comparison_type"),
        "Display As",
        choices = c(
          "Absolute Values" = "absolute",
          "Difference from Baseline" = "diff",
          "Percent Difference" = "pct_diff"
        ),
        selected = "absolute"
      ),

      hr(),

      sliderInput(
        ns("year_range"),
        "Year Range",
        min = 2022, max = 2099,
        value = c(2022, 2099),
        step = 1,
        sep = ""
      ),

      hr(),

      actionButton(
        ns("export_comparison"),
        "Export Comparison",
        icon = icon("download"),
        class = "btn-outline-primary w-100"
      )
    ),

    # Main content
    layout_column_wrap(
      width = 1,

      # Time series comparison
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Scenario Comparison",
          span(
            class = "badge bg-info",
            textOutput(ns("n_scenarios"), inline = TRUE),
            " scenarios"
          )
        ),
        card_body(
          plotlyOutput(ns("comparison_chart"), height = "500px")
        )
      ),

      # Comparison table
      card(
        card_header("Summary Comparison"),
        card_body(
          DTOutput(ns("comparison_table"))
        )
      ),

      # Population pyramids side by side
      conditionalPanel(
        condition = sprintf("input['%s'] == 'population'", ns("metric")),
        card(
          card_header("Population Pyramids"),
          card_body(
            sliderInput(
              ns("pyramid_year"),
              "Year",
              min = 2022, max = 2099,
              value = 2050,
              step = 1,
              sep = "",
              width = "100%"
            ),
            plotlyOutput(ns("pyramid_comparison"), height = "500px")
          )
        )
      )
    )
  )
}

#' Comparison View Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
mod_comparison_view_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Update scenario choices when scenarios change
    observe({
      choices <- c("TR2025 Baseline" = "baseline")

      if (length(rv$scenarios) > 0) {
        scenario_choices <- setNames(names(rv$scenarios), names(rv$scenarios))
        choices <- c(choices, scenario_choices)
      }

      updateCheckboxGroupInput(
        session, "scenarios",
        choices = choices,
        selected = input$scenarios
      )
    })

    # Number of selected scenarios
    output$n_scenarios <- renderText({
      length(input$scenarios)
    })

    # Get data for comparison
    comparison_data <- reactive({
      req(input$scenarios)
      req(length(input$scenarios) >= 1)

      years <- seq(input$year_range[1], input$year_range[2])
      metric <- input$metric

      results <- list()

      for (scenario_id in input$scenarios) {
        # Get scenario data
        if (scenario_id == "baseline") {
          data <- rv$baseline
          name <- "TR2025 Baseline"
        } else if (scenario_id %in% names(rv$scenarios)) {
          data <- rv$scenarios[[scenario_id]]$results
          name <- scenario_id
        } else {
          next
        }

        if (is.null(data)) next

        # Extract metric
        metric_data <- switch(metric,
          "population" = {
            if (!is.null(data$projected_population)) {
              data$projected_population[year %in% years,
                .(value = sum(population, na.rm = TRUE)), by = year]
            }
          },
          "births" = {
            if (!is.null(data$projected_births)) {
              data$projected_births[year %in% years,
                .(value = sum(births, na.rm = TRUE)), by = year]
            }
          },
          "deaths" = {
            if (!is.null(data$projected_deaths)) {
              data$projected_deaths[year %in% years,
                .(value = sum(deaths, na.rm = TRUE)), by = year]
            }
          },
          "immigration" = {
            if (!is.null(data$projected_net_immigration)) {
              data$projected_net_immigration[year %in% years,
                .(value = sum(net_immigration, na.rm = TRUE)), by = year]
            }
          },
          "tfr" = {
            if (!is.null(data$fertility_rates_complete)) {
              fert <- copy(data$fertility_rates_complete)
              rate_col <- if ("birth_rate" %in% names(fert)) "birth_rate" else "rate"
              fert[year %in% years, .(value = sum(get(rate_col), na.rm = TRUE)), by = year]
            }
          },
          "dependency" = {
            if (!is.null(data$projected_population)) {
              pop <- data$projected_population[year %in% years]
              pop[, .(
                value = sum(population[age < 18 | age >= 67], na.rm = TRUE) /
                        sum(population[age >= 18 & age < 67], na.rm = TRUE) * 100
              ), by = year]
            }
          },
          NULL
        )

        if (!is.null(metric_data) && nrow(metric_data) > 0) {
          metric_data[, scenario := name]
          results[[scenario_id]] <- metric_data
        }
      }

      if (length(results) > 0) {
        rbindlist(results, use.names = TRUE, fill = TRUE)
      } else {
        NULL
      }
    })

    # Main comparison chart
    output$comparison_chart <- renderPlotly({
      data <- comparison_data()

      if (is.null(data) || nrow(data) == 0) {
        return(plotly_empty_message("Select scenarios to compare"))
      }

      # Apply comparison type transformation
      if (input$comparison_type != "absolute" && "TR2025 Baseline" %in% unique(data$scenario)) {
        baseline_vals <- data[scenario == "TR2025 Baseline", .(year, baseline = value)]
        data <- merge(data, baseline_vals, by = "year", all.x = TRUE)

        if (input$comparison_type == "diff") {
          data[, value := value - baseline]
        } else if (input$comparison_type == "pct_diff") {
          data[, value := (value - baseline) / baseline * 100]
        }
      }

      # Set y-axis label
      y_label <- switch(input$metric,
        "population" = "Population",
        "births" = "Births",
        "deaths" = "Deaths",
        "immigration" = "Net Immigration",
        "tfr" = "TFR",
        "dependency" = "Dependency Ratio (%)",
        "Value"
      )

      if (input$comparison_type == "diff") {
        y_label <- paste("Difference in", y_label)
      } else if (input$comparison_type == "pct_diff") {
        y_label <- paste("% Difference in", y_label)
      }

      # Scale values for display
      scale_factor <- 1
      if (input$metric %in% c("population", "births", "deaths", "immigration") &&
          input$comparison_type == "absolute") {
        scale_factor <- 1e6
        y_label <- paste(y_label, "(millions)")
      }

      p <- ggplot(data, aes(x = year, y = value / scale_factor, color = scenario)) +
        geom_line(linewidth = 1) +
        labs(x = NULL, y = y_label, color = "Scenario") +
        scale_color_manual(values = artemis_colors$scenarios[1:length(unique(data$scenario))]) +
        theme_artemis()

      if (input$comparison_type != "absolute") {
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "#7F8C8D")
      }

      ggplotly(p) |>
        layout_artemis() |>
        layout(legend = list(orientation = "h", y = -0.15))
    })

    # Comparison table
    output$comparison_table <- renderDT({
      data <- comparison_data()

      if (is.null(data) || nrow(data) == 0) {
        return(datatable(data.frame(Message = "No data to display")))
      }

      # Calculate summary by scenario
      summary_dt <- data[, .(
        `Start Value` = value[which.min(year)],
        `End Value` = value[which.max(year)],
        `Change` = value[which.max(year)] - value[which.min(year)],
        `% Change` = (value[which.max(year)] - value[which.min(year)]) /
                     value[which.min(year)] * 100,
        `Mean` = mean(value, na.rm = TRUE),
        `Min` = min(value, na.rm = TRUE),
        `Max` = max(value, na.rm = TRUE)
      ), by = scenario]

      datatable(
        summary_dt,
        options = list(
          pageLength = 10,
          dom = "t"
        ),
        rownames = FALSE
      ) |>
        formatRound(columns = 2:7, digits = 0) |>
        formatRound(columns = 5, digits = 1)
    })

    # Population pyramid comparison
    output$pyramid_comparison <- renderPlotly({
      req(input$scenarios)
      req(input$metric == "population")

      selected_year <- input$pyramid_year
      pyramids <- list()

      for (scenario_id in input$scenarios) {
        if (scenario_id == "baseline") {
          data <- rv$baseline$projected_population
          name <- "Baseline"
        } else if (scenario_id %in% names(rv$scenarios)) {
          data <- rv$scenarios[[scenario_id]]$results$projected_population
          name <- scenario_id
        } else {
          next
        }

        if (is.null(data)) next

        pop <- data[year == selected_year]
        if (nrow(pop) == 0) next

        # Aggregate by 5-year age groups
        pop[, age_group := cut(
          age,
          breaks = c(seq(0, 100, 5), Inf),
          labels = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+"),
          right = FALSE
        )]

        pop_agg <- pop[, .(population = sum(population, na.rm = TRUE)),
                       by = .(age_group, sex)]
        pop_agg[sex == "male", population := -population]
        pop_agg[, scenario := name]

        pyramids[[scenario_id]] <- pop_agg
      }

      if (length(pyramids) == 0) {
        return(plotly_empty_message("No population data available"))
      }

      pyramid_data <- rbindlist(pyramids, use.names = TRUE)

      # Create faceted pyramid
      p <- ggplot(pyramid_data, aes(x = age_group, y = population / 1e6, fill = sex)) +
        geom_col() +
        coord_flip() +
        facet_wrap(~scenario, nrow = 1) +
        scale_fill_manual(values = SEX_COLORS) +
        labs(x = "Age", y = "Population (millions)") +
        scale_y_continuous(labels = function(x) abs(x)) +
        theme_artemis() +
        theme(
          strip.text = element_text(size = 10),
          axis.text.x = element_text(size = 8)
        )

      ggplotly(p) |>
        layout(legend = list(orientation = "h", y = -0.1))
    })

    # Export comparison
    observeEvent(input$export_comparison, {
      data <- comparison_data()

      if (is.null(data) || nrow(data) == 0) {
        showNotification("No data to export", type = "warning")
        return()
      }

      # Create download modal
      showModal(modalDialog(
        title = "Export Comparison",
        selectInput(
          session$ns("export_format"),
          "Format",
          choices = c("CSV" = "csv", "Excel" = "xlsx", "RDS" = "rds"),
          selected = "csv"
        ),
        footer = tagList(
          downloadButton(session$ns("do_export"), "Download", class = "btn-primary"),
          modalButton("Cancel")
        )
      ))
    })

    output$do_export <- downloadHandler(
      filename = function() {
        ext <- input$export_format
        paste0("scenario_comparison_", format(Sys.Date(), "%Y%m%d"), ".", ext)
      },
      content = function(file) {
        data <- comparison_data()

        if (input$export_format == "csv") {
          fwrite(data, file)
        } else if (input$export_format == "xlsx") {
          if (requireNamespace("openxlsx", quietly = TRUE)) {
            openxlsx::write.xlsx(data, file)
          } else {
            fwrite(data, file)
          }
        } else {
          saveRDS(data, file)
        }

        removeModal()
      }
    )
  })
}
