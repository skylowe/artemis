# Marriage/Divorce Visualization Module
# =============================================================================
# Marriage and divorce rate grids and trends

#' Marriage/Divorce Visualization UI
#' @param id Module namespace ID
mod_marriage_divorce_viz_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 250,

      radioButtons(
        ns("event_type"),
        "Event Type",
        choices = c("Marriage" = "marriage", "Divorce" = "divorce"),
        selected = "marriage"
      ),

      hr(),

      selectInput(
        ns("chart_type"),
        "Chart Type",
        choices = c(
          "Rate Trend (AMR/ADR)" = "rate_trend",
          "Rate Grid Heatmap" = "heatmap",
          "Age Patterns" = "age_pattern",
          "Marital Status Flow" = "flow"
        ),
        selected = "rate_trend"
      ),

      hr(),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'heatmap'", ns("chart_type")),
        sliderInput(
          ns("heatmap_year"),
          "Year",
          min = 2023, max = 2099,
          value = 2050,
          step = 1,
          sep = "",
          animate = TRUE
        ),
        sliderInput(
          ns("age_range"),
          "Age Range",
          min = 14, max = 100,
          value = c(20, 60),
          step = 1
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rate_trend'", ns("chart_type")),
        sliderInput(
          ns("year_range"),
          "Year Range",
          min = 2022, max = 2099,
          value = c(2022, 2099),
          step = 1,
          sep = ""
        ),
        checkboxInput(
          ns("show_ultimate"),
          "Show ultimate rate line",
          value = TRUE
        )
      )
    ),

    # Main content
    layout_column_wrap(
      width = 1,

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          textOutput(ns("chart_title")),
          downloadButton(ns("download"), "Download", class = "btn-sm btn-outline-secondary")
        ),
        card_body(
          plotlyOutput(ns("main_chart"), height = "550px")
        )
      ),

      # Summary stats
      layout_column_wrap(
        width = 1/2,

        card(
          card_header("Key Statistics"),
          card_body(
            tableOutput(ns("summary_stats"))
          )
        ),

        card(
          card_header("Configuration"),
          card_body(
            uiOutput(ns("config_display"))
          )
        )
      )
    )
  )
}

#' Marriage/Divorce Visualization Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
mod_marriage_divorce_viz_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Get data based on event type
    event_data <- reactive({
      if (input$event_type == "marriage") {
        rv$active_data$marriage_amr_projected
      } else {
        rv$active_data$divorce_adr_projected
      }
    })

    # Chart title
    output$chart_title <- renderText({
      type <- if (input$event_type == "marriage") "Marriage" else "Divorce"
      chart <- switch(input$chart_type,
        "rate_trend" = "Rate Trend",
        "heatmap" = "Rate Grid",
        "age_pattern" = "Age Patterns",
        "flow" = "Status Flow"
      )
      paste(type, chart)
    })

    # Main chart
    output$main_chart <- renderPlotly({
      data <- event_data()
      req(data)

      chart_type <- input$chart_type
      event_type <- input$event_type

      if (chart_type == "rate_trend") {
        years <- seq(input$year_range[1], input$year_range[2])

        # Get rate column name based on data structure
        # Handle both "amr"/"adr" and "projected_amr"/"projected_adr" column names
        if ("projected_amr" %in% names(data) || "projected_adr" %in% names(data)) {
          rate_col <- if (event_type == "marriage") "projected_amr" else "projected_adr"
          rate_dt <- data[year %in% years, .(year, rate = get(rate_col))]
          rate_dt <- unique(rate_dt[!is.na(rate)])
        } else if ("amr" %in% names(data) || "adr" %in% names(data)) {
          rate_col <- if (event_type == "marriage") "amr" else "adr"
          rate_dt <- data[year %in% years, .(year, rate = get(rate_col))]
          rate_dt <- unique(rate_dt[!is.na(rate)])
        } else if ("rate" %in% names(data)) {
          # Aggregate if grid data
          rate_dt <- data[year %in% years,
                          .(rate = sum(rate, na.rm = TRUE)),
                          by = year]
        } else {
          return(plotly_empty_message("Rate data not available"))
        }

        rate_label <- if (event_type == "marriage") "AMR" else "ADR"

        p <- ggplot(rate_dt, aes(x = year, y = rate)) +
          geom_line(linewidth = 1.2, color = if (event_type == "marriage") "#27AE60" else "#E67E22") +
          labs(x = NULL, y = paste(rate_label, "(per 100,000)")) +
          theme_artemis()

        # Add ultimate rate line
        if (input$show_ultimate) {
          ultimate_rate <- if (event_type == "marriage") {
            rv$config$marriage$ultimate_amr
          } else {
            rv$config$divorce$ultimate_adr
          }

          if (!is.null(ultimate_rate)) {
            p <- p +
              geom_hline(yintercept = ultimate_rate, linetype = "dashed", color = "#7F8C8D") +
              annotate("text", x = max(years), y = ultimate_rate,
                       label = paste("Ultimate:", ultimate_rate),
                       hjust = 1, vjust = -0.5, size = 3.5, color = "#7F8C8D")
          }
        }

        ggplotly(p) |> layout_artemis()

      } else if (chart_type == "heatmap") {
        selected_year <- input$heatmap_year
        age_min <- input$age_range[1]
        age_max <- input$age_range[2]

        # Check for grid structure
        if (!all(c("husband_age", "wife_age") %in% names(data))) {
          return(plotly_empty_message("Grid data not available"))
        }

        grid <- data[year == selected_year &
                     husband_age >= age_min & husband_age <= age_max &
                     wife_age >= age_min & wife_age <= age_max]

        if (nrow(grid) == 0) {
          return(plotly_empty_message("No data for selected year"))
        }

        p <- ggplot(grid, aes(x = wife_age, y = husband_age, fill = rate)) +
          geom_tile() +
          scale_fill_viridis_c(
            option = if (event_type == "marriage") "viridis" else "magma",
            name = "Rate"
          ) +
          labs(x = "Wife Age", y = "Husband Age") +
          coord_fixed() +
          theme_artemis()

        ggplotly(p) |> layout_artemis()

      } else if (chart_type == "age_pattern") {
        # Average rates by husband or wife age
        if (!all(c("husband_age", "wife_age") %in% names(data))) {
          return(plotly_empty_message("Age pattern data not available"))
        }

        # Get latest year
        latest_year <- max(data$year, na.rm = TRUE)

        husband_rates <- data[year == latest_year,
                              .(rate = mean(rate, na.rm = TRUE)),
                              by = .(age = husband_age)]
        husband_rates[, type := "Husband Age"]

        wife_rates <- data[year == latest_year,
                           .(rate = mean(rate, na.rm = TRUE)),
                           by = .(age = wife_age)]
        wife_rates[, type := "Wife Age"]

        age_pattern <- rbindlist(list(husband_rates, wife_rates))

        p <- ggplot(age_pattern, aes(x = age, y = rate, color = type)) +
          geom_line(linewidth = 1) +
          scale_color_manual(values = c("Husband Age" = "#3498DB", "Wife Age" = "#E74C3C")) +
          labs(x = "Age", y = paste(if (event_type == "marriage") "Marriage" else "Divorce", "Rate"),
               color = NULL) +
          theme_artemis()

        ggplotly(p) |> layout_artemis()

      } else if (chart_type == "flow") {
        # Marital status transitions
        req(rv$active_data$projected_marital_population)

        marital <- rv$active_data$projected_marital_population
        years <- seq(input$year_range[1], input$year_range[2])

        marital_totals <- marital[year %in% years,
                                  .(population = sum(population, na.rm = TRUE)),
                                  by = .(year, marital_status)]

        p <- ggplot(marital_totals, aes(x = year, y = population / 1e6, fill = marital_status)) +
          geom_area(alpha = 0.8) +
          scale_fill_manual(values = MARITAL_COLORS) +
          labs(x = NULL, y = "Population (millions)", fill = "Status") +
          theme_artemis()

        ggplotly(p) |> layout_artemis()
      }
    })

    # Summary statistics
    output$summary_stats <- renderTable({
      data <- event_data()
      req(data)

      # Determine rate column name
      rate_col <- if ("projected_amr" %in% names(data) || "projected_adr" %in% names(data)) {
        if (input$event_type == "marriage") "projected_amr" else "projected_adr"
      } else if ("amr" %in% names(data) || "adr" %in% names(data)) {
        if (input$event_type == "marriage") "amr" else "adr"
      } else "rate"

      # Calculate summary by period
      data[, period := cut(
        year,
        breaks = c(2022, 2030, 2050, 2075, 2100),
        labels = c("2023-2030", "2031-2050", "2051-2075", "2076-2099"),
        right = TRUE
      )]

      if (rate_col %in% names(data)) {
        summary_dt <- data[, .(
          Mean = mean(get(rate_col), na.rm = TRUE),
          Min = min(get(rate_col), na.rm = TRUE),
          Max = max(get(rate_col), na.rm = TRUE)
        ), by = period]
        summary_dt <- summary_dt[!is.na(period)]
        summary_dt
      } else {
        data.frame(Period = "Data not available", Mean = NA, Min = NA, Max = NA)
      }
    }, digits = 0)

    # Configuration display
    output$config_display <- renderUI({
      config <- if (input$event_type == "marriage") {
        rv$config$marriage
      } else {
        rv$config$divorce
      }

      if (is.null(config)) {
        return(p("Configuration not loaded"))
      }

      ultimate_rate <- if (input$event_type == "marriage") {
        config$ultimate_amr
      } else {
        config$ultimate_adr
      }

      ultimate_year <- config$ultimate_year

      tags$dl(
        tags$dt("Ultimate Rate"),
        tags$dd(paste(ultimate_rate, "per 100,000")),
        tags$dt("Ultimate Year"),
        tags$dd(ultimate_year),
        tags$dt("Age Range"),
        tags$dd(paste(config$min_age, "-", config$max_age))
      )
    })

    # Download handler
    output$download <- downloadHandler(
      filename = function() {
        paste0(input$event_type, "_", input$chart_type, ".png")
      },
      content = function(file) {
        # Recreate plot for download (simplified version)
        data <- event_data()
        if (is.null(data)) return()

        if ("projected_amr" %in% names(data) || "projected_adr" %in% names(data)) {
          rate_col <- if (input$event_type == "marriage") "projected_amr" else "projected_adr"
          rate_dt <- unique(data[, .(year, rate = get(rate_col))])
          rate_dt <- rate_dt[!is.na(rate)]
        } else if ("amr" %in% names(data) || "adr" %in% names(data)) {
          rate_col <- if (input$event_type == "marriage") "amr" else "adr"
          rate_dt <- unique(data[, .(year, rate = get(rate_col))])
          rate_dt <- rate_dt[!is.na(rate)]
        } else {
          rate_dt <- data.frame(year = integer(), rate = numeric())
        }

        if (nrow(rate_dt) > 0) {
          p <- ggplot(rate_dt, aes(x = year, y = rate)) +
            geom_line(linewidth = 1.2) +
            labs(x = "Year", y = "Rate per 100,000",
                 title = paste(tools::toTitleCase(input$event_type), "Rate Trend")) +
            theme_artemis()
          ggsave(file, p, width = 10, height = 6, dpi = 150)
        }
      }
    )
  })
}
