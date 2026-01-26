# Time Series Visualization Module
# =============================================================================
# Population and demographic time series

#' Time Series Visualization UI
#' @param id Module namespace ID
#' @param show_marital Show marital status options
mod_timeseries_viz_ui <- function(id, show_marital = FALSE) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 250,

      # Metric selector
      selectInput(
        ns("metric"),
        "Metric",
        choices = if (show_marital) {
          c(
            "Population by Marital Status" = "marital_pop",
            "Marriage Rate (AMR)" = "amr",
            "Divorce Rate (ADR)" = "adr",
            "% Married" = "pct_married"
          )
        } else {
          c(
            "Total Population" = "population",
            "Births" = "births",
            "Deaths" = "deaths",
            "Net Immigration" = "immigration",
            "Natural Increase" = "natural_increase",
            "Dependency Ratio" = "dependency"
          )
        },
        selected = if (show_marital) "marital_pop" else "population"
      ),

      hr(),

      # Year range
      sliderInput(
        ns("year_range"),
        "Year Range",
        min = 2022, max = 2099,
        value = c(2022, 2099),
        step = 1,
        sep = ""
      ),

      hr(),

      # Breakdown options
      radioButtons(
        ns("breakdown"),
        "Breakdown By",
        choices = c(
          "Total" = "total",
          "By Sex" = "sex",
          "By Age Group" = "age"
        ),
        selected = "total"
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'age'", ns("breakdown")),
        checkboxGroupInput(
          ns("age_groups"),
          "Age Groups",
          choices = c(
            "0-17" = "0_17",
            "18-24" = "18_24",
            "25-44" = "25_44",
            "45-64" = "45_64",
            "65-84" = "65_84",
            "85+" = "85_plus"
          ),
          selected = c("0_17", "18_24", "25_44", "45_64", "65_84", "85_plus")
        )
      ),

      hr(),

      # Display options
      checkboxInput(
        ns("log_scale"),
        "Log scale",
        value = FALSE
      ),

      checkboxInput(
        ns("show_historical"),
        "Include historical",
        value = TRUE
      )
    ),

    # Main content
    layout_column_wrap(
      width = 1,

      # Main chart
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          textOutput(ns("chart_title")),
          downloadButton(ns("download_chart"), "Download", class = "btn-sm btn-outline-secondary")
        ),
        card_body(
          plotlyOutput(ns("timeseries"), height = "550px")
        )
      ),

      # Summary table
      card(
        card_header("Summary by Period"),
        card_body(
          tableOutput(ns("period_summary"))
        )
      )
    )
  )
}

#' Time Series Visualization Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
#' @param show_marital Show marital status data
mod_timeseries_viz_server <- function(id, rv, show_marital = FALSE) {
  moduleServer(id, function(input, output, session) {

    # Prepare data based on metric selection
    chart_data <- reactive({
      req(rv$active_data)

      metric <- input$metric
      years <- seq(input$year_range[1], input$year_range[2])

      data <- NULL

      if (metric == "population") {
        req(rv$active_data$projected_population)
        pop <- rv$active_data$projected_population[year %in% years]

        if (input$breakdown == "total") {
          data <- pop[, .(value = sum(population, na.rm = TRUE)), by = year]
          data[, group := "Total"]
        } else if (input$breakdown == "sex") {
          data <- pop[, .(value = sum(population, na.rm = TRUE)), by = .(year, group = sex)]
        } else if (input$breakdown == "age") {
          pop[, age_grp := fcase(
            age <= 17, "0_17",
            age <= 24, "18_24",
            age <= 44, "25_44",
            age <= 64, "45_64",
            age <= 84, "65_84",
            default = "85_plus"
          )]
          pop <- pop[age_grp %in% input$age_groups]
          data <- pop[, .(value = sum(population, na.rm = TRUE)), by = .(year, group = age_grp)]
        }

      } else if (metric == "births") {
        req(rv$active_data$projected_births)
        births <- rv$active_data$projected_births[year %in% years]

        if (input$breakdown == "sex") {
          data <- births[, .(value = sum(births, na.rm = TRUE)), by = .(year, group = sex)]
        } else {
          data <- births[, .(value = sum(births, na.rm = TRUE)), by = year]
          data[, group := "Total"]
        }

      } else if (metric == "deaths") {
        req(rv$active_data$projected_deaths)
        deaths <- rv$active_data$projected_deaths[year %in% years]

        if (input$breakdown == "sex") {
          data <- deaths[, .(value = sum(deaths, na.rm = TRUE)), by = .(year, group = sex)]
        } else if (input$breakdown == "age") {
          deaths[, age_grp := fcase(
            age <= 17, "0_17",
            age <= 24, "18_24",
            age <= 44, "25_44",
            age <= 64, "45_64",
            age <= 84, "65_84",
            default = "85_plus"
          )]
          deaths <- deaths[age_grp %in% input$age_groups]
          data <- deaths[, .(value = sum(deaths, na.rm = TRUE)), by = .(year, group = age_grp)]
        } else {
          data <- deaths[, .(value = sum(deaths, na.rm = TRUE)), by = year]
          data[, group := "Total"]
        }

      } else if (metric == "immigration") {
        req(rv$active_data$projected_net_immigration)
        imm <- rv$active_data$projected_net_immigration[year %in% years]

        if (input$breakdown == "sex") {
          data <- imm[, .(value = sum(net_immigration, na.rm = TRUE)), by = .(year, group = sex)]
        } else {
          data <- imm[, .(value = sum(net_immigration, na.rm = TRUE)), by = year]
          data[, group := "Total"]
        }

      } else if (metric == "natural_increase") {
        req(rv$active_data$projected_births, rv$active_data$projected_deaths)

        births <- rv$active_data$projected_births[year %in% years,
                    .(births = sum(births, na.rm = TRUE)), by = year]
        deaths <- rv$active_data$projected_deaths[year %in% years,
                    .(deaths = sum(deaths, na.rm = TRUE)), by = year]

        data <- merge(births, deaths, by = "year")
        data[, value := births - deaths]
        data[, group := "Natural Increase"]

      } else if (metric == "dependency") {
        req(rv$active_data$projected_population)
        pop <- rv$active_data$projected_population[year %in% years]

        dep <- pop[, .(
          young = sum(population[age < 18], na.rm = TRUE),
          working = sum(population[age >= 18 & age < 67], na.rm = TRUE),
          elderly = sum(population[age >= 67], na.rm = TRUE)
        ), by = year]

        data <- melt(dep, id.vars = "year",
                     measure.vars = c("young", "elderly"),
                     variable.name = "group", value.name = "numerator")
        data <- merge(data, dep[, .(year, working)], by = "year")
        data[, value := numerator / working * 100]
        data[, group := factor(group,
          levels = c("young", "elderly"),
          labels = c("Young (0-17)", "Elderly (67+)")
        )]

      } else if (metric == "marital_pop") {
        req(rv$active_data$projected_marital_population)
        marital <- rv$active_data$projected_marital_population[year %in% years]
        data <- marital[, .(value = sum(population, na.rm = TRUE)),
                        by = .(year, group = marital_status)]

      } else if (metric == "pct_married") {
        req(rv$active_data$projected_marital_population)
        marital <- rv$active_data$projected_marital_population[year %in% years]

        totals <- marital[, .(total = sum(population, na.rm = TRUE)), by = year]
        married <- marital[marital_status == "married",
                           .(married = sum(population, na.rm = TRUE)), by = year]
        data <- merge(totals, married, by = "year")
        data[, value := married / total * 100]
        data[, group := "% Married"]
      }

      data
    })

    # Chart title
    output$chart_title <- renderText({
      metric_labels <- c(
        "population" = "Total Population",
        "births" = "Annual Births",
        "deaths" = "Annual Deaths",
        "immigration" = "Net Immigration",
        "natural_increase" = "Natural Increase (Births - Deaths)",
        "dependency" = "Dependency Ratio (%)",
        "marital_pop" = "Population by Marital Status",
        "amr" = "Age-Adjusted Marriage Rate",
        "adr" = "Age-Adjusted Divorce Rate",
        "pct_married" = "Percent Married"
      )
      metric_labels[input$metric]
    })

    # Main time series chart
    output$timeseries <- renderPlotly({
      data <- chart_data()
      req(data, nrow(data) > 0)

      # Set colors based on groups
      if ("group" %in% names(data)) {
        n_groups <- length(unique(data$group))
        if (all(unique(data$group) %in% c("male", "female"))) {
          colors <- SEX_COLORS
        } else if (all(unique(data$group) %in% names(MARITAL_COLORS))) {
          colors <- MARITAL_COLORS
        } else {
          colors <- viridis::viridis(n_groups)
        }

        p <- ggplot(data, aes(x = year, y = value, color = group)) +
          geom_line(linewidth = 1) +
          labs(x = NULL, y = NULL, color = NULL)

        if (is.list(colors) || is.character(colors)) {
          p <- p + scale_color_manual(values = colors)
        }
      } else {
        p <- ggplot(data, aes(x = year, y = value)) +
          geom_line(linewidth = 1, color = "#3498DB") +
          labs(x = NULL, y = NULL)
      }

      # Apply log scale if selected
      if (input$log_scale && input$metric %in% c("population", "births", "deaths")) {
        p <- p + scale_y_log10(labels = scales::comma)
      } else if (input$metric %in% c("dependency", "pct_married")) {
        p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
      } else {
        p <- p + scale_y_continuous(labels = scales::comma)
      }

      p <- p + theme_minimal() +
        theme(legend.position = "bottom")

      ggplotly(p) |>
        layout(
          legend = list(orientation = "h", y = -0.15),
          hovermode = "x unified"
        )
    })

    # Period summary table
    output$period_summary <- renderTable({
      data <- chart_data()
      req(data, nrow(data) > 0)

      # Define periods
      periods <- list(
        "2022-2030" = 2022:2030,
        "2031-2050" = 2031:2050,
        "2051-2075" = 2051:2075,
        "2076-2099" = 2076:2099
      )

      summary_list <- lapply(names(periods), function(period) {
        yrs <- periods[[period]]
        subset <- data[year %in% yrs]

        if (nrow(subset) == 0) return(NULL)

        if ("group" %in% names(data)) {
          means <- subset[, .(value = mean(value, na.rm = TRUE)), by = group]
          means[, period := period]
          means
        } else {
          data.frame(
            period = period,
            value = mean(subset$value, na.rm = TRUE)
          )
        }
      })

      summary_dt <- rbindlist(summary_list, fill = TRUE)

      if ("group" %in% names(summary_dt)) {
        summary_wide <- dcast(summary_dt, period ~ group, value.var = "value")
        summary_wide
      } else {
        summary_dt
      }
    }, digits = 0)

    # Download handler
    output$download_chart <- downloadHandler(
      filename = function() {
        paste0(input$metric, "_", input$year_range[1], "_", input$year_range[2], ".png")
      },
      content = function(file) {
        data <- chart_data()

        if ("group" %in% names(data)) {
          p <- ggplot(data, aes(x = year, y = value, color = group)) +
            geom_line(linewidth = 1) +
            labs(x = "Year", y = NULL, color = NULL)
        } else {
          p <- ggplot(data, aes(x = year, y = value)) +
            geom_line(linewidth = 1, color = "#3498DB") +
            labs(x = "Year", y = NULL)
        }

        p <- p +
          scale_y_continuous(labels = scales::comma) +
          theme_minimal() +
          theme(legend.position = "bottom")

        ggsave(file, p, width = 12, height = 6, dpi = 150)
      }
    )
  })
}
