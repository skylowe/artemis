# Mortality Visualization Module
# =============================================================================
# Death probabilities, life expectancy, and mortality trends

#' Mortality Visualization UI
#' @param id Module namespace ID
mod_mortality_viz_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 250,

      selectInput(
        ns("chart_type"),
        "Chart Type",
        choices = c(
          "Life Expectancy Trend" = "le_trend",
          "Death Probabilities (qx)" = "qx_curves",
          "Mortality Surface" = "surface",
          "Improvement Rates (AAx)" = "aax"
        ),
        selected = "le_trend"
      ),

      hr(),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'le_trend'", ns("chart_type")),
        checkboxGroupInput(
          ns("le_ages"),
          "Life Expectancy at Age:",
          choices = c("0 (at birth)" = "0", "65" = "65", "85" = "85"),
          selected = c("0", "65")
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'qx_curves'", ns("chart_type")),
        sliderInput(
          ns("qx_year_range"),
          "Year Range",
          min = MIN_YEAR + 1, max = MAX_YEAR,
          value = c(MIN_YEAR + 1, MAX_YEAR),
          step = 1,
          sep = ""
        ),
        checkboxInput(
          ns("log_scale_qx"),
          "Log scale",
          value = TRUE
        )
      ),

      hr(),

      radioButtons(
        ns("sex_filter"),
        "Sex",
        choices = c("Both" = "both", "Male" = "male", "Female" = "female"),
        selected = "both"
      ),

      hr(),

      sliderInput(
        ns("year_range"),
        "Year Range",
        min = MIN_YEAR, max = MAX_YEAR,
        value = c(MIN_YEAR, MAX_YEAR),
        step = 1,
        sep = ""
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
        "By Sex",
        card_body(
          plotlyOutput(ns("sex_comparison"), height = "550px")
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

#' Mortality Visualization Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
mod_mortality_viz_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Dynamically update year sliders when projection data changes
    observe({
      data <- rv$active_data$mortality_qx_projected
      req(data)
      max_year <- max(data$year, na.rm = TRUE)
      for (sid in c("qx_year_range", "year_range")) {
        current <- isolate(input[[sid]])
        if (!is.null(current) && max_year != current[2]) {
          updateSliderInput(session, sid,
            max = max_year,
            value = c(current[1], max_year))
        }
      }
    })

    # Calculate life expectancy from qx
    calc_life_expectancy <- function(qx_data, at_age = 0) {
      # Sort by age
      qx_data <- qx_data[order(age)]

      # Calculate survival probability
      px <- 1 - qx_data$qx
      lx <- cumprod(c(1, px[-length(px)]))

      # Number surviving to each age
      qx_data$lx <- lx * 100000

      # Deaths at each age
      qx_data$dx <- c(-diff(qx_data$lx), qx_data$lx[nrow(qx_data)])

      # Person-years lived
      qx_data$Lx <- qx_data$lx - 0.5 * qx_data$dx

      # Total person-years remaining
      qx_data$Tx <- rev(cumsum(rev(qx_data$Lx)))

      # Life expectancy
      qx_data$ex <- qx_data$Tx / qx_data$lx

      if (at_age %in% qx_data$age) {
        return(qx_data[age == at_age]$ex)
      }
      NA
    }

    # Main chart
    output$main_chart <- renderPlotly({
      req(rv$active_data$mortality_qx_projected)

      qx <- rv$active_data$mortality_qx_projected
      chart_type <- input$chart_type
      years <- seq(input$year_range[1], input$year_range[2])

      # Filter by sex
      if (input$sex_filter != "both") {
        qx <- qx[sex == input$sex_filter]
      }

      qx <- qx[year %in% years]

      if (chart_type == "le_trend") {
        # Life expectancy trends
        le_ages <- as.numeric(input$le_ages)

        le_data <- lapply(unique(qx$year), function(y) {
          lapply(unique(qx$sex), function(s) {
            year_qx <- qx[year == y & sex == s]
            if (nrow(year_qx) == 0) return(NULL)

            data.frame(
              year = y,
              sex = s,
              e0 = if (0 %in% le_ages) calc_life_expectancy(year_qx, 0) else NA,
              e65 = if (65 %in% le_ages) calc_life_expectancy(year_qx, 65) else NA,
              e85 = if (85 %in% le_ages) calc_life_expectancy(year_qx, 85) else NA
            )
          })
        })

        le_dt <- rbindlist(unlist(le_data, recursive = FALSE), fill = TRUE)

        # Reshape for plotting
        le_long <- melt(
          le_dt,
          id.vars = c("year", "sex"),
          measure.vars = c("e0", "e65", "e85"),
          variable.name = "age",
          value.name = "expectancy"
        )
        le_long <- le_long[!is.na(expectancy)]

        le_long[, age := factor(age,
          levels = c("e0", "e65", "e85"),
          labels = c("At Birth", "At 65", "At 85")
        )]

        if (input$sex_filter == "both") {
          p <- ggplot(le_long, aes(x = year, y = expectancy,
                                    color = age, linetype = sex)) +
            geom_line(linewidth = 1) +
            scale_linetype_manual(values = c("male" = "solid", "female" = "dashed"))
        } else {
          p <- ggplot(le_long, aes(x = year, y = expectancy, color = age)) +
            geom_line(linewidth = 1)
        }

        p <- p +
          labs(x = NULL, y = "Years", color = "Life Expectancy", linetype = "Sex") +
          scale_color_viridis_d() +
          theme_artemis()

        ggplotly(p) |> layout_artemis()

      } else if (chart_type == "qx_curves") {
        # Death probability curves
        selected_years <- unique(qx$year)
        selected_years <- selected_years[
          selected_years >= input$qx_year_range[1] &
          selected_years <= input$qx_year_range[2]
        ]

        if (length(selected_years) > 5) {
          idx <- round(seq(1, length(selected_years), length.out = 5))
          selected_years <- selected_years[idx]
        }

        qx_subset <- qx[year %in% selected_years]

        if (input$sex_filter == "both") {
          qx_subset <- qx_subset[, .(qx = mean(qx, na.rm = TRUE)),
                                  by = .(year, age)]
        }

        qx_subset[, year_factor := factor(year)]

        p <- ggplot(qx_subset, aes(x = age, y = qx, color = year_factor)) +
          geom_line(linewidth = 1) +
          labs(x = "Age", y = "Death Probability (qx)", color = "Year") +
          scale_color_viridis_d() +
          theme_artemis()

        if (input$log_scale_qx) {
          p <- p + scale_y_log10(labels = scales::scientific)
        }

        ggplotly(p) |> layout_artemis()

      } else if (chart_type == "surface") {
        # 3D mortality surface
        if (input$sex_filter == "both") {
          qx <- qx[, .(qx = mean(qx, na.rm = TRUE)), by = .(year, age)]
        }

        qx_wide <- dcast(qx, age ~ year, value.var = "qx", fun.aggregate = mean)
        z <- as.matrix(qx_wide[, -1])
        rownames(z) <- qx_wide$age

        plot_ly(
          x = as.numeric(colnames(z)),
          y = as.numeric(rownames(z)),
          z = log10(z + 1e-6),
          type = "surface",
          colorscale = "RdYlGn",
          reversescale = TRUE
        ) |>
          layout(
            scene = list(
              xaxis = list(title = "Year"),
              yaxis = list(title = "Age"),
              zaxis = list(title = "log10(qx)")
            )
          )

      } else if (chart_type == "aax") {
        # Mortality improvement rates
        qx_by_year <- qx[, .(qx = mean(qx, na.rm = TRUE)), by = .(year, age)]
        setorder(qx_by_year, age, year)

        # Calculate annual improvement
        qx_by_year[, prev_qx := shift(qx, 1), by = age]
        qx_by_year[, improvement := (prev_qx - qx) / prev_qx * 100]

        # Average by age group
        qx_by_year[, age_group := cut(
          age,
          breaks = c(0, 15, 50, 65, 85, Inf),
          labels = c("0-14", "15-49", "50-64", "65-84", "85+"),
          right = FALSE
        )]

        aax <- qx_by_year[!is.na(improvement),
                          .(improvement = mean(improvement, na.rm = TRUE)),
                          by = .(year, age_group)]

        p <- ggplot(aax, aes(x = year, y = improvement, color = age_group)) +
          geom_line(linewidth = 1) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#7F8C8D") +
          labs(x = NULL, y = "Annual Improvement (%)", color = "Age Group") +
          scale_color_viridis_d() +
          theme_artemis()

        ggplotly(p) |> layout_artemis()
      }
    })

    # Sex comparison chart
    output$sex_comparison <- renderPlotly({
      req(rv$active_data$mortality_qx_projected)

      qx <- rv$active_data$mortality_qx_projected
      years <- seq(input$year_range[1], input$year_range[2])
      qx <- qx[year %in% years]

      if (input$chart_type == "le_trend") {
        # Life expectancy by sex
        le_data <- lapply(unique(qx$year), function(y) {
          lapply(c("male", "female"), function(s) {
            year_qx <- qx[year == y & sex == s]
            if (nrow(year_qx) == 0) return(NULL)
            data.frame(
              year = y,
              sex = s,
              e0 = calc_life_expectancy(year_qx, 0)
            )
          })
        })

        le_dt <- rbindlist(unlist(le_data, recursive = FALSE), fill = TRUE)
        le_dt <- le_dt[!is.na(e0)]

        # Calculate gap
        le_wide <- dcast(le_dt, year ~ sex, value.var = "e0")
        le_wide[, gap := female - male]

        p1 <- ggplot(le_dt, aes(x = year, y = e0, color = sex)) +
          geom_line(linewidth = 1) +
          scale_color_manual(values = SEX_COLORS) +
          labs(x = NULL, y = "Life Expectancy at Birth", color = NULL) +
          theme_artemis()

        p2 <- ggplot(le_wide, aes(x = year, y = gap)) +
          geom_line(linewidth = 1, color = "#9B59B6") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(x = NULL, y = "Female-Male Gap (years)") +
          theme_artemis()

        subplot(ggplotly(p1), ggplotly(p2), nrows = 2, shareX = TRUE)

      } else {
        # qx ratio by sex
        qx_wide <- dcast(
          qx[, .(qx = mean(qx, na.rm = TRUE)), by = .(year, age, sex)],
          year + age ~ sex,
          value.var = "qx"
        )
        qx_wide[, ratio := male / female]

        # Average ratio by age group
        qx_wide[, age_group := cut(
          age,
          breaks = c(0, 25, 45, 65, 85, Inf),
          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
          right = FALSE
        )]

        ratio_agg <- qx_wide[, .(ratio = mean(ratio, na.rm = TRUE)),
                             by = .(year, age_group)]

        p <- ggplot(ratio_agg, aes(x = year, y = ratio, color = age_group)) +
          geom_line(linewidth = 1) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "#7F8C8D") +
          labs(x = NULL, y = "Male/Female qx Ratio", color = "Age Group") +
          scale_color_viridis_d() +
          theme_artemis()

        ggplotly(p) |> layout_artemis()
      }
    })

    # Data table
    output$data_table <- renderDT({
      req(rv$active_data$mortality_qx_projected)

      qx <- rv$active_data$mortality_qx_projected
      years <- seq(input$year_range[1], input$year_range[2])
      qx <- qx[year %in% years]

      if (input$sex_filter != "both") {
        qx <- qx[sex == input$sex_filter]
      }

      # Show summary by year and sex
      summary_dt <- qx[, .(
        mean_qx = mean(qx, na.rm = TRUE),
        median_qx = median(qx, na.rm = TRUE),
        min_qx = min(qx, na.rm = TRUE),
        max_qx = max(qx, na.rm = TRUE)
      ), by = .(year, sex)]

      datatable(summary_dt, options = list(pageLength = 15)) |>
        formatRound(columns = 3:6, digits = 6)
    })
  })
}
