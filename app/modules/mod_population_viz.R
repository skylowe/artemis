# Population Visualization Module
# =============================================================================
# Population pyramids and distributions

#' Population Visualization UI
#' @param id Module namespace ID
mod_population_viz_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 250,

      # Year selector with animation
      sliderInput(
        ns("year"),
        "Year",
        min = MIN_YEAR, max = MAX_YEAR,
        value = MID_YEAR,
        step = 1,
        sep = "",
        animate = animationOptions(interval = 300)
      ),

      hr(),

      # Display options
      radioButtons(
        ns("age_grouping"),
        "Age Grouping",
        choices = c(
          "Single year" = "single",
          "5-year groups" = "five"
        ),
        selected = "five"
      ),

      checkboxInput(
        ns("show_marital"),
        "Show marital status",
        value = FALSE
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_marital")),
        checkboxGroupInput(
          ns("marital_statuses"),
          "Marital statuses:",
          choices = c(
            "Single" = "single",
            "Married" = "married",
            "Divorced" = "divorced",
            "Widowed" = "widowed"
          ),
          selected = c("single", "married", "divorced", "widowed")
        )
      ),

      hr(),

      # Population subset
      radioButtons(
        ns("population_type"),
        "Population Type",
        choices = c(
          "Total SS Area" = "total",
          "LPR Only" = "lpr",
          "O Population" = "o_pop"
        ),
        selected = "total"
      ),

      hr(),

      # Comparison options
      checkboxInput(
        ns("show_comparison"),
        "Compare to baseline",
        value = FALSE
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_comparison")),
        radioButtons(
          ns("comparison_type"),
          "Show as:",
          choices = c(
            "Overlay" = "overlay",
            "Difference" = "diff"
          ),
          selected = "overlay"
        )
      )
    ),

    # Main panel
    layout_column_wrap(
      width = 1,

      # Population pyramid
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          textOutput(ns("pyramid_title")),
          downloadButton(ns("download_pyramid"), "Download", class = "btn-sm btn-outline-secondary")
        ),
        card_body(
          plotlyOutput(ns("pyramid"), height = "550px")
        )
      ),

      # Summary stats
      layout_column_wrap(
        width = 1/3,

        value_box(
          title = "Total Population",
          value = textOutput(ns("total_pop")),
          theme = "primary"
        ),

        value_box(
          title = "Median Age",
          value = textOutput(ns("median_age")),
          theme = "info"
        ),

        value_box(
          title = "Sex Ratio (M/F)",
          value = textOutput(ns("sex_ratio")),
          theme = "secondary"
        )
      )
    )
  )
}

#' Population Visualization Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
mod_population_viz_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Get population data for selected year
    pop_data <- reactive({
      req(rv$active_data$projected_population)

      selected_year <- input$year
      pop <- rv$active_data$projected_population[year == selected_year]

      if (input$show_marital && !is.null(rv$active_data$projected_marital_population)) {
        marital <- rv$active_data$projected_marital_population[year == selected_year]
        marital <- marital[marital_status %in% input$marital_statuses]
        return(list(simple = pop, marital = marital))
      }

      list(simple = pop, marital = NULL)
    })

    # Pyramid title
    output$pyramid_title <- renderText({
      paste("Population Pyramid -", input$year)
    })

    # Population pyramid plot
    output$pyramid <- renderPlotly({
      data <- pop_data()
      req(data$simple, nrow(data$simple) > 0)

      pop <- data$simple

      # Age grouping
      if (input$age_grouping == "five") {
        pop[, age_group := cut(
          age,
          breaks = c(seq(0, 100, 5), Inf),
          labels = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+"),
          right = FALSE
        )]
        pop <- pop[, .(population = sum(population, na.rm = TRUE)),
                   by = .(age_group, sex)]
      } else {
        pop[, age_group := as.character(age)]
        pop <- pop[, .(population = sum(population, na.rm = TRUE)),
                   by = .(age_group, sex)]
      }

      # Create pyramid data
      if (!is.null(data$marital) && input$show_marital) {
        marital <- data$marital

        if (input$age_grouping == "five") {
          marital[, age_group := cut(
            age,
            breaks = c(seq(0, 100, 5), Inf),
            labels = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+"),
            right = FALSE
          )]
        } else {
          marital[, age_group := as.character(age)]
        }

        marital <- marital[, .(population = sum(population, na.rm = TRUE)),
                           by = .(age_group, sex, marital_status)]

        # Make male negative
        marital[sex == "male", population := -population]

        p <- ggplot(marital, aes(x = age_group, y = population / 1e6,
                                  fill = marital_status)) +
          geom_col() +
          coord_flip() +
          facet_wrap(~sex, scales = "free_x") +
          scale_fill_manual(values = MARITAL_COLORS, name = "Status") +
          labs(x = "Age Group", y = "Population (millions)") +
          scale_y_continuous(labels = function(x) abs(x)) +
          theme_minimal() +
          theme(legend.position = "bottom")

      } else {
        # Simple pyramid
        pop[sex == "male", population := -population]

        p <- ggplot(pop, aes(x = age_group, y = population / 1e6, fill = sex)) +
          geom_col() +
          coord_flip() +
          scale_fill_manual(values = SEX_COLORS) +
          labs(x = "Age Group", y = "Population (millions)") +
          scale_y_continuous(labels = function(x) abs(x)) +
          theme_minimal() +
          theme(legend.position = "bottom")
      }

      # Add comparison if enabled
      if (input$show_comparison && !is.null(rv$baseline$projected_population)) {
        baseline_pop <- rv$baseline$projected_population[year == input$year]

        if (input$age_grouping == "five") {
          baseline_pop[, age_group := cut(
            age,
            breaks = c(seq(0, 100, 5), Inf),
            labels = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+"),
            right = FALSE
          )]
          baseline_pop <- baseline_pop[, .(baseline = sum(population, na.rm = TRUE)),
                                        by = .(age_group, sex)]
        } else {
          baseline_pop[, age_group := as.character(age)]
          baseline_pop <- baseline_pop[, .(baseline = sum(population, na.rm = TRUE)),
                                        by = .(age_group, sex)]
        }

        baseline_pop[sex == "male", baseline := -baseline]

        # Add outline for baseline
        p <- p +
          geom_step(
            data = baseline_pop,
            aes(x = age_group, y = baseline / 1e6, group = sex),
            color = "black",
            linewidth = 0.8,
            linetype = "dashed",
            inherit.aes = FALSE
          )
      }

      ggplotly(p) |>
        layout(legend = list(orientation = "h", y = -0.15))
    })

    # Summary statistics
    output$total_pop <- renderText({
      data <- pop_data()
      req(data$simple)
      total <- sum(data$simple$population, na.rm = TRUE)
      paste0(format(round(total / 1e6, 2), big.mark = ","), " M")
    })

    output$median_age <- renderText({
      data <- pop_data()
      req(data$simple)

      pop <- data$simple
      cum_pop <- pop[order(age), .(age = age, cum = cumsum(population))]
      total <- sum(pop$population, na.rm = TRUE)
      median_row <- cum_pop[cum >= total / 2][1]
      paste(median_row$age, "years")
    })

    output$sex_ratio <- renderText({
      data <- pop_data()
      req(data$simple)

      males <- sum(data$simple[sex == "male"]$population, na.rm = TRUE)
      females <- sum(data$simple[sex == "female"]$population, na.rm = TRUE)
      sprintf("%.3f", males / females)
    })

    # Download handler
    output$download_pyramid <- downloadHandler(
      filename = function() {
        paste0("population_pyramid_", input$year, ".png")
      },
      content = function(file) {
        data <- pop_data()
        pop <- data$simple

        if (input$age_grouping == "five") {
          pop[, age_group := cut(
            age,
            breaks = c(seq(0, 100, 5), Inf),
            labels = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+"),
            right = FALSE
          )]
          pop <- pop[, .(population = sum(population, na.rm = TRUE)),
                     by = .(age_group, sex)]
        } else {
          pop[, age_group := as.character(age)]
          pop <- pop[, .(population = sum(population, na.rm = TRUE)),
                     by = .(age_group, sex)]
        }

        pop[sex == "male", population := -population]

        p <- ggplot(pop, aes(x = age_group, y = population / 1e6, fill = sex)) +
          geom_col() +
          coord_flip() +
          scale_fill_manual(values = SEX_COLORS) +
          labs(
            x = "Age Group",
            y = "Population (millions)",
            title = paste("Population Pyramid -", input$year)
          ) +
          scale_y_continuous(labels = function(x) abs(x)) +
          theme_minimal() +
          theme(legend.position = "bottom")

        ggsave(file, p, width = 10, height = 8, dpi = 150)
      }
    )
  })
}
