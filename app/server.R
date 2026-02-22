# ARTEMIS Visualization Tool - Server Logic
# =============================================================================

server <- function(input, output, session) {

  # ===========================================================================
  # Reactive Values
  # ===========================================================================

  # Store application state
  rv <- reactiveValues(
    # Baseline data (loaded once)
    baseline = NULL,

    # Current configuration
    config = NULL,

    # Saved scenarios
    scenarios = list(),

    # Active scenario data
    active_data = NULL,

    # Loading state
    loading = FALSE
  )

  # ===========================================================================
  # Initialize Application
  # ===========================================================================

  observe({
    # Show loading screen (if waiter package available)
    waiter <- if (requireNamespace("waiter", quietly = TRUE)) {
      waiter::Waiter$new(
        html = tagList(
          waiter::spin_fading_circles(),
          br(),
          h4("Loading ARTEMIS..."),
          p("Loading baseline projections")
        ),
        color = "#2C3E50"
      )
    } else NULL

    if (!is.null(waiter)) waiter$show()

    # Load baseline data
    rv$baseline <- load_baseline_data()

    # Load config
    tryCatch({
      rv$config <- load_config()
    }, error = function(e) {
      showNotification(
        paste("Could not load config:", e$message),
        type = "warning"
      )
      rv$config <- list()
    })

    # Set active data to baseline
    rv$active_data <- rv$baseline

    # Load saved scenarios
    rv$scenarios <- load_saved_scenarios()

    # Update scenario selector
    if (length(rv$scenarios) > 0) {
      scenario_choices <- c(
        "ARTEMIS 2025 Baseline" = "baseline",
        setNames(names(rv$scenarios), names(rv$scenarios))
      )
      updateSelectInput(session, "active_scenario", choices = scenario_choices)
    }

    if (!is.null(waiter)) waiter$hide()
  }) |> bindEvent(TRUE, once = TRUE)

  # ===========================================================================
  # Dashboard Outputs
  # ===========================================================================

  # Quick stats
  output$quick_stats <- renderUI({
    req(rv$active_data$projected_population)

    pop <- rv$active_data$projected_population
    selected_year <- input$dashboard_year %||% MID_YEAR

    pop_year <- pop[year == selected_year]
    total <- sum(pop_year$population, na.rm = TRUE)

    births <- rv$active_data$projected_births
    deaths <- rv$active_data$projected_deaths

    births_year <- if (!is.null(births)) {
      sum(births[year == selected_year]$births, na.rm = TRUE)
    } else NA

    deaths_year <- if (!is.null(deaths)) {
      sum(deaths[year == selected_year]$deaths, na.rm = TRUE)
    } else NA

    tagList(
      tags$div(
        class = "d-flex justify-content-between mb-1",
        tags$span("Population:"),
        tags$strong(format(round(total / 1e6, 1), big.mark = ","), "M")
      ),
      tags$div(
        class = "d-flex justify-content-between mb-1",
        tags$span("Births:"),
        tags$strong(
          if (!is.na(births_year)) {
            paste0(format(round(births_year / 1e6, 2), big.mark = ","), "M")
          } else "N/A"
        )
      ),
      tags$div(
        class = "d-flex justify-content-between mb-1",
        tags$span("Deaths:"),
        tags$strong(
          if (!is.na(deaths_year)) {
            paste0(format(round(deaths_year / 1e6, 2), big.mark = ","), "M")
          } else "N/A"
        )
      )
    )
  })

  # Population trend
  output$dashboard_pop_trend <- renderPlotly({
    req(rv$active_data$projected_population)

    pop <- rv$active_data$projected_population
    pop_by_year <- pop[, .(population = sum(population, na.rm = TRUE)), by = year]

    p <- ggplot(pop_by_year, aes(x = year, y = population / 1e6)) +
      geom_line(color = "#3498DB", linewidth = 1) +
      geom_vline(
        xintercept = input$dashboard_year %||% MID_YEAR,
        linetype = "dashed",
        color = "#E74C3C"
      ) +
      labs(x = NULL, y = "Population (millions)") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(hovermode = "x unified")
  })

  # Population pyramid
  output$dashboard_pyramid <- renderPlotly({
    req(rv$active_data$projected_population)

    pop <- rv$active_data$projected_population
    selected_year <- input$dashboard_year %||% MID_YEAR

    pop_year <- pop[year == selected_year]

    # Group into 5-year age groups
    pop_year[, age_group := cut(
      age,
      breaks = c(seq(0, 100, 5), Inf),
      labels = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+"),
      right = FALSE
    )]

    pyramid <- pop_year[, .(population = sum(population, na.rm = TRUE)),
                        by = .(age_group, sex)]

    # Convert to percent of total population
    total_pop <- sum(abs(pyramid$population))
    pyramid[, pct := population / total_pop * 100]

    # Make male negative for pyramid
    pyramid[sex == "male", pct := -pct]

    p <- ggplot(pyramid, aes(x = age_group, y = pct, fill = sex)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = SEX_COLORS) +
      labs(x = NULL, y = "% of Total Population") +
      scale_y_continuous(limits = c(-5, 5), labels = function(x) paste0(abs(x), "%")) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) |>
      layout(
        legend = list(orientation = "h", y = -0.2),
        margin = list(b = 80)
      )
  })

  # Components of change
  output$dashboard_components <- renderPlotly({
    req(rv$active_data)

    births <- rv$active_data$projected_births
    deaths <- rv$active_data$projected_deaths
    immigration <- rv$active_data$projected_net_immigration

    if (is.null(births) || is.null(deaths)) {
      return(plotly_empty() |> layout(title = "Data not available"))
    }

    # Aggregate by year
    births_agg <- births[, .(value = sum(births, na.rm = TRUE), component = "Births"), by = year]
    deaths_agg <- deaths[, .(value = -sum(deaths, na.rm = TRUE), component = "Deaths"), by = year]

    components <- rbindlist(list(births_agg, deaths_agg))

    if (!is.null(immigration)) {
      imm_agg <- immigration[, .(value = sum(net_immigration, na.rm = TRUE),
                                  component = "Net Immigration"), by = year]
      components <- rbindlist(list(components, imm_agg))
    }

    p <- ggplot(components, aes(x = year, y = value / 1e6, fill = component)) +
      geom_area(alpha = 0.7) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      geom_vline(
        xintercept = input$dashboard_year %||% MID_YEAR,
        linetype = "dashed",
        color = "#2C3E50"
      ) +
      scale_fill_manual(values = c(
        "Births" = "#8E6FBF",
        "Deaths" = "#C4635F",
        "Net Immigration" = "#5A9BD5"
      )) +
      labs(x = NULL, y = "Millions", fill = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) |>
      layout(legend = list(orientation = "h", y = -0.1))
  })

  # Dependency ratios
  output$dashboard_dependency <- renderPlotly({
    req(rv$active_data$projected_population)

    pop <- rv$active_data$projected_population

    # Calculate dependency ratios
    dep <- pop[, .(
      young = sum(population[age < 18], na.rm = TRUE),
      working = sum(population[age >= 18 & age < 67], na.rm = TRUE),
      elderly = sum(population[age >= 67], na.rm = TRUE)
    ), by = year]

    dep[, `:=`(
      young_ratio = young / working * 100,
      elderly_ratio = elderly / working * 100,
      total_ratio = (young + elderly) / working * 100
    )]

    dep_long <- melt(
      dep,
      id.vars = "year",
      measure.vars = c("young_ratio", "elderly_ratio", "total_ratio"),
      variable.name = "type",
      value.name = "ratio"
    )

    dep_long[, type := factor(type,
      levels = c("young_ratio", "elderly_ratio", "total_ratio"),
      labels = c("Young (0\u201317)", "Elderly (67+)", "Total Dependency")
    )]

    p <- ggplot(dep_long, aes(x = year, y = ratio, color = type, group = type)) +
      geom_line(linewidth = 1) +
      geom_vline(
        xintercept = input$dashboard_year %||% MID_YEAR,
        linetype = "dashed",
        color = "#7F8C8D"
      ) +
      scale_color_manual(values = c(
        "Young (0\u201317)" = "#9B59B6",
        "Elderly (67+)" = "#E67E22",
        "Total Dependency" = "#2C3E50"
      )) +
      labs(x = NULL, y = "Dependents per 100 working-age", color = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p, tooltip = c("x", "color", "y")) |>
      layout(
        legend = list(orientation = "h", y = -0.15),
        hovermode = "x unified"
      )
  })

  # ===========================================================================
  # Module Servers
  # ===========================================================================

  # Configuration editor
  config_result <- mod_config_editor_server("config_editor", rv)

  # Scenario manager (pass parent session for tab navigation)
  mod_scenario_manager_server("scenario_manager", rv, config_result, parent_session = session)

  # Population visualization
  mod_population_viz_server("pop_viz", rv)

  # Time series visualization
  mod_timeseries_viz_server("pop_ts", rv, show_marital = FALSE)
  mod_timeseries_viz_server("marital_ts", rv, show_marital = TRUE)

  # Fertility visualization
  mod_fertility_viz_server("fertility_viz", rv)

  # Mortality visualization
  mod_mortality_viz_server("mortality_viz", rv)

  # Immigration visualization
  mod_immigration_viz_server("immigration_viz", rv)

  # Marriage/Divorce visualization
  mod_marriage_divorce_viz_server("marriage_viz", rv)

  # US Employment visualization
  mod_employment_viz_server("employment_viz", rv)

  # Comparison view
  mod_comparison_view_server("comparison", rv)

  # ===========================================================================
  # Scenario Switching
  # ===========================================================================

  # Update scenario dropdown when scenarios change

  observe({
    scenario_choices <- c("ARTEMIS 2025 Baseline" = "baseline")
    if (length(rv$scenarios) > 0) {
      scenario_choices <- c(
        scenario_choices,
        setNames(names(rv$scenarios), names(rv$scenarios))
      )
    }
    updateSelectInput(session, "active_scenario",
                      choices = scenario_choices,
                      selected = input$active_scenario)
  })

  observeEvent(input$active_scenario, {
    if (input$active_scenario == "baseline") {
      rv$active_data <- rv$baseline
    } else if (input$active_scenario %in% names(rv$scenarios)) {
      rv$active_data <- rv$scenarios[[input$active_scenario]]$results
    }
  })

  # ===========================================================================
  # New Scenario Button
  # ===========================================================================

  observeEvent(input$btn_new_scenario, {
    updateTabsetPanel(session, "main_navbar", selected = "Scenarios")
  })

  # ===========================================================================
  # About Modal
  # ===========================================================================

  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = "About ARTEMIS",
      tagList(
        h4("ARTEMIS"),
        p(
          "ARTEMIS is an open-source replication of the Social Security Administration's",
          "Office of the Chief Actuary (OCACT) long-range demographic projection model.",
          "It projects the U.S. Social Security area population by age, sex, and marital status",
          "using the component method: starting from a base population, it applies age-specific",
          "birth rates, death rates, and net immigration each year to produce annual population",
          "estimates through the end of the projection period."
        ),
        p(
          "The model implements all eight demographic subprocesses described in OCACT's",
          "methodology documentation: fertility, mortality, LPR immigration, historical population,",
          "other (non-LPR) immigration, marriage, divorce, and projected population.",
          "Each subprocess can be configured independently through the assumptions panel,",
          "allowing scenario analysis and sensitivity testing against the official Trustees Report projections."
        ),
        hr(),
        h5("Data Sources"),
        tags$ul(
          class = "small",
          tags$li(tags$strong("SSA/OCACT:"), " Trustees Report population projections (V.A2 immigration totals,",
                  " historical death probabilities, official Dec 31 population by age/sex/marital status)"),
          tags$li(tags$strong("National Center for Health Statistics (NCHS):"), " Birth certificate microdata",
                  " (births by age of mother, birth rates), death certificate data (deaths by age/sex/cause),",
                  " provisional vital statistics for recent years"),
          tags$li(tags$strong("Census Bureau:"), " Population estimates (intercensal and postcensal),",
                  " American Community Survey (ACS) PUMS microdata (marital status, foreign-born populations,",
                  " same-sex households), Current Population Survey (CPS) marital status data"),
          tags$li(tags$strong("Human Mortality Database (HMD):"), " Life tables for ages 85+ calibration",
                  " (Kannisto elderly mortality model)"),
          tags$li(tags$strong("Department of Homeland Security (DHS):"), " Yearbook of Immigration Statistics",
                  " (LPR admissions, adjustments of status by age/sex/country, emigration estimates)"),
          tags$li(tags$strong("Congressional Budget Office (CBO):"), " Immigration projections",
                  " used for LPR assumption calibration"),
          tags$li(tags$strong("CDC WONDER:"), " Provisional mortality data for recent years",
                  " (bridged-race population denominators)")
        )
      ),
      footer = modalButton("Close"),
      size = "l"
    ))
  })
}

# ===========================================================================
# Helper Functions
# ===========================================================================

#' Load saved scenarios from disk
load_saved_scenarios <- function() {
  scenarios <- list()

  if (dir.exists(SCENARIOS_DIR)) {
    rds_files <- list.files(SCENARIOS_DIR, pattern = "\\.rds$", full.names = TRUE)
    for (f in rds_files) {
      name <- tools::file_path_sans_ext(basename(f))
      tryCatch({
        scenarios[[name]] <- readRDS(f)
      }, error = function(e) {
        cli::cli_alert_warning("Could not load scenario: {.file {name}}")
      })
    }
  }

  scenarios
}
