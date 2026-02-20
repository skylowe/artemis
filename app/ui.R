# OASDI Population Projection Tool - UI Definition
# =============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$strong("OASDI"),
    tags$span(" | Population Projection Tool", class = "text-light")
  ),
  id = "main_navbar",
  theme = artemis_theme(),
  fillable = FALSE,
  header = tagList(
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::useShinyjs(),
    if (requireNamespace("waiter", quietly = TRUE)) waiter::useWaiter(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
      tags$script(src = "www/console.js")
    )
  ),

  # =============================================================================
  # Dashboard Tab
  # =============================================================================
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        title = "Quick Controls",

        # Year selector
        sliderInput(
          "dashboard_year",
          "Year",
          min = MIN_YEAR, max = MAX_YEAR,
          value = MID_YEAR,
          step = 1,
          sep = "",
          animate = animationOptions(interval = 500)
        ),

        hr(),

        # Scenario selector
        selectInput(
          "active_scenario",
          "Active Scenario",
          choices = c("ARTEMIS 2025 Baseline" = "baseline"),
          selected = "baseline"
        ),

        actionButton(
          "btn_new_scenario",
          "New Scenario",
          icon = icon("plus"),
          class = "btn-outline-primary btn-sm w-100 mb-2"
        ),

        hr(),

        # Quick stats card
        card(
          card_header("Key Metrics", class = "bg-primary text-white"),
          card_body(
            class = "p-2",
            uiOutput("quick_stats")
          )
        )
      ),

      # Main content
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",

        # Population trend
        card(
          card_header("Total Population", class = "bg-light"),
          card_body(
            plotlyOutput("dashboard_pop_trend", height = "400px")
          )
        ),

        # Population pyramid
        card(
          card_header("Population Pyramid", class = "bg-light"),
          card_body(
            plotlyOutput("dashboard_pyramid", height = "400px")
          )
        ),

        # Components of change
        card(
          card_header("Components of Change", class = "bg-light"),
          card_body(
            plotlyOutput("dashboard_components", height = "400px")
          )
        ),

        # Dependency ratios
        card(
          card_header(
            "Dependency Ratio",
            tags$small(
              class = "text-muted d-block fw-normal",
              "Dependents (ages 0\u201317 and 67+) per 100 working-age (18\u201366)"
            ),
            class = "bg-light"
          ),
          card_body(
            plotlyOutput("dashboard_dependency", height = "400px")
          )
        )
      )
    )
  ),

  # =============================================================================
  # Population Tab
  # =============================================================================
  nav_panel(
    title = "Population",
    icon = icon("users"),
    tags$div(
      class = "alert alert-warning text-center fw-bold mb-3",
      "Under Development"
    ),
    navset_card_tab(
      nav_panel(
        "Pyramids",
        mod_population_viz_ui("pop_viz")
      ),
      nav_panel(
        "Time Series",
        mod_timeseries_viz_ui("pop_ts")
      ),
      nav_panel(
        "Marital Status",
        mod_timeseries_viz_ui("marital_ts", show_marital = TRUE)
      )
    )
  ),

  # =============================================================================
  # Fertility Tab
  # =============================================================================
  nav_panel(
    title = "Fertility",
    icon = icon("baby"),
    tags$div(
      class = "alert alert-warning text-center fw-bold mb-3",
      "Under Development"
    ),
    mod_fertility_viz_ui("fertility_viz")
  ),

  # =============================================================================
  # Mortality Tab
  # =============================================================================
  nav_panel(
    title = "Mortality",
    icon = icon("heartbeat"),
    tags$div(
      class = "alert alert-warning text-center fw-bold mb-3",
      "Under Development"
    ),
    mod_mortality_viz_ui("mortality_viz")
  ),

  # =============================================================================
  # Immigration Tab
  # =============================================================================
  nav_panel(
    title = "Immigration",
    icon = icon("plane"),
    tags$div(
      class = "alert alert-warning text-center fw-bold mb-3",
      "Under Development"
    ),
    mod_immigration_viz_ui("immigration_viz")
  ),

  # =============================================================================
  # Marriage/Divorce Tab
  # =============================================================================
  nav_panel(
    title = "Marriage/Divorce",
    icon = icon("heart"),
    tags$div(
      class = "alert alert-warning text-center fw-bold mb-3",
      "Under Development"
    ),
    mod_marriage_divorce_viz_ui("marriage_viz")
  ),

  # =============================================================================
  # Scenarios Tab
  # =============================================================================
  nav_panel(
    title = "Scenarios",
    icon = icon("layer-group"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Configuration",
        mod_config_editor_ui("config_editor")
      ),
      # Main content
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        mod_scenario_manager_ui("scenario_manager")
      )
    )
  ),

  # =============================================================================
  # Comparison Tab
  # =============================================================================
  nav_panel(
    title = "Compare",
    icon = icon("balance-scale"),
    mod_comparison_view_ui("comparison")
  ),

  # =============================================================================
  # Navigation menu
  # =============================================================================
  nav_spacer(),
  nav_menu(
    title = "Help",
    icon = icon("question-circle"),
    nav_item(
      actionLink("show_about", label = tagList(icon("info-circle"), " About"))
    )
  )
)
