# OASDI Population Projection Tool - UI Definition
# =============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$strong("OASDI"),
    tags$span(" | Population Projection Tool", class = "text-light")
  ),
  id = "main_navbar",
  theme = artemis_theme(),
  fillable = TRUE,
  header = tagList(
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::useShinyjs(),
    if (requireNamespace("waiter", quietly = TRUE)) waiter::useWaiter(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
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
        width = 300,
        title = "Quick Controls",

        # Year selector
        sliderInput(
          "dashboard_year",
          "Year",
          min = 2022, max = 2099,
          value = 2050,
          step = 1,
          sep = "",
          animate = animationOptions(interval = 500)
        ),

        hr(),

        # Scenario selector
        selectInput(
          "active_scenario",
          "Active Scenario",
          choices = c("TR2025 Baseline" = "baseline"),
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
            plotlyOutput("dashboard_pop_trend", height = "280px")
          )
        ),

        # Population pyramid
        card(
          card_header("Population Pyramid", class = "bg-light"),
          card_body(
            plotlyOutput("dashboard_pyramid", height = "280px")
          )
        ),

        # Components of change
        card(
          card_header("Components of Change", class = "bg-light"),
          card_body(
            plotlyOutput("dashboard_components", height = "280px")
          )
        ),

        # Dependency ratios
        card(
          card_header("Dependency Ratios", class = "bg-light"),
          card_body(
            plotlyOutput("dashboard_dependency", height = "280px")
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
    mod_fertility_viz_ui("fertility_viz")
  ),

  # =============================================================================
  # Mortality Tab
  # =============================================================================
  nav_panel(
    title = "Mortality",
    icon = icon("heartbeat"),
    mod_mortality_viz_ui("mortality_viz")
  ),

  # =============================================================================
  # Immigration Tab
  # =============================================================================
  nav_panel(
    title = "Immigration",
    icon = icon("plane"),
    mod_immigration_viz_ui("immigration_viz")
  ),

  # =============================================================================
  # Marriage/Divorce Tab
  # =============================================================================
  nav_panel(
    title = "Marriage/Divorce",
    icon = icon("heart"),
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
        width = 350,
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
      tags$a(
        icon("book"), "Documentation",
        href = "https://github.com/skylerlee/artemis",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        icon("file-alt"), "TR2025 Methodology",
        href = "#",
        onclick = "Shiny.setInputValue('show_methodology', Math.random())"
      )
    ),
    nav_item(
      actionLink("show_about", label = tagList(icon("info-circle"), " About OASDI"))
    )
  )
)
