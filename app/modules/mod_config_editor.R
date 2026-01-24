# Configuration Editor Module
# =============================================================================
# Allows users to modify projection assumptions

#' Configuration Editor UI
#' @param id Module namespace ID
mod_config_editor_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # CRITICAL Parameters - Always Visible
    h5("Critical Parameters", class = "text-primary"),

    # Ultimate TFR
    sliderInput(
      ns("ultimate_tfr"),
      "Ultimate TFR",
      min = 1.0, max = 3.0,
      value = 1.90,
      step = 0.05
    ),

    # Immigration Scenario
    selectInput(
      ns("immigration_scenario"),
      "Immigration Scenario",
      choices = c(
        "Low Cost" = "low",
        "Intermediate" = "intermediate",
        "High Cost" = "high"
      ),
      selected = "intermediate"
    ),

    # Ultimate Marriage Rate
    sliderInput(
      ns("ultimate_amr"),
      "Ultimate Marriage Rate (AMR)",
      min = 2000, max = 6000,
      value = 4000,
      step = 100,
      post = " per 100K"
    ),

    # Ultimate Divorce Rate
    sliderInput(
      ns("ultimate_adr"),
      "Ultimate Divorce Rate (ADR)",
      min = 1000, max = 2500,
      value = 1700,
      step = 50,
      post = " per 100K"
    ),

    hr(),

    # HIGH Priority Parameters - Collapsible
    accordion(
      id = ns("high_params"),
      open = FALSE,

      accordion_panel(
        title = "Fertility Options",
        icon = icon("baby"),

        numericInput(
          ns("fertility_ultimate_year"),
          "Ultimate Year",
          value = 2050,
          min = 2030, max = 2080
        ),

        numericInput(
          ns("weight_exponent"),
          "Interpolation Weight",
          value = 1.5,
          min = 1.0, max = 3.0,
          step = 0.1
        ),

        checkboxInput(
          ns("constrain_tfr"),
          "Constrain TFR to TR2025 values",
          value = TRUE
        )
      ),

      accordion_panel(
        title = "Mortality Options",
        icon = icon("heartbeat"),

        numericInput(
          ns("mortality_ultimate_year"),
          "Ultimate Year",
          value = 2043,
          min = 2030, max = 2080
        ),

        selectInput(
          ns("starting_aax_method"),
          "Starting AAx Method",
          choices = c(
            "TR2025 qx Values" = "tr_qx",
            "Regression-based" = "regression",
            "Capped at Multiple" = "capped"
          ),
          selected = "tr_qx"
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'capped'", ns("starting_aax_method")),
          sliderInput(
            ns("aax_cap_multiplier"),
            "Cap Multiplier",
            min = 1.0, max = 3.0,
            value = 1.5,
            step = 0.1
          )
        ),

        checkboxInput(
          ns("apply_covid_adjustments"),
          "Apply COVID-19 Adjustments",
          value = TRUE
        )
      ),

      accordion_panel(
        title = "Immigration Options",
        icon = icon("plane"),

        selectInput(
          ns("distribution_method"),
          "Age-Sex Distribution",
          choices = c(
            "TR2025-derived" = "tr_derived",
            "DHS Historical" = "dhs"
          ),
          selected = "tr_derived"
        ),

        sliderInput(
          ns("emigration_ratio"),
          "Emigration Ratio",
          min = 0.10, max = 0.50,
          value = 0.25,
          step = 0.01
        )
      )
    ),

    hr(),

    # Data Source Options
    accordion(
      id = ns("data_source_params"),
      open = FALSE,

      accordion_panel(
        title = "Data Sources",
        icon = icon("database"),
        class = "text-secondary",

        # Quick toggle buttons
        div(
          class = "btn-group w-100 mb-3",
          role = "group",
          actionButton(
            ns("use_all_tr"),
            "Use All TR2025",
            class = "btn-outline-primary btn-sm"
          ),
          actionButton(
            ns("use_all_artemis"),
            "Use All ARTEMIS",
            class = "btn-outline-secondary btn-sm"
          )
        ),

        checkboxInput(
          ns("use_tr_historical_pop"),
          "Starting Population: TR2025",
          value = TRUE
        ),

        checkboxInput(
          ns("use_tr2025_births"),
          "Recent Births: TR2025 (2023-24)",
          value = TRUE
        )
      )
    ),

    hr(),

    # Action buttons
    div(
      class = "d-grid gap-2",
      actionButton(
        ns("apply_config"),
        "Apply Changes",
        icon = icon("check"),
        class = "btn-primary"
      ),
      actionButton(
        ns("reset_config"),
        "Reset to TR2025",
        icon = icon("undo"),
        class = "btn-outline-secondary"
      ),
      actionButton(
        ns("export_config"),
        "Export as YAML",
        icon = icon("download"),
        class = "btn-outline-info"
      )
    )
  )
}

#' Configuration Editor Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
#' @return Reactive with modified configuration
mod_config_editor_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Track config modifications
    modified_config <- reactiveVal(NULL)

    # Initialize from baseline config
    observe({
      req(rv$config)
      config <- rv$config

      # Update inputs to match loaded config
      updateSliderInput(session, "ultimate_tfr",
        value = config$fertility$ultimate_ctfr %||% 1.90)

      updateSelectInput(session, "immigration_scenario",
        selected = config$immigration$va2_alternative %||% "intermediate")

      updateSliderInput(session, "ultimate_amr",
        value = config$marriage$ultimate_amr %||% 4000)

      updateSliderInput(session, "ultimate_adr",
        value = config$divorce$ultimate_adr %||% 1700)

      updateNumericInput(session, "fertility_ultimate_year",
        value = config$fertility$ultimate_year %||% 2050)

      updateNumericInput(session, "weight_exponent",
        value = config$fertility$weight_exponent %||% 1.5)

      updateNumericInput(session, "mortality_ultimate_year",
        value = config$mortality$ultimate_year %||% 2043)

      updateSelectInput(session, "starting_aax_method",
        selected = config$mortality$starting_aax_method %||% "tr_qx")

      updateSelectInput(session, "distribution_method",
        selected = config$immigration$lpr$distribution_method %||% "tr_derived")

      updateSliderInput(session, "emigration_ratio",
        value = config$immigration$emigration$ratio %||% 0.25)

      updateCheckboxInput(session, "use_tr_historical_pop",
        value = config$projected_population$use_tr_historical_population %||% TRUE)

      # Check if TFR constraint is enabled
      has_tfr_constraints <- !is.null(config$fertility$constrain_tfr) &&
                             length(config$fertility$constrain_tfr) > 0
      updateCheckboxInput(session, "constrain_tfr", value = has_tfr_constraints)

      # Check for TR2025 births
      has_tr_births <- !is.null(config$fertility$use_tr2025_births_for_years) &&
                       length(config$fertility$use_tr2025_births_for_years) > 0
      updateCheckboxInput(session, "use_tr2025_births", value = has_tr_births)

    }) |> bindEvent(rv$config, ignoreNULL = TRUE)

    # Use All TR2025 button
    observeEvent(input$use_all_tr, {
      updateCheckboxInput(session, "use_tr_historical_pop", value = TRUE)
      updateCheckboxInput(session, "use_tr2025_births", value = TRUE)
      updateSelectInput(session, "starting_aax_method", selected = "tr_qx")
      updateSelectInput(session, "distribution_method", selected = "tr_derived")
    })

    # Use All ARTEMIS button
    observeEvent(input$use_all_artemis, {
      updateCheckboxInput(session, "use_tr_historical_pop", value = FALSE)
      updateCheckboxInput(session, "use_tr2025_births", value = FALSE)
      updateSelectInput(session, "starting_aax_method", selected = "regression")
      updateSelectInput(session, "distribution_method", selected = "dhs")
    })

    # Build modified config when Apply is clicked
    observeEvent(input$apply_config, {
      req(rv$config)

      # Start with baseline config
      config <- rv$config

      # Apply modifications
      config$fertility$ultimate_ctfr <- input$ultimate_tfr
      config$fertility$ultimate_year <- input$fertility_ultimate_year
      config$fertility$weight_exponent <- input$weight_exponent

      if (input$constrain_tfr) {
        config$fertility$constrain_tfr <- list("2023" = 1.62, "2024" = 1.62)
      } else {
        config$fertility$constrain_tfr <- NULL
      }

      if (input$use_tr2025_births) {
        config$fertility$use_tr2025_births_for_years <- c(2023, 2024)
      } else {
        config$fertility$use_tr2025_births_for_years <- NULL
      }

      config$mortality$ultimate_year <- input$mortality_ultimate_year
      config$mortality$starting_aax_method <- input$starting_aax_method

      if (input$starting_aax_method == "capped") {
        config$mortality$starting_aax_cap$multiplier <- input$aax_cap_multiplier
      }

      config$immigration$va2_alternative <- input$immigration_scenario
      config$immigration$lpr$distribution_method <- input$distribution_method
      config$immigration$emigration$ratio <- input$emigration_ratio

      config$projected_population$use_tr_historical_population <- input$use_tr_historical_pop

      # Store modified config
      modified_config(config)

      showNotification(
        "Configuration updated. Click 'Run Projection' to generate new results.",
        type = "message"
      )
    })

    # Reset to TR2025
    observeEvent(input$reset_config, {
      tryCatch({
        rv$config <- load_config()
        modified_config(NULL)
        showNotification("Reset to TR2025 baseline", type = "message")
      }, error = function(e) {
        showNotification(paste("Reset failed:", e$message), type = "error")
      })
    })

    # Export as YAML
    observeEvent(input$export_config, {
      config <- modified_config() %||% rv$config

      if (is.null(config)) {
        showNotification("No configuration to export", type = "warning")
        return()
      }

      # Create download
      showModal(modalDialog(
        title = "Export Configuration",
        textAreaInput(
          session$ns("yaml_output"),
          "YAML Configuration",
          value = yaml::as.yaml(config),
          rows = 20,
          width = "100%"
        ),
        footer = tagList(
          downloadButton(session$ns("download_yaml"), "Download", class = "btn-primary"),
          modalButton("Close")
        ),
        size = "l"
      ))
    })

    # Download handler
    output$download_yaml <- downloadHandler(
      filename = function() {
        paste0("artemis_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yaml")
      },
      content = function(file) {
        config <- modified_config() %||% rv$config
        yaml::write_yaml(config, file)
      }
    )

    # Return modified config
    return(reactive({
      modified_config()
    }))
  })
}
