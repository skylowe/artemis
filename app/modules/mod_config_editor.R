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
          ns("custom_recent_tfr"),
          "Override Recent TFR (TR year -1/-2)",
          value = FALSE
        ),

        conditionalPanel(
          condition = sprintf("input['%s']", ns("custom_recent_tfr")),
          numericInput(
            ns("custom_tfr_year1"),
            "TFR (2 years prior)",
            value = 1.62,
            min = 0.5, max = 4.0,
            step = 0.01
          ),
          numericInput(
            ns("custom_tfr_year2"),
            "TFR (1 year prior)",
            value = 1.62,
            min = 0.5, max = 4.0,
            step = 0.01
          )
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
          "Mortality Projection Source",
          choices = c(
            "ARTEMIS (Regression)" = "regression",
            "TR2025 Official" = "tr_qx"
          ),
          selected = "regression"
        ),

        checkboxInput(
          ns("apply_covid_adjustments"),
          "Apply COVID-19 Adjustments",
          value = TRUE
        ),

        checkboxInput(
          ns("use_wonder_provisional"),
          "Use CDC WONDER Provisional Data",
          value = TRUE
        ),

        checkboxInput(
          ns("hmd_calibration_enabled"),
          "Apply HMD Calibration for Ages 85+",
          value = TRUE
        ),

        checkboxInput(
          ns("elderly_aax_cap_enabled"),
          "Cap AAx at Ages 85+ (~0.55%)",
          value = FALSE
        ),

        conditionalPanel(
          condition = sprintf("input['%s']", ns("elderly_aax_cap_enabled")),
          sliderInput(
            ns("elderly_aax_cap_value"),
            "Max AAx (Ages 85+)",
            min = 0.003, max = 0.010,
            value = 0.0055,
            step = 0.0005,
            post = " /yr"
          )
        )
      ),

      accordion_panel(
        title = "Immigration Options",
        icon = icon("plane"),

        selectInput(
          ns("lpr_assumptions_source"),
          "LPR Assumptions Source",
          choices = c(
            "TR2025 V.A2 (Official)" = "va2",
            "CBO Demographic Outlook" = "cbo"
          ),
          selected = "va2"
        ),

        selectInput(
          ns("distribution_method"),
          "Age-Sex Distribution",
          choices = c(
            "TR2025-derived" = "tr_derived",
            "DHS Historical" = "dhs"
          ),
          selected = "tr_derived"
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'va2'", ns("lpr_assumptions_source")),
          sliderInput(
            ns("emigration_ratio"),
            "Emigration Ratio",
            min = 0.10, max = 0.50,
            value = 0.25,
            step = 0.01
          )
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
          value = FALSE
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

      updateCheckboxInput(session, "apply_covid_adjustments",
        value = config$mortality$apply_covid_adjustments %||% TRUE)

      updateCheckboxInput(session, "use_wonder_provisional",
        value = config$mortality$use_wonder_provisional %||% TRUE)

      updateCheckboxInput(session, "hmd_calibration_enabled",
        value = config$mortality$hmd_calibration$enabled %||% TRUE)

      updateCheckboxInput(session, "elderly_aax_cap_enabled",
        value = config$mortality$elderly_aax_cap$enabled %||% FALSE)

      updateSliderInput(session, "elderly_aax_cap_value",
        value = config$mortality$elderly_aax_cap$max_aax %||% 0.0055)

      updateSelectInput(session, "lpr_assumptions_source",
        selected = config$immigration$lpr$assumptions_source %||% "va2")

      updateSelectInput(session, "distribution_method",
        selected = config$immigration$lpr$distribution_method %||% "tr_derived")

      updateSliderInput(session, "emigration_ratio",
        value = config$immigration$emigration$ratio %||% 0.25)

      updateCheckboxInput(session, "use_tr_historical_pop",
        value = config$projected_population$use_tr_historical_population %||% FALSE)

      # Check if custom recent TFR is enabled and populate values
      has_custom_tfr <- !is.null(config$fertility$custom_recent_tfr) &&
                        length(config$fertility$custom_recent_tfr) > 0
      updateCheckboxInput(session, "custom_recent_tfr", value = has_custom_tfr)

      if (has_custom_tfr) {
        tfr_values <- unlist(config$fertility$custom_recent_tfr)
        if (length(tfr_values) >= 1) {
          updateNumericInput(session, "custom_tfr_year1", value = tfr_values[1])
        }
        if (length(tfr_values) >= 2) {
          updateNumericInput(session, "custom_tfr_year2", value = tfr_values[2])
        }
      }

    }) |> bindEvent(rv$config, ignoreNULL = TRUE)

    # Use All TR2025 button
    observeEvent(input$use_all_tr, {
      updateCheckboxInput(session, "use_tr_historical_pop", value = TRUE)
      updateCheckboxInput(session, "custom_recent_tfr", value = TRUE)
      updateSelectInput(session, "starting_aax_method", selected = "tr_qx")
      updateSelectInput(session, "lpr_assumptions_source", selected = "va2")
      updateSelectInput(session, "distribution_method", selected = "tr_derived")
    })

    # Use All ARTEMIS button
    observeEvent(input$use_all_artemis, {
      updateCheckboxInput(session, "use_tr_historical_pop", value = FALSE)
      updateCheckboxInput(session, "custom_recent_tfr", value = FALSE)
      updateSelectInput(session, "starting_aax_method", selected = "regression")
      updateSelectInput(session, "lpr_assumptions_source", selected = "va2")
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

      if (input$custom_recent_tfr) {
        tr_year <- config$metadata$trustees_report_year %||% 2025
        config$fertility$custom_recent_tfr <- stats::setNames(
          as.list(c(input$custom_tfr_year1, input$custom_tfr_year2)),
          as.character(c(tr_year - 2, tr_year - 1))
        )
      } else {
        config$fertility$custom_recent_tfr <- NULL
      }

      config$mortality$ultimate_year <- input$mortality_ultimate_year
      config$mortality$starting_aax_method <- input$starting_aax_method
      config$mortality$apply_covid_adjustments <- input$apply_covid_adjustments
      config$mortality$use_wonder_provisional <- input$use_wonder_provisional

      if (is.null(config$mortality$hmd_calibration)) {
        config$mortality$hmd_calibration <- list()
      }
      config$mortality$hmd_calibration$enabled <- input$hmd_calibration_enabled

      if (is.null(config$mortality$elderly_aax_cap)) {
        config$mortality$elderly_aax_cap <- list()
      }
      config$mortality$elderly_aax_cap$enabled <- input$elderly_aax_cap_enabled
      config$mortality$elderly_aax_cap$max_aax <- input$elderly_aax_cap_value

      config$immigration$va2_alternative <- input$immigration_scenario
      config$immigration$lpr$assumptions_source <- input$lpr_assumptions_source
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
