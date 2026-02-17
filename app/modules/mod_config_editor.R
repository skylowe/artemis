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
        helpText(
          "Regression uses NCHS-derived mortality for 1980+",
          "and SSA/TR2025 historical data for pre-1980 years.",
          "TR2025 Official uses SSA data for all years."
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
        ),

        selectInput(
          ns("o_population_method"),
          "O-Population Method",
          choices = c(
            "Residual + V.A2 levels (default)" = "residual",
            "V.A2 flows only" = "va2_flows"
          ),
          selected = "residual"
        ),

        selectInput(
          ns("net_o_source"),
          "Net O Immigration Source",
          choices = c(
            "V.A2 Totals (Official)" = "va2",
            "ARTEMIS Computed" = "artemis"
          ),
          selected = "va2"
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

        selectInput(
          ns("historical_pop_source"),
          "Historical Population Source",
          choices = c(
            "ARTEMIS (Census all ages)" = "census",
            "ARTEMIS Hybrid (Census 0-84, SSA 85+)" = "hybrid",
            "SSA TR2025 (all ages)" = "ssa"
          ),
          selected = "hybrid"
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
        "Reset to Baseline",
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

    # Sync all UI inputs to match a config list
    sync_inputs_to_config <- function(config) {
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
        selected = config$mortality$starting_aax_method)

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

      updateSelectInput(session, "historical_pop_source",
        selected = config$historical_population$population_source %||% "hybrid")

      updateSelectInput(session, "o_population_method",
        selected = config$historical_population$o_population$method %||% "residual")

      updateSelectInput(session, "net_o_source",
        selected = config$projected_population$net_o_source %||% "va2")

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
    }

    # Initialize from baseline config
    observe({
      req(rv$config)
      sync_inputs_to_config(rv$config)
    })

    # Helper: update a nested config value only if it actually changed.
    # Coerces Shiny input types (double from numericInput/sliderInput) to match
    # the original yaml::read_yaml types (integer), preserving identical hashes
    # for unchanged domain config sections (enables targets early cutoff).
    set_config <- function(config, path, new_val) {
      # Get original value by traversing path
      orig <- config
      for (key in path) {
        if (is.null(orig)) { orig <- NULL; break }
        orig <- orig[[key]]
      }

      # Coerce type to match original
      if (!is.null(orig) && !is.null(new_val)) {
        if (is.integer(orig) && is.numeric(new_val)) {
          new_val <- as.integer(new_val)
        }
      }

      # Skip if unchanged
      if (identical(new_val, orig)) return(config)

      # Ensure parent lists exist for 3-level paths
      if (length(path) == 3 && is.null(config[[path[1]]][[path[2]]])) {
        config[[path[1]]][[path[2]]] <- list()
      }

      # Set value
      if (length(path) == 2) {
        config[[path[1]]][[path[2]]] <- new_val
      } else if (length(path) == 3) {
        config[[path[1]]][[path[2]]][[path[3]]] <- new_val
      }

      config
    }

    # Build modified config when Apply is clicked
    observeEvent(input$apply_config, {
      req(rv$config)

      # Start with baseline config — only fields that actually changed
      # get written back, preserving original types for unchanged sections
      config <- rv$config

      # Fertility
      config <- set_config(config, c("fertility", "ultimate_ctfr"), input$ultimate_tfr)
      config <- set_config(config, c("fertility", "ultimate_year"), input$fertility_ultimate_year)
      config <- set_config(config, c("fertility", "weight_exponent"), input$weight_exponent)

      # Custom recent TFR (special case: list or NULL)
      if (input$custom_recent_tfr) {
        tr_year <- config$metadata$trustees_report_year %||% 2025
        new_custom_tfr <- stats::setNames(
          as.list(c(input$custom_tfr_year1, input$custom_tfr_year2)),
          as.character(c(tr_year - 2, tr_year - 1))
        )
        if (!identical(new_custom_tfr, config$fertility$custom_recent_tfr)) {
          config$fertility$custom_recent_tfr <- new_custom_tfr
        }
      } else if (!is.null(config$fertility$custom_recent_tfr)) {
        config$fertility$custom_recent_tfr <- NULL
      }

      # Mortality
      config <- set_config(config, c("mortality", "ultimate_year"), input$mortality_ultimate_year)
      config <- set_config(config, c("mortality", "starting_aax_method"), input$starting_aax_method)
      config <- set_config(config, c("mortality", "apply_covid_adjustments"), input$apply_covid_adjustments)
      config <- set_config(config, c("mortality", "use_wonder_provisional"), input$use_wonder_provisional)
      config <- set_config(config, c("mortality", "hmd_calibration", "enabled"), input$hmd_calibration_enabled)
      config <- set_config(config, c("mortality", "elderly_aax_cap", "enabled"), input$elderly_aax_cap_enabled)
      config <- set_config(config, c("mortality", "elderly_aax_cap", "max_aax"), input$elderly_aax_cap_value)

      # Immigration
      config <- set_config(config, c("immigration", "va2_alternative"), input$immigration_scenario)
      config <- set_config(config, c("immigration", "lpr", "assumptions_source"), input$lpr_assumptions_source)
      config <- set_config(config, c("immigration", "lpr", "distribution_method"), input$distribution_method)
      config <- set_config(config, c("immigration", "emigration", "ratio"), input$emigration_ratio)

      # Historical Population
      config <- set_config(config, c("historical_population", "population_source"), input$historical_pop_source)
      config <- set_config(config, c("projected_population", "use_tr_historical_population"),
                           input$historical_pop_source == "ssa")
      config <- set_config(config, c("historical_population", "o_population", "method"), input$o_population_method)
      config <- set_config(config, c("projected_population", "net_o_source"), input$net_o_source)

      # Store modified config
      modified_config(config)

      showNotification(
        "Configuration updated. Click 'Run Projection' to generate new results.",
        type = "message"
      )
    })

    # Reset to baseline
    observeEvent(input$reset_config, {
      tryCatch({
        config <- load_config()
        rv$config <- config
        modified_config(NULL)

        # Directly update all inputs (don't rely on observer reactivity,
        # since rv$config content may be identical to current value)
        sync_inputs_to_config(config)

        showNotification("Reset to baseline", type = "message")
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
