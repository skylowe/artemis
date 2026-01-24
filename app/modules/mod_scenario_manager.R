# Scenario Manager Module
# =============================================================================
# Handles scenario saving, loading, and execution

#' Scenario Manager UI
#' @param id Module namespace ID
mod_scenario_manager_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    width = 1,
    heights_equal = "row",

    # Scenario execution card
    card(
      card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        "Run Projection",
        actionButton(
          ns("run_projection"),
          "Run",
          icon = icon("play"),
          class = "btn-light btn-sm"
        )
      ),
      card_body(
        # Status indicator
        uiOutput(ns("run_status")),

        # Progress indicator
        conditionalPanel(
          condition = sprintf("input['%s'] > 0", ns("run_projection")),
          div(
            id = ns("progress_container"),
            if (requireNamespace("shinyWidgets", quietly = TRUE)) {
              shinyWidgets::progressBar(
                id = ns("projection_progress"),
                value = 0,
                status = "primary",
                display_pct = TRUE,
                striped = TRUE
              )
            } else {
              tags$div(
                class = "progress",
                tags$div(
                  id = ns("projection_progress"),
                  class = "progress-bar progress-bar-striped",
                  role = "progressbar",
                  style = "width: 0%"
                )
              )
            }
          )
        ),

        hr(),

        # Scenario saving
        h6("Save Scenario"),
        textInput(
          ns("scenario_name"),
          NULL,
          placeholder = "Scenario name..."
        ),
        textAreaInput(
          ns("scenario_description"),
          NULL,
          placeholder = "Description (optional)...",
          rows = 2
        ),
        actionButton(
          ns("save_scenario"),
          "Save Scenario",
          icon = icon("save"),
          class = "btn-outline-success w-100"
        )
      )
    ),

    # Saved scenarios card
    card(
      card_header(class = "bg-secondary text-white", "Saved Scenarios"),
      card_body(
        DTOutput(ns("scenarios_table"), height = "300px")
      ),
      card_footer(
        div(
          class = "d-flex gap-2",
          actionButton(
            ns("load_scenario"),
            "Load",
            icon = icon("folder-open"),
            class = "btn-outline-primary btn-sm"
          ),
          actionButton(
            ns("delete_scenario"),
            "Delete",
            icon = icon("trash"),
            class = "btn-outline-danger btn-sm"
          ),
          actionButton(
            ns("duplicate_scenario"),
            "Duplicate",
            icon = icon("copy"),
            class = "btn-outline-secondary btn-sm"
          )
        )
      )
    ),

    # Comparison selection
    card(
      card_header(class = "bg-info text-white", "Scenario Comparison"),
      card_body(
        checkboxGroupInput(
          ns("compare_scenarios"),
          "Select scenarios to compare:",
          choices = c("TR2025 Baseline" = "baseline"),
          selected = "baseline"
        ),
        actionButton(
          ns("view_comparison"),
          "View Comparison",
          icon = icon("exchange-alt"),
          class = "btn-info w-100"
        )
      )
    )
  )
}

#' Scenario Manager Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
#' @param config_result Reactive with modified config from config editor
#' @param parent_session Parent Shiny session for tab navigation
mod_scenario_manager_server <- function(id, rv, config_result, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {

    # Track projection state
    projection_state <- reactiveValues(
      running = FALSE,
      progress = 0,
      message = "",
      last_result = NULL
    )

    # Render run status
    output$run_status <- renderUI({
      if (projection_state$running) {
        div(
          class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          " Running projection...",
          tags$small(class = "d-block", projection_state$message)
        )
      } else if (!is.null(projection_state$last_result)) {
        if (projection_state$last_result$success) {
          div(
            class = "alert alert-success",
            icon("check-circle"),
            " Projection complete",
            tags$small(
              class = "d-block",
              sprintf("Population: %.1fM (2050), %.1fM (2099)",
                      projection_state$last_result$pop_2050 / 1e6,
                      projection_state$last_result$pop_2099 / 1e6)
            )
          )
        } else {
          div(
            class = "alert alert-danger",
            icon("exclamation-circle"),
            " Projection failed",
            tags$small(class = "d-block", projection_state$last_result$error)
          )
        }
      } else {
        div(
          class = "alert alert-secondary",
          icon("info-circle"),
          " Configure assumptions and click Run to generate a projection"
        )
      }
    })

    # Run projection
    observeEvent(input$run_projection, {
      config <- config_result()

      if (is.null(config)) {
        showNotification(
          "No configuration changes to run. Modify parameters first.",
          type = "warning"
        )
        return()
      }

      # Show waiter
      waiter <- Waiter$new(
        id = session$ns("run_status"),
        html = tagList(
          spin_fading_circles(),
          h5("Running projection pipeline...")
        )
      )
      waiter$show()

      projection_state$running <- TRUE
      projection_state$progress <- 0
      projection_state$message <- "Initializing..."

      # Run projection
      tryCatch({
        result <- run_scenario_projection(
          config = config,
          artemis_root = ARTEMIS_ROOT,
          progress_callback = function(pct, msg) {
            projection_state$progress <- pct
            projection_state$message <- msg
            updateProgressBar(
              session = session,
              id = "projection_progress",
              value = pct,
              status = if (pct < 100) "primary" else "success"
            )
          }
        )

        projection_state$last_result <- result
        projection_state$running <- FALSE

        if (result$success) {
          rv$active_data <- result$data
          showNotification("Projection complete!", type = "message")
        }

      }, error = function(e) {
        projection_state$last_result <- list(
          success = FALSE,
          error = e$message
        )
        projection_state$running <- FALSE
        showNotification(paste("Error:", e$message), type = "error")
      })

      waiter$hide()
    })

    # Scenarios table
    output$scenarios_table <- renderDT({
      scenarios <- rv$scenarios

      if (length(scenarios) == 0) {
        return(data.frame(
          Name = character(),
          Created = character(),
          Description = character()
        ))
      }

      df <- data.frame(
        Name = names(scenarios),
        Created = sapply(scenarios, function(s) {
          format(s$metadata$created, "%Y-%m-%d %H:%M")
        }),
        Description = sapply(scenarios, function(s) {
          s$metadata$description %||% ""
        }),
        stringsAsFactors = FALSE
      )

      datatable(
        df,
        selection = "single",
        options = list(
          pageLength = 5,
          dom = "tp",
          scrollY = "200px"
        ),
        rownames = FALSE
      )
    })

    # Save scenario
    observeEvent(input$save_scenario, {
      name <- trimws(input$scenario_name)

      if (name == "") {
        showNotification("Please enter a scenario name", type = "warning")
        return()
      }

      if (is.null(rv$active_data) || length(rv$active_data) == 0) {
        showNotification("No data to save. Run a projection first.", type = "warning")
        return()
      }

      # Create scenario object
      scenario <- list(
        metadata = list(
          name = name,
          description = input$scenario_description,
          created = Sys.time(),
          baseline = "tr2025_intermediate"
        ),
        config = config_result() %||% rv$config,
        results = rv$active_data,
        checksum = digest::digest(rv$active_data)
      )

      # Save to disk
      filename <- paste0(gsub("[^a-zA-Z0-9_-]", "_", name), ".rds")
      filepath <- file.path(SCENARIOS_DIR, filename)
      saveRDS(scenario, filepath)

      # Update reactive values
      rv$scenarios[[name]] <- scenario

      # Update comparison choices
      updateCheckboxGroupInput(
        session, "compare_scenarios",
        choices = c(
          "TR2025 Baseline" = "baseline",
          setNames(names(rv$scenarios), names(rv$scenarios))
        ),
        selected = input$compare_scenarios
      )

      # Clear inputs
      updateTextInput(session, "scenario_name", value = "")
      updateTextAreaInput(session, "scenario_description", value = "")

      showNotification(paste("Scenario saved:", name), type = "message")
    })

    # Load scenario
    observeEvent(input$load_scenario, {
      selected <- input$scenarios_table_rows_selected

      if (length(selected) == 0) {
        showNotification("Please select a scenario to load", type = "warning")
        return()
      }

      name <- names(rv$scenarios)[selected]
      scenario <- rv$scenarios[[name]]

      if (!is.null(scenario$results)) {
        rv$active_data <- scenario$results
        showNotification(paste("Loaded scenario:", name), type = "message")
      }
    })

    # Delete scenario
    observeEvent(input$delete_scenario, {
      selected <- input$scenarios_table_rows_selected

      if (length(selected) == 0) {
        showNotification("Please select a scenario to delete", type = "warning")
        return()
      }

      name <- names(rv$scenarios)[selected]

      showModal(modalDialog(
        title = "Confirm Delete",
        p("Are you sure you want to delete scenario '", name, "'?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_delete"), "Delete",
                       class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_delete, {
      selected <- input$scenarios_table_rows_selected
      name <- names(rv$scenarios)[selected]

      # Remove from disk
      filename <- paste0(gsub("[^a-zA-Z0-9_-]", "_", name), ".rds")
      filepath <- file.path(SCENARIOS_DIR, filename)
      if (file.exists(filepath)) {
        file.remove(filepath)
      }

      # Remove from reactive values
      rv$scenarios[[name]] <- NULL

      # Update comparison choices
      updateCheckboxGroupInput(
        session, "compare_scenarios",
        choices = c(
          "TR2025 Baseline" = "baseline",
          setNames(names(rv$scenarios), names(rv$scenarios))
        )
      )

      removeModal()
      showNotification(paste("Deleted scenario:", name), type = "message")
    })

    # Duplicate scenario
    observeEvent(input$duplicate_scenario, {
      selected <- input$scenarios_table_rows_selected

      if (length(selected) == 0) {
        showNotification("Please select a scenario to duplicate", type = "warning")
        return()
      }

      name <- names(rv$scenarios)[selected]
      scenario <- rv$scenarios[[name]]

      # Create new name
      new_name <- paste0(name, "_copy")
      i <- 1
      while (new_name %in% names(rv$scenarios)) {
        new_name <- paste0(name, "_copy_", i)
        i <- i + 1
      }

      # Create copy
      new_scenario <- scenario
      new_scenario$metadata$name <- new_name
      new_scenario$metadata$created <- Sys.time()

      # Save
      filename <- paste0(gsub("[^a-zA-Z0-9_-]", "_", new_name), ".rds")
      filepath <- file.path(SCENARIOS_DIR, filename)
      saveRDS(new_scenario, filepath)

      rv$scenarios[[new_name]] <- new_scenario

      showNotification(paste("Created copy:", new_name), type = "message")
    })

    # View comparison
    observeEvent(input$view_comparison, {
      selected <- input$compare_scenarios

      if (length(selected) < 2) {
        showNotification("Select at least 2 scenarios to compare", type = "warning")
        return()
      }

      # Navigate to comparison tab
      if (!is.null(parent_session)) {
        updateTabsetPanel(parent_session, "main_navbar", selected = "Compare")
      }
    })
  })
}
