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
        # Status and progress indicator
        uiOutput(ns("run_status")),

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

    # Console output card
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Console Output",
        actionButton(
          ns("clear_console"),
          "Clear",
          icon = icon("eraser"),
          class = "btn-outline-secondary btn-sm"
        )
      ),
      card_body(
        class = "p-0",
        tags$pre(
          id = ns("console_output"),
          class = "scenario-console"
        )
      )
    ),

    # Saved scenarios card
    card(
      card_header(class = "bg-secondary text-white", "Saved Scenarios"),
      card_body(
        DTOutput(ns("scenarios_table"), height = "400px")
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
      # Initial state — no projection has been run yet
      if (!projection_state$running && is.null(projection_state$last_result)) {
        return(div(
          class = "alert alert-secondary",
          icon("info-circle"),
          " Configure assumptions and click Run to generate a projection"
        ))
      }

      if (projection_state$running) {
        div(class = "alert alert-info mb-0 py-2",
            icon("cog", class = "fa-spin"),
            strong(" Projection in progress"))
      } else if (projection_state$last_result$success) {
        div(class = "alert alert-success mb-0 py-2",
            icon("check-circle"),
            strong(" Projection complete"),
            tags$small(
              class = "d-block mt-1",
              sprintf("Population: %.1fM (2050), %.1fM (2099)",
                      projection_state$last_result$pop_2050 / 1e6,
                      projection_state$last_result$pop_2099 / 1e6)
            ))
      } else {
        div(class = "alert alert-danger mb-0 py-2",
            icon("exclamation-circle"),
            strong(" Projection failed"),
            tags$small(class = "d-block mt-1",
                       projection_state$last_result$error))
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

      projection_state$running <- TRUE
      projection_state$progress <- 0
      projection_state$message <- "Running projection pipeline..."

      # Show immediate notification (bypasses reactive cycle — sends via WebSocket)
      showNotification(
        tagList(icon("cog", class = "fa-spin"), " Running projection pipeline..."),
        duration = NULL,
        type = "message",
        id = session$ns("projection_running")
      )

      # Clear console and prepare for new run
      console_id <- session$ns("console_output")
      session$sendCustomMessage("scenario_console_clear", list(id = console_id))

      # Helper to push a line to the browser console
      console_log <- function(text) {
        tryCatch(
          session$sendCustomMessage("scenario_console_log",
                                    list(id = console_id, text = text)),
          error = function(e) NULL
        )
      }

      console_log("Starting projection pipeline...\n")
      run_start <- Sys.time()

      # Run synchronously with withProgress() to keep the WebSocket alive
      # withCallingHandlers captures cli/message output and pushes to browser
      result <- NULL
      withProgress(message = "Running projection pipeline...", value = 0, {
        result <- tryCatch(
          withCallingHandlers(
            run_scenario_projection(
              config = config,
              artemis_root = ARTEMIS_ROOT,
              progress_callback = function(pct, msg) {
                setProgress(pct / 100, message = msg)
              }
            ),
            message = function(m) {
              tryCatch({
                line <- cli::ansi_strip(conditionMessage(m))
                console_log(line)
              }, error = function(e) NULL)
              invokeRestart("muffleMessage")
            }
          ),
          error = function(e) {
            console_log(paste0("Error: ", e$message, "\n"))
            list(success = FALSE, error = e$message, data = NULL)
          }
        )
      })

      # Summary line
      elapsed <- as.numeric(difftime(Sys.time(), run_start, units = "secs"))
      if (!is.null(result) && result$success) {
        console_log(sprintf("\nProjection complete [%.1f seconds]\n", elapsed))
      } else {
        console_log(sprintf("\nProjection failed [%.1f seconds]\n", elapsed))
      }

      # Remove the running notification
      removeNotification(session$ns("projection_running"))

      projection_state$last_result <- result
      projection_state$running <- FALSE

      if (!is.null(result) && result$success) {
        rv$active_data <- result$data
        showNotification("Projection complete", type = "message")
      } else {
        showNotification(
          paste("Projection failed:", result$error %||% "Unknown error"),
          type = "error"
        )
      }
    })

    # Clear console button
    observeEvent(input$clear_console, {
      session$sendCustomMessage("scenario_console_clear",
                                list(id = session$ns("console_output")))
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

  })
}
