# Immigration Visualization Module
# =============================================================================
# Net immigration trends and components

#' Immigration Visualization UI
#' @param id Module namespace ID
mod_immigration_viz_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 250,

      selectInput(
        ns("chart_type"),
        "Chart Type",
        choices = c(
          "Net Immigration Trend" = "trend",
          "Components (LPR vs O)" = "components",
          "Age Distribution" = "age_dist",
          "By Sex" = "by_sex"
        ),
        selected = "trend"
      ),

      hr(),

      sliderInput(
        ns("year_range"),
        "Year Range",
        min = MIN_YEAR, max = MAX_YEAR,
        value = c(MIN_YEAR, MAX_YEAR),
        step = 1,
        sep = ""
      ),

      hr(),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'age_dist'", ns("chart_type")),
        sliderInput(
          ns("age_dist_year"),
          "Year",
          min = MIN_YEAR + 1, max = MAX_YEAR,
          value = MID_YEAR,
          step = 1,
          sep = "",
          animate = TRUE
        ),
        radioButtons(
          ns("age_grouping"),
          "Age Grouping",
          choices = c(
            "5-year" = "five",
            "Single year" = "single"
          ),
          selected = "five"
        )
      ),

      hr(),

      checkboxInput(
        ns("show_stacked"),
        "Stacked area chart",
        value = TRUE
      )
    ),

    # Main content
    layout_column_wrap(
      width = 1,

      card(
        card_header("Immigration Trends"),
        card_body(
          plotlyOutput(ns("main_chart"), height = "550px")
        )
      ),

      layout_column_wrap(
        width = 1/3,

        value_box(
          title = paste0("Avg Annual (2025-2030)"),
          value = textOutput(ns("avg_early")),
          theme = "success"
        ),

        value_box(
          title = paste0("Avg Annual (2031-", MID_YEAR, ")"),
          value = textOutput(ns("avg_mid")),
          theme = "info"
        ),

        value_box(
          title = paste0("Avg Annual (", MID_YEAR + 1, "-", MAX_YEAR, ")"),
          value = textOutput(ns("avg_late")),
          theme = "secondary"
        )
      )
    )
  )
}

#' Immigration Visualization Server
#' @param id Module namespace ID
#' @param rv Reactive values from parent
mod_immigration_viz_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Main chart
    output$main_chart <- renderPlotly({
      req(rv$active_data$projected_net_immigration)

      imm <- rv$active_data$projected_net_immigration
      years <- seq(input$year_range[1], input$year_range[2])
      imm <- imm[year %in% years]

      chart_type <- input$chart_type

      if (chart_type == "trend") {
        # Simple net immigration trend
        imm_total <- imm[, .(net_immigration = sum(net_immigration, na.rm = TRUE)),
                         by = year]

        p <- ggplot(imm_total, aes(x = year, y = net_immigration / 1e6)) +
          geom_line(linewidth = 1.2, color = "#27AE60") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#7F8C8D") +
          labs(x = NULL, y = "Net Immigration (millions)") +
          theme_artemis()

        ggplotly(p) |> layout_artemis()

      } else if (chart_type == "components") {
        # Try to get LPR and O components separately
        # If not available, show total only
        lpr <- tryCatch({
          rv$active_data$net_lpr_immigration
        }, error = function(e) NULL)

        net_o <- tryCatch({
          rv$active_data$net_o_for_projection
        }, error = function(e) NULL)

        if (!is.null(lpr) && !is.null(net_o)) {
          lpr_agg <- lpr[year %in% years,
                         .(value = sum(net_immigration, na.rm = TRUE), type = "Net LPR"),
                         by = year]
          o_agg <- net_o[year %in% years,
                          .(value = sum(net_o, na.rm = TRUE), type = "Net O"),
                          by = year]

          components <- rbindlist(list(lpr_agg, o_agg))

          if (input$show_stacked) {
            p <- ggplot(components, aes(x = year, y = value / 1e6, fill = type)) +
              geom_area(alpha = 0.7) +
              scale_fill_manual(values = c("Net LPR" = "#27AE60", "Net O" = "#3498DB")) +
              labs(x = NULL, y = "Net Immigration (millions)", fill = NULL) +
              theme_artemis()
          } else {
            p <- ggplot(components, aes(x = year, y = value / 1e6, color = type)) +
              geom_line(linewidth = 1) +
              scale_color_manual(values = c("Net LPR" = "#27AE60", "Net O" = "#3498DB")) +
              labs(x = NULL, y = "Net Immigration (millions)", color = NULL) +
              theme_artemis()
          }

          ggplotly(p) |> layout_artemis()

        } else {
          # Fallback to total only
          imm_total <- imm[, .(net_immigration = sum(net_immigration, na.rm = TRUE)),
                           by = year]

          p <- ggplot(imm_total, aes(x = year, y = net_immigration / 1e6)) +
            geom_line(linewidth = 1.2, color = "#27AE60") +
            labs(x = NULL, y = "Net Immigration (millions)") +
            annotate("text", x = mean(years), y = max(imm_total$net_immigration) / 1e6 * 0.9,
                     label = "Component breakdown not available",
                     color = "#7F8C8D") +
            theme_artemis()

          ggplotly(p) |> layout_artemis()
        }

      } else if (chart_type == "age_dist") {
        # Age distribution for selected year
        year_data <- imm[year == input$age_dist_year]

        if (input$age_grouping == "five") {
          year_data[, age_group := cut(
            age,
            breaks = c(seq(0, 100, 5), Inf),
            labels = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+"),
            right = FALSE
          )]
          year_data <- year_data[, .(net_immigration = sum(net_immigration, na.rm = TRUE)),
                                  by = .(age_group, sex)]
        } else {
          year_data[, age_group := as.character(age)]
          year_data <- year_data[, .(net_immigration = sum(net_immigration, na.rm = TRUE)),
                                  by = .(age_group, sex)]
        }

        p <- ggplot(year_data, aes(x = age_group, y = net_immigration / 1000, fill = sex)) +
          geom_col(position = "dodge") +
          scale_fill_manual(values = SEX_COLORS) +
          labs(x = "Age", y = "Net Immigration (thousands)", fill = NULL,
               title = paste("Net Immigration Distribution -", input$age_dist_year)) +
          theme_artemis() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        ggplotly(p) |> layout_artemis()

      } else if (chart_type == "by_sex") {
        # By sex over time
        imm_by_sex <- imm[, .(net_immigration = sum(net_immigration, na.rm = TRUE)),
                          by = .(year, sex)]

        p <- ggplot(imm_by_sex, aes(x = year, y = net_immigration / 1e6, color = sex)) +
          geom_line(linewidth = 1) +
          scale_color_manual(values = SEX_COLORS) +
          labs(x = NULL, y = "Net Immigration (millions)", color = NULL) +
          theme_artemis()

        ggplotly(p) |> layout_artemis()
      }
    })

    # Value boxes
    output$avg_early <- renderText({
      req(rv$active_data$projected_net_immigration)
      imm <- rv$active_data$projected_net_immigration
      avg <- imm[year >= 2025 & year <= 2030,
                 sum(net_immigration, na.rm = TRUE) / length(unique(year))]
      format_number(avg)
    })

    output$avg_mid <- renderText({
      req(rv$active_data$projected_net_immigration)
      imm <- rv$active_data$projected_net_immigration
      avg <- imm[year >= 2031 & year <= MID_YEAR,
                 sum(net_immigration, na.rm = TRUE) / length(unique(year))]
      format_number(avg)
    })

    output$avg_late <- renderText({
      req(rv$active_data$projected_net_immigration)
      imm <- rv$active_data$projected_net_immigration
      avg <- imm[year >= (MID_YEAR + 1) & year <= MAX_YEAR,
                 sum(net_immigration, na.rm = TRUE) / length(unique(year))]
      format_number(avg)
    })
  })
}
