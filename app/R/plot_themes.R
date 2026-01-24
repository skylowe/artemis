# Plot Themes and Helpers
# =============================================================================
# Consistent styling for all visualizations

#' ARTEMIS ggplot2 theme
#' @param base_size Base font size
#' @param base_family Base font family
#' @return ggplot2 theme object
theme_artemis <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Title
      plot.title = element_text(
        face = "bold",
        size = rel(1.2),
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        color = "#7F8C8D",
        size = rel(0.9),
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        color = "#95A5A6",
        size = rel(0.8),
        hjust = 1
      ),

      # Axes
      axis.title = element_text(
        color = "#2C3E50",
        size = rel(0.9)
      ),
      axis.text = element_text(
        color = "#34495E",
        size = rel(0.85)
      ),
      axis.line = element_line(color = "#BDC3C7", linewidth = 0.5),
      axis.ticks = element_line(color = "#BDC3C7"),

      # Panel
      panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),

      # Legend
      legend.position = "bottom",
      legend.title = element_text(
        face = "bold",
        size = rel(0.9)
      ),
      legend.text = element_text(size = rel(0.85)),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),

      # Strip (facets)
      strip.background = element_rect(fill = "#34495E", color = NA),
      strip.text = element_text(
        color = "white",
        face = "bold",
        size = rel(0.9),
        margin = margin(t = 5, b = 5)
      ),

      # Plot background
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )
}

#' Color palettes for different data types
#' @name artemis_colors
#' @export
artemis_colors <- list(
  # Sex colors
  sex = c(
    male = "#3498DB",
    female = "#E74C3C"
  ),

  # Marital status colors
  marital = c(
    single = "#9B59B6",
    married = "#27AE60",
    divorced = "#E67E22",
    widowed = "#7F8C8D"
  ),

  # Age group colors (sequential)
  age_groups = if (requireNamespace("viridis", quietly = TRUE)) {
    viridis::viridis(6)
  } else {
    c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725")
  },

  # Scenario comparison colors
  scenarios = c(
    "#3498DB", "#E74C3C", "#27AE60", "#F39C12",
    "#9B59B6", "#1ABC9C", "#E91E63", "#00BCD4"
  ),

  # Cause of death colors
  causes = c(
    CVD = "#E74C3C",
    CAN = "#9B59B6",
    ACV = "#F39C12",
    RES = "#3498DB",
    DEM = "#7F8C8D",
    OTH = "#2C3E50"
  ),

  # Immigration type colors
  immigration = c(
    LPR = "#27AE60",
    "Net O" = "#3498DB",
    Emigration = "#E74C3C"
  ),

  # Diverging palette for differences
  diverging = c(
    negative = "#E74C3C",
    neutral = "#ECF0F1",
    positive = "#27AE60"
  )
)

#' Format large numbers for display
#' @param x Numeric vector
#' @param suffix Optional suffix (e.g., "M" for millions)
#' @return Formatted character vector
format_number <- function(x, suffix = "") {
  ifelse(
    abs(x) >= 1e9,
    paste0(format(round(x / 1e9, 1), nsmall = 1), "B", suffix),
    ifelse(
      abs(x) >= 1e6,
      paste0(format(round(x / 1e6, 1), nsmall = 1), "M", suffix),
      ifelse(
        abs(x) >= 1e3,
        paste0(format(round(x / 1e3, 1), nsmall = 1), "K", suffix),
        format(round(x, 0), big.mark = ",")
      )
    )
  )
}

#' Create age group labels
#' @param breaks Age group break points
#' @return Character vector of labels
make_age_labels <- function(breaks = seq(0, 100, 5)) {
  n <- length(breaks) - 1
  labels <- paste0(breaks[1:n], "-", breaks[2:(n + 1)] - 1)
  labels[n] <- paste0(breaks[n], "+")
  labels
}

#' Empty plotly chart with message
#' @param message Message to display
#' @return plotly object
plotly_empty_message <- function(message = "No data available") {
  plotly::plot_ly() |>
    plotly::layout(
      title = list(
        text = message,
        font = list(color = "#7F8C8D", size = 14)
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      paper_bgcolor = "#FAFAFA",
      plot_bgcolor = "#FAFAFA"
    )
}

#' Standard plotly layout settings
#' @param p plotly object
#' @param title Chart title
#' @param x_title X-axis title
#' @param y_title Y-axis title
#' @return Modified plotly object
layout_artemis <- function(p, title = NULL, x_title = NULL, y_title = NULL) {
  p |>
    plotly::layout(
      title = if (!is.null(title)) {
        list(text = title, font = list(size = 16, color = "#2C3E50"))
      },
      xaxis = list(
        title = x_title,
        gridcolor = "#ECF0F1",
        zerolinecolor = "#BDC3C7"
      ),
      yaxis = list(
        title = y_title,
        gridcolor = "#ECF0F1",
        zerolinecolor = "#BDC3C7"
      ),
      legend = list(
        orientation = "h",
        y = -0.15,
        x = 0.5,
        xanchor = "center"
      ),
      hovermode = "x unified",
      margin = list(l = 60, r = 30, t = 50, b = 60)
    )
}
