#' The default Ludis visualization theme
#'
#' ludis_theme defines the default style for Ludis visualizations
#' @export
ludis_theme <- function(){
  ggplot2::theme_grey() +
    ggplot2::theme_update(
      plot.background = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = "black"),
      panel.border = ggplot2::element_rect(fill = NA, color = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#4f4f4f"),
      axis.text.x = ggplot2::element_text(size = 8, color = "#f5f6fa"),
      axis.text.y = ggplot2::element_text(size = 10, color = "#f5f6fa"),
      axis.title.x = ggplot2::element_text(color = "#f5f6fa"),
      axis.title.y = ggplot2::element_text(color = "#f5f6fa"),
      strip.text = ggplot2::element_text(color = "white", face = "bold"),
      strip.background = ggplot2::element_rect(fill = "#4f4f4f"),
      legend.background = ggplot2::element_rect(fill = "black"),
      legend.text = ggplot2::element_text(color = "white"),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Create a bar chart grouped by team (match-level)
#'
#' groupTeamMatch returns a bar chart, grouped by team, ggplot object for individual match stats
#'
#' This function creates a grouped bar chart visualization comparing team/opp match stats,
#' where color is based on the team.
#' @param df A formatted dataframe.
#' @param x_lab A string. Labels the x axis.
#' @param y_lab A string. Labels the y axis.
#' @param visCols A vector of hex color strings. Sets the colors of the chart.
#' @return If all parameters are valid, then the output will be a ggplot object.
#' @export
#' @importFrom rlang .data
groupTeamMatch <- function(df, x_lab, y_lab, visCols){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$type, fill = .data$team, y = .data$val)) +
    ggplot2::geom_bar(position="dodge", stat="identity", ggplot2::aes(text = paste0(.data$team, "\n", .data$type, ": ", .data$val, .data$lab))) +
    # plot formatting
    ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab) +
    # style
    ludis_theme() + ggplot2::scale_fill_manual(values = visCols)
  return(p)
}

#' Create a bar chart grouped by stat (match-level)
#'
#' groupStatMatch returns a bar chart, grouped by stat, ggplot object for individual match stats
#'
#' This function creates a grouped bar chart visualization comparing team/opp match stats,
#' where color is based on the stat.
#' @inherit groupTeamMatch params return
#' @export
#' @importFrom rlang .data
groupStatMatch <- function(df, x_lab, y_lab, visCols){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$type, fill = interaction(.data$type, .data$team), y = .data$val)) +
    ggplot2::geom_bar(position="dodge", stat="identity", ggplot2::aes(text = paste0(.data$team, "\n", .data$type, ": ", .data$val, .data$lab))) +
    # plot formatting
    ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab) +
    # style
    ludis_theme() + ggplot2::scale_fill_manual(values = visCols)
  return(p)
}

#' Create a stacked bar chart (match-level)
#'
#' stackMatch returns a stacked bar chart ggplot object for individual match stats
#'
#' This function creates a stacked bar chart visualization comparing team/opp match stats.
#' column stats are aggregated together (eg. summed, used to calculate a ratio, etc.).
#' @inheritParams groupTeamMatch
#' @param y_lim (optional) A number to set the max y value for the chart. Standardizes the visualization scales across the team/opp.
#' @inherit groupTeamMatch return
#' @export
#' @importFrom rlang .data
stackMatch <- function(df, x_lab, y_lab, visCols, y_lim){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$team, y = .data$val, fill = .data$type)) +
    ggplot2::geom_bar(position="stack", stat="identity", ggplot2::aes(text = paste0(.data$type, "\nval: ", .data$val, .data$lab))) +
    # plot formatting
    ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab) +
    # style
    ludis_theme() + ggplot2::scale_fill_manual(values = visCols)
  if(!missing(y_lim)){
    p <- p + ggplot2::ylim(0, y_lim)
  }
  return(p)
}

#' Create a donut chart (match-level)
#'
#' donutMatch returns a donut chart ggplot object for individual match stats
#'
#' This function creates a donut chart visualization of match stats.
#' @param df A formatted dataframe.
#' @param x_lab A string. Labels the x axis.
#' @param y_lab A string. Labels the y axis.
#' @param visCols A vector of hex color strings. Sets the colors of the chart.
#' @param themeCol A hex color string. Sets the background color of the chart.
#' @param textCol A hex color string. Sets the text color of the chart legend.
#' @inherit groupTeamMatch return
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
donutMatch <- function(df, x_lab, y_lab, visCols, themeCol, textCol){
  p <- df %>% plotly::plot_ly(labels = ~.data$type, values=~.data$val, textinfo = 'none', marker = list(colors = visCols)) %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(plot_bgcolor = themeCol, paper_bgcolor = themeCol,
                   legend = list(font = list(color = textCol)))
  return(p)
}

#' Create a stacked bar chart (season-level)
#'
#' stackTrend returns a stacked bar chart ggplot object for season stats
#'
#' This function creates a stacked bar chart visualization of a stat trend over time.
#' @inherit groupTeamMatch params return
#' @export
#' @importFrom rlang .data
stackTrend <- function(df, x_lab, y_lab, visCols){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$game, y = .data$val, fill = .data$type)) +
    ggplot2::geom_bar(position="stack", stat="identity", ggplot2::aes(text = paste0("game: ", .data$game, "\n", .data$type, "\nval: ", .data$val, .data$lab))) +
    # plot formatting
    ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab) +
    # style
    ludis_theme() + ggplot2::scale_fill_manual(values = visCols)
  return(p)
}

#' Create a line chart (season-level)
#'
#' lineTrend returns a line chart ggplot object for season stats
#'
#' This function creates a line chart visualization of a stat trend over time.
#' @inherit groupTeamMatch params return
#' @export
#' @importFrom rlang .data
lineTrend <- function(df, x_lab, y_lab, visCols){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$game, y = .data$val, group = .data$type, color = .data$type)) +
    ggplot2::geom_line() + ggplot2::geom_point(ggplot2::aes(text = paste0("game: ", .data$game, "\n", .data$type, "\nval: ", .data$val, .data$lab))) +
    # plot formatting
    ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab) +
    # style
    ludis_theme() + ggplot2::scale_color_manual(values = visCols)
  return(p)
}

#' Create a multi-panel line chart (season-level)
#'
#' totalFacetTrend returns a multi-panel line chart ggplot object for season stats
#'
#' This function creates a multi-panel line chart visualization of a stat trend over time.
#' @param df A formatted datframe. The Stat variable in the df is the stat by which the function plots a new panel (e.g. per player).
#' @param x_lab A string. Labels the x axis.
#' @param y_lab A string. Labels the y axis.
#' @param visCols A vector of hex color strings. Sets the colors of the chart.
#' @inherit groupTeamMatch return
#' @export
#' @importFrom rlang .data
totalFacetTrend <- function(df, x_lab, y_lab, visCols){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$game, y = .data$val, group = .data$Stat, color = .data$Stat)) +
    ggplot2::geom_line() + ggplot2::geom_point(ggplot2::aes(text = paste0("game: ", .data$game, "\n", .data$Stat, "\nval: ", .data$val, .data$lab))) +
    # plot formatting
    ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab) +
    # style
    ludis_theme() + ggplot2::scale_color_manual(values = visCols) +
    ggplot2::facet_wrap(~.data$type, ncol = 1) +
    ggplot2::theme(panel.spacing.y = ggplot2::unit(1, "lines"))
  return(p)
}

#' Create a grouped bar chart (season-level)
#'
#' groupBarTrend returns a grouped bar chart ggplot object for season stats
#'
#' This function creates a grouped bar chart visualization for a stat over time.
#' @inherit groupTeamMatch params return
#' @export
#' @importFrom rlang .data
groupBarTrend <- function(df, x_lab, y_lab, visCols){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$game, fill = .data$type, y = .data$val)) +
    ggplot2::geom_bar(position="dodge", stat="identity", ggplot2::aes(text = paste0("game: ", .data$game, "\n", .data$type, ": ", .data$val, .data$lab))) +
    # plot formatting
    ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab) +
    # style
    ludis_theme() + ggplot2::scale_fill_manual(values = visCols)
  return(p)
}

#' Create a multi-panel stacked bar chart (season-level)
#'
#' stackGroupTrend returns a multi-panel stacked bar chart ggplot object for season stats
#'
#' This function creates a multi-panel stacked bar chart visualization of a stat trend over time, grouped by game.
#' @inherit groupTeamMatch params return
#' @export
#' @importFrom rlang .data
stackGroupTrend <- function(df, x_lab, y_lab, visCols){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$stat, y = .data$val, fill = .data$type)) +
    ggplot2::geom_bar(position="stack", stat="identity", ggplot2::aes(text = paste0("game: ", .data$game, "\n", .data$type, "\nval: ", .data$val, .data$lab))) +
    # plot formatting
    ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab) +
    # style
    ludis_theme() + ggplot2::scale_fill_manual(values = visCols) +
    ggplot2::facet_grid(~.data$game)
  return(p)
}

#' Create a donut chart (season-level)
#'
#' donutSznSum returns a donut chart ggplot object for season stats.
#'
#' This function creates a donut chart visualization of cumulative stat totals for a season.
#' @param df A formatted dataframe.
#' @param plot_lab A string. Labels the chart.
#' @param visCols A vector of hex color strings. Sets the colors of the chart.
#' @param themeCol A hex color string. Sets the background color of the chart.
#' @param textCol A hex color string. Sets the text color of the chart legend.
#' @inherit groupTeamMatch return
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
donutSznSum <- function(df, plot_lab, visCols, themeCol, textCol){
  p <- df %>% plotly::plot_ly(labels = ~.data$type, values=~.data$sum, textinfo = 'none', marker = list(colors = visCols)) %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(title = list(text = plot_lab, font = list(color = textCol)),
                   plot_bgcolor = themeCol, paper_bgcolor = themeCol,
                   legend = list(font = list(color = textCol)))
  return(p)
}
