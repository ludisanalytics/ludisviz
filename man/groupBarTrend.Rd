% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_data.R
\name{groupBarTrend}
\alias{groupBarTrend}
\title{Create a grouped bar chart (season-level)}
\usage{
groupBarTrend(df, x_lab, y_lab, visCols)
}
\arguments{
\item{df}{A formatted dataframe.}

\item{x_lab}{A string. Labels the x axis.}

\item{y_lab}{A string. Labels the y axis.}

\item{visCols}{A vector of hex color strings. Sets the colors of the chart.}
}
\value{
If all parameters are valid, then the output will be a ggplot object.
}
\description{
groupBarTrend returns a grouped bar chart ggplot object for season stats
}
\details{
This function creates a grouped bar chart visualization for a stat over time.
}
