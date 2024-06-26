% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_scatter_animated.R
\name{plot_scatter_animated}
\alias{plot_scatter_animated}
\title{Plot alignment scatterplot}
\usage{
plot_scatter_animated(
  data,
  data_level = c("company", "group_var"),
  sector = NULL,
  scenario_source = NULL,
  scenario = NULL,
  region = NULL,
  title = NULL,
  subtitle = NULL,
  alignment_limit = NULL,
  cap_outliers = NULL,
  floor_outliers = NULL
)
}
\arguments{
\item{data}{data.frame. Should have the same format as output of
\code{prep_scatter_animated()} and contain columns: 'name', 'buildout',
'phaseout', 'net' and 'year'.}

\item{data_level}{Character. Level of the plotted data. Can be 'group_var' or
'company'.}

\item{sector}{Character. Sector name to be used in the plot title.}

\item{scenario_source}{Character. Scenario source to be used in the plot
caption.}

\item{scenario}{Character. Scenario name to be used in the plot caption.}

\item{region}{Character. Region to be used in the plot caption.}

\item{title}{Character. Custom title if different than default.}

\item{subtitle}{Character. Custom subtitle if different than default.}

\item{alignment_limit}{Numeric. Limit to be applied to the x- and y-axis
scales and to alignment values for colouring. By default the maximum
absolute alignment value from data is used.}

\item{cap_outliers}{Numeric. Cap which should be applied to the alignment
values in the data. Values bigger than cap are plotted on the border of the
plot.}

\item{floor_outliers}{Numeric. Floor which should be applied to the alignment
values in the data. Values smaller than floor are plotted on the border of
the plot.}
}
\value{
object of type "plotly"
}
\description{
Plot alignment scatterplot
}
\examples{
# TODO
}
