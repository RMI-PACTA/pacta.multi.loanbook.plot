% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_scatter_alignment_exposure.R
\name{plot_scatter_alignment_exposure}
\alias{plot_scatter_alignment_exposure}
\title{Plot alignment scatterplot}
\usage{
plot_scatter_alignment_exposure(
  data,
  floor_outliers,
  cap_outliers,
  category,
  currency
)
}
\arguments{
\item{data}{data.frame. Should have the same format as output of
\code{prep_scatter()} and contain columns: 'name', 'buildout', phaseout', 'net'.}

\item{floor_outliers}{Numeric. Floor which should be applied to the alignment
values in the data. Values smaller than floor are plotted on the border of
the plot.}

\item{cap_outliers}{Numeric. Cap which should be applied to the alignment
values in the data. Values bigger than cap are plotted on the border of the
plot.}

\item{category}{Character. Character specifying the variable that contains
the groups by which to analyse the loan books. Usually this will be
\code{"group_id"} unless there is a clearly specified reason to use another
category.}

\item{currency}{Character. Currency to display in the plot labels.}
}
\value{
object of type "ggplot"
}
\description{
Plot alignment scatterplot
}
\examples{
# TODO
}
