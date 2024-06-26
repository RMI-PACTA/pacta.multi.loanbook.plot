% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_scatter.R
\name{prep_scatter}
\alias{prep_scatter}
\title{Prepare data to plot scatterplot}
\usage{
prep_scatter(
  data_bopo,
  data_net,
  data_level = c("group_var", "company"),
  year,
  sector,
  region,
  group_var,
  groups_to_plot = NULL
)
}
\arguments{
\item{data_bopo}{data.frame. Data containing buildout and phaseout alignment
values. Must contain columns: \code{'year'}, \code{'sector'}, \code{'region'},
\code{'direction'} and either \code{'name_abcd'} and \code{'alignment_metric'} or
\code{'exposure_weighted_net_alignment'} plus any column implied by \code{group_var}.}

\item{data_net}{data.frame. Data containing net alignment values. Must
contain columns: \code{group_var}, \code{'year'}, \code{'sector'}, \code{'region'},
\code{'direction'} and either \code{'name_abcd'} and \code{'alignment_metric'} or
\code{'exposure_weighted_net_alignment'}.}

\item{data_level}{Character. Level of the plotted data. Can be \code{'group_var'} or
\code{'company'}.}

\item{year}{Integer. Year on which the data should be filtered.}

\item{sector}{Character. Sector to filter data on.}

\item{region}{Character. Region to filter data on.}

\item{group_var}{Character. Vector of length 1. Variable to group by.}

\item{groups_to_plot}{Character vector. Groups to filter on.}
}
\value{
data.frame
}
\description{
Prepare data to plot scatterplot
}
\examples{
# TODO
}
