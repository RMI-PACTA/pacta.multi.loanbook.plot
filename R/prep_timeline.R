#' Prepare data to plot timeline
#'
#' @param data data.frame. Must contain columns: `'direction'`, `'year'`,
#'   `'exposure_weighted_net_alignment'`, `'sector'` and any column implied by
#'   `by_group`.
#' @param sector Character. Sector to filter data on.
#' @param region Character. Region to filter data on.
#' @param by_group Character. Vector of length 1. Variable to group by.
#' @param groups_to_plot Character vector. Groups to filter on.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # TODO
prep_timeline <- function(data,
                          sector,
                          region,
                          by_group,
                          groups_to_plot) {
  if (!is.null(by_group)) {
    if (!inherits(by_group, "character")) {
      stop("by_group must be of class character")
    }
    if (!length(by_group) == 1) {
      stop("by_group must be of length 1")
    }
  } else {
    data <- data %>%
      dplyr::mutate(aggregate_loan_book = "Aggregate loan book")
    by_group <- "aggregate_loan_book"
  }

  check_prep_timeline(data, sector, region, by_group, groups_to_plot)

  data_timeline <- data %>%
    dplyr::filter(
      .data$sector == .env$sector,
      .data$region == .env$region,
      !!rlang::sym(by_group) %in% groups_to_plot
    )

  data_timeline
}

check_prep_timeline <- function(data, sector, region, by_group, groups_to_plot) {
  abort_if_missing_names(data, c(
    "direction",
    "year",
    "exposure_weighted_net_alignment",
    "sector",
    by_group
  ))
  abort_if_unknown_values(sector, data, "sector")
  abort_if_unknown_values(region, data, "region")
  abort_if_unknown_values(groups_to_plot, data, by_group)
}
