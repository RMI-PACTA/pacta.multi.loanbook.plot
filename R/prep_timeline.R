#' Prepare data to plot timeline
#'
#' @param data data.frame. Must contain columns: `'direction'`, `'year'`,
#'   `'exposure_weighted_net_alignment'`, `'sector'` and any column implied by
#'   `group_var`.
#' @param sector Character. Sector to filter data on.
#' @param region Character. Region to filter data on.
#' @param group_var Character. Vector of length 1. Variable to group by.
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
                          group_var,
                          groups_to_plot) {
  if (!is.null(group_var)) {
    if (!inherits(group_var, "character")) {
      stop("group_var must be of class character")
    }
    if (!length(group_var) == 1) {
      stop("group_var must be of length 1")
    }
  } else {
    data <- data %>%
      dplyr::mutate(aggregate_loan_book = "Aggregate loan book")
    group_var <- "aggregate_loan_book"
  }

  check_prep_timeline(data, sector, region, group_var, groups_to_plot)

  data_timeline <- data %>%
    dplyr::filter(
      .data$sector == .env$sector,
      .data$region == .env$region,
      !!rlang::sym(group_var) %in% groups_to_plot
    )

  data_timeline
}

check_prep_timeline <- function(data, sector, region, group_var, groups_to_plot) {
  abort_if_missing_names(data, c(
    "direction",
    "year",
    "exposure_weighted_net_alignment",
    "sector",
    group_var
  ))
  abort_if_unknown_values(sector, data, "sector")
  abort_if_unknown_values(region, data, "region")
  abort_if_unknown_values(groups_to_plot, data, group_var)
}
