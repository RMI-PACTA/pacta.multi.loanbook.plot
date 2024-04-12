#' Prepare data to plot timeline
#'
#' @param data data.frame. Must contain columns: `by_group`, `'direction'`,
#'   `'year'`, `'exposure_weighted_net_alignment'`, `'sector'`.
#' @param sector Character. Sector to filter data on.
#' @param region Character. Region to filter data on.
#' @param by_group Character. Vector of length 1. Variable to group by.
#' @param groups_to_plot Character vector. Group ids to filter on.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # TODO
prep_timeline <- function(data, sector, region, by_group, groups_to_plot) {
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
