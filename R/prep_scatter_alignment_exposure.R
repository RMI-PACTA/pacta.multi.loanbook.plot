#' Prepare data to plot using `plot_scatter_alignment_exposure()`
#'
#' @param data data.frame. Holds net aggregated alignment metrics on the loan
#'   book level. Must contain columns: `group_id`, `scenario`, `region`,
#'   `sector`, `year`, `exposure_weighted_net_alignment`,
#'   `sum_loan_size_outstanding`.
#' @param year Integer. Year on which `data` should be filtered.
#' @param region Character. Region to filter `data` data frame on.
#' @param scenario Character. Scenario to filter `data` data frame on.
#' @param category Character. Character specifying the variable that contains
#'   the groups by which to analyse the loan books. Usually this will be
#'   `"group_id"` unless there is a clearly specified reason to use another
#'   category.
#' @param exclude_group_ids Character. Character specifying any values from
#'   `category` that should not be included in the analysis. This is useful to
#'   remove benchmarks that are not meant to be compared at the same level.
#'   Defaults to `"benchmark"`.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # TODO
prep_scatter_alignment_exposure <- function(data,
                                            year,
                                            region,
                                            scenario,
                                            category,
                                            exclude_group_ids = "benchmark") {
  data <- data %>%
    dplyr::filter(
      !grepl(paste0(.env$exclude_group_ids, collapse = "|"), .data$group_id)
    ) %>%
    dplyr::filter(
      .data$year == .env$year,
      .data$region == .env$region,
      .data$scenario == .env$scenario
    ) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          .env$category,
          "scenario",
          "region",
          "sector",
          "year",
          "exposure_weighted_net_alignment",
          "sum_loan_size_outstanding"
        )
      )
    )

  data
}
