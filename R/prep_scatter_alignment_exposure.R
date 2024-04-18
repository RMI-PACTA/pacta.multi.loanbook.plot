#' Prepare data to plot using `plot_scatter_alignment_exposure()`
#'
#' @param data data.frame. Holds net aggregated alignment metrics on the loan
#'   book level. Must contain columns: `"scenario"`, `"region"`,
#'   `"sector"`, `"year"`, `"exposure_weighted_net_alignment"`,
#'   `"sum_loan_size_outstanding"` and any column implied by `group_var`.
#' @param year Integer. Year on which `data` should be filtered.
#' @param region Character. Region to filter `data` data frame on.
#' @param scenario Character. Scenario to filter `data` data frame on.
#' @param group_var Character. Vector of length 1. A column to group by. Must be
#'   available variables in `data`.
#' @param exclude_groups Character. Character specifying any values from
#'   `group_var` that should not be included in the analysis. This is useful to
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
                                            group_var,
                                            exclude_groups = "benchmark") {
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

  data <- data %>%
    dplyr::filter(
      !grepl(paste0(.env$exclude_groups, collapse = "|"), !!rlang::sym(group_var))
    ) %>%
    dplyr::filter(
      .data$year == .env$year,
      .data$region == .env$region,
      .data$scenario == .env$scenario
    ) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          group_var,
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
