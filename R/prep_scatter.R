#' Prepare data to plot scatterplot
#'
#' @param data_bopo data.frame. Data containing buildout and phaseout alignment
#'   values. Must contain columns: `'year'`, `'sector'`, `'region'`,
#'   `'direction'` and either `'name_abcd'` and `'alignment_metric'` or
#'   `'exposure_weighted_net_alignment'` plus any column implied by `group_var`.
#' @param data_net data.frame. Data containing net alignment values. Must
#'   contain columns: `group_var`, `'year'`, `'sector'`, `'region'`,
#'   `'direction'` and either `'name_abcd'` and `'alignment_metric'` or
#'   `'exposure_weighted_net_alignment'`.
#' @param data_level Character. Level of the plotted data. Can be `'group_var'` or
#'   `'company'`.
#' @param year Integer. Year on which the data should be filtered.
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
prep_scatter <- function(data_bopo,
                         data_net,
                         data_level = c("group_var", "company"),
                         year,
                         sector,
                         region,
                         group_var,
                         groups_to_plot = NULL) {
  rlang::arg_match(data_level)

  if (!is.null(group_var)) {
    if (!inherits(group_var, "character")) {
      stop("group_var must be of class character")
    }
    if (!length(group_var) == 1) {
      stop("group_var must be of length 1")
    }
  } else {
    data_bopo <- data_bopo %>%
      dplyr::mutate(aggregate_loan_book = "Aggregate loan book")
    data_net <- data_net %>%
      dplyr::mutate(aggregate_loan_book = "Aggregate loan book")
    group_var <- "aggregate_loan_book"
  }

  if (data_level == "group_var") {
    name_col <- group_var
    value_col <- "exposure_weighted_net_alignment"
  } else {
    name_col <- "name_abcd"
    value_col <- "alignment_metric"
  }

  check_prep_scatter(data_bopo, year, sector, region, group_var, groups_to_plot, name_col, value_col)
  check_prep_scatter(data_net, year, sector, region, group_var, groups_to_plot, name_col, value_col)

  if (is.null(groups_to_plot)) {
    groups_to_plot <- unique(
      c(
        dplyr::pull(data_bopo, group_var),
        dplyr::pull(data_net, group_var)
      )
    )
  }

  data_scatter <- data_bopo %>%
    dplyr::bind_rows(data_net) %>%
    dplyr::filter(
      .data$year == .env$year,
      .data$sector == .env$sector,
      .data$region == .env$region,
      !!rlang::sym(group_var) %in% groups_to_plot
    ) %>%
    dplyr::select("name" = name_col, "direction", "value" = value_col) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = "direction", values_from = "value") %>%
    dplyr::mutate(
      datapoint = dplyr::case_when(
        grepl(".*[Bb]enchmark,*", .data$name) ~ "benchmark",
        TRUE & (data_level == "group_var") ~ "group",
        TRUE & (data_level == "company") ~ "company",
        TRUE ~ "other"
      )
    )

  data_scatter
}

check_prep_scatter <- function(data,
                               year,
                               sector,
                               region,
                               group_var,
                               groups_to_plot,
                               name_col,
                               value_col) {
  abort_if_missing_names(
    data,
    c(
      group_var,
      "year",
      "sector",
      "region",
      "direction",
      name_col,
      value_col
    )
  )
  abort_if_unknown_values(sector, data, "sector")
  abort_if_unknown_values(region, data, "region")
  abort_if_unknown_values(year, data, "year")
  abort_if_unknown_values(groups_to_plot, data, group_var)
}
