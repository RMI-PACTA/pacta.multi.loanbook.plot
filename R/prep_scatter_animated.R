#' Prepare data to plot animated scatterplot
#'
#' @param data_bopo data.frame. Data containing buildout and phaseout alignment
#'   values. Must contain columns: `by_group`, `'year'`, `'sector'`, `'region'`,
#'   `'direction'` and either `'name_abcd'` and `'alignment_metric'` or
#'   `'exposure_weighted_net_alignment'`.
#' @param data_net data.frame. Data containing net alignment values. Must
#'   contain columns: `by_group`, `'year'`, `'sector'`, `'region'`, `'direction'` and
#'   either `'name_abcd'` and `'alignment_metric'` or `'exposure_weighted_net_alignment'`.
#' @param data_level Character. Level of the plotted data. Can be `'bank'` or
#'   `'company'`.
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
prep_scatter_animated <- function(data_bopo,
                                  data_net,
                                  data_level = c("bank", "company"),
                                  sector,
                                  region,
                                  by_group,
                                  groups_to_plot = NULL) {
  rlang::arg_match(data_level)

  if (!is.null(by_group)) {
    if (!inherits(by_group, "character")) {
      stop("by_group must be of class character")
    }
    if (!length(by_group) == 1) {
      stop("by_group must be of length 1")
    }
  } else {
    data_bopo <- data_bopo %>%
      dplyr::mutate(aggregate_loan_book = "Aggregate loan book")
    data_net <- data_net %>%
      dplyr::mutate(aggregate_loan_book = "Aggregate loan book")
    by_group <- "aggregate_loan_book"
  }

  if (data_level == "bank") {
    name_col <- by_group
    value_col <- "exposure_weighted_net_alignment"
  } else {
    name_col <- "name_abcd"
    value_col <- "alignment_metric"
  }

  check_prep_scatter_animated(data_bopo, sector, region, by_group, groups_to_plot, name_col, value_col)
  check_prep_scatter_animated(data_net, sector, region, by_group, groups_to_plot, name_col, value_col)

  if (is.null(groups_to_plot)) {
    groups_to_plot <- unique(
      c(
        dplyr::pull(data_bopo, by_group),
        dplyr::pull(data_net, by_group)
      )
    )
  }

  data_scatter <- data_bopo %>%
    dplyr::bind_rows(data_net) %>%
    dplyr::filter(
      .data$sector == .env$sector,
      .data$region == .env$region,
      !!rlang::sym(by_group) %in% groups_to_plot
    ) %>%
    dplyr::select("name" = name_col, "direction", "year", "value" = value_col) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = "direction", values_from = "value") %>%
    dplyr::mutate(
      datapoint = dplyr::case_when(
        grepl(".*[Bb]enchmark,*", .data$name) ~ "Benchmark",
        TRUE & data_level == "bank" ~ "Bank",
        TRUE & data_level == "company" ~ "Company",
        TRUE ~ "Portfolio"
      )
    ) %>%
    dplyr::mutate(
      datapoint = factor(.data$datapoint, levels = c("Bank", "Company", "Portfolio", "Benchmark"))
    ) %>%
    dplyr::arrange(.data$datapoint)

  data_scatter
}

check_prep_scatter_animated <- function(data,
                                        sector,
                                        region,
                                        by_group,
                                        groups_to_plot,
                                        name_col,
                                        value_col) {
  abort_if_missing_names(
    data,
    c(
      by_group,
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
  abort_if_unknown_values(groups_to_plot, data, by_group)
}
