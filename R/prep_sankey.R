#' Prepare data to plot using `plot_sankey()`
#'
#' @param data_alignment data.frame. Holds aggregated alignment metrics per
#'   company for tms sectors. Must contain columns: `"name_abcd"`,
#'   `"sector"` and any column implied by `group_var`.
#' @param region Character. Region to filter `data_alignment` data frame on.
#' @param year Integer. Year on which `data_alignment` should be filtered.
#' @param group_var Character. Vector of length 1. Variable to group by.
#' @param middle_node Character. Column specifying the middle nodes to be
#'   plotted in sankey plot. Must be present in `data_alignment`.
#' @param middle_node2 Character. Column specifying the middle nodes to be
#'   plotted in sankey plot. Must be present in `data_alignment`.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # TODO
prep_sankey <- function(data_alignment,
                        region,
                        year,
                        group_var,
                        middle_node,
                        middle_node2 = NULL) {
  if (!is.null(group_var)) {
    if (!inherits(group_var, "character")) {
      stop("group_var must be of class character")
    }
    if (!length(group_var) == 1) {
      stop("group_var must be of length 1")
    }
  } else {
    data_alignment <- data_alignment %>%
      dplyr::mutate(aggregate_loan_book = "Aggregate loan book")
    group_var <- "aggregate_loan_book"
  }

  check_prep_sankey(
    data_alignment = data_alignment,
    region = region,
    year = year,
    group_var = group_var,
    middle_node = middle_node,
    middle_node2 = middle_node2
  )

  data_alignment <- data_alignment %>%
    dplyr::filter(
      .data$region == .env$region,
      .data$year == .env$year
    )

  if (is.null(middle_node2)) {
    data_out <- data_alignment %>%
      dplyr::mutate(
        is_aligned = dplyr::case_when(
          alignment_metric >= 0 ~ "Aligned",
          alignment_metric < 0 ~ "Not aligned",
          TRUE ~ "Unknown"
        ),
        middle_node = !!rlang::sym(middle_node)
      ) %>%
      dplyr::select(group_var, "middle_node", "is_aligned", "loan_size_outstanding") %>%
      dplyr::group_by(!!rlang::sym(group_var), .data$middle_node, .data$is_aligned) %>%
      dplyr::summarise(loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!rlang::sym(group_var), .data$is_aligned)
  } else {
    data_out <- data_alignment %>%
      dplyr::mutate(
        is_aligned = dplyr::case_when(
          alignment_metric >= 0 ~ "Aligned",
          alignment_metric < 0 ~ "Not aligned",
          TRUE ~ "Unknown"
        ),
        middle_node = !!rlang::sym(middle_node),
        middle_node2 = !!rlang::sym(middle_node2)
      ) %>%
      dplyr::select(group_var, "middle_node", "middle_node2", "is_aligned", "loan_size_outstanding") %>%
      dplyr::group_by(!!rlang::sym(group_var), .data$middle_node, .data$middle_node2, .data$is_aligned) %>%
      dplyr::summarise(loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!rlang::sym(group_var), .data$is_aligned)
  }
  data_out
}

check_prep_sankey <- function(data_alignment,
                              region,
                              year,
                              group_var,
                              middle_node,
                              middle_node2) {
  names_all <- c(group_var, "name_abcd", "sector")
  names_aggergate <- c("region", "year")
  abort_if_missing_names(data_alignment, c(names_all, names_aggergate))
  if (!(region %in% unique(data_alignment$region))) {
    rlang::abort(c(
      "`region_tms` value not found in `data_alignment` dataset.",
      i = glue::glue("Regions in `data_alignment` are: {toString(unique(data_alignment$region))}"),
      x = glue::glue("You provided region = {region}.")
    ))
  }
  if (!(year %in% unique(data_alignment$year))) {
    rlang::abort(c(
      "`year` value not found in `data_alignment`.",
      i = glue::glue(
        "Years in `data_alignment` are: {toString(unique(data_alignment$year))}
        "
      ),
      x = glue::glue("You provided year = {year}.")
    ))
  }
  abort_if_middle_node_column_not_found(data_alignment, middle_node, env = list(data = substitute(data_alignment)))
  if (!is.null(middle_node2)) {
    abort_if_middle_node_column_not_found(data_alignment, middle_node2, list(data = substitute(data_alignment)))
  }
}

abort_if_middle_node_column_not_found <- function(data, name, env = parent.frame()) {
  .data <- deparse_1(substitute(data, env = env))

  if (!(name %in% names(data))) {
    rlang::abort(c(
      glue::glue("Column name you passed as one of the middle nodes not found in {.data}."),
      i = glue::glue(
        "Column names in `{.data}` are: {toString(names(data))}"
      ),
      x = glue::glue("You asked to use column named: `{name}`.")
    ))
  }
}
