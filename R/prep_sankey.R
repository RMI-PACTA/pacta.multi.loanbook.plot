#' Prepare data to plot using `plot_sankey()`
#'
#' @param data_alignment data.frame. Holds aggregated alignment metrics per
#'   company for tms sectors. Must contain columns: `group_id`, `name_abcd`,
#'   `sector`.
#' @param matched_loanbook data.frame. Holds the matched loan books of a set of
#'   groups. Must include a column `group_id` and `loan_size_outstanding`.
#' @param region Character. Region to filter `data_alignment` data frame on.
#' @param year Integer. Year on which `data_alignment` should be filtered.
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
                        matched_loanbook,
                        region,
                        year,
                        middle_node,
                        middle_node2 = NULL) {
  check_prep_sankey(
    data_alignment,
    matched_loanbook,
    region,
    year,
    middle_node,
    middle_node2
  )

  data_alignment <- data_alignment %>%
    dplyr::filter(
      .data$region == .env$region,
      .data$year == .env$year
    )

  matched_loanbook <- matched_loanbook %>%
    dplyr::select("group_id", "name_abcd", "sector", "loan_size_outstanding")

  if (is.null(middle_node2)) {
    data_out <- data_alignment %>%
      dplyr::inner_join(matched_loanbook, by = c("group_id", "name_abcd", "sector")) %>%
      dplyr::mutate(
        is_aligned = dplyr::case_when(
          alignment_metric >= 0 ~ "Aligned",
          alignment_metric < 0 ~ "Not aligned",
          TRUE ~ "Unknown"
        ),
        middle_node = !!rlang::sym(middle_node)
      ) %>%
      dplyr::select("group_id", "middle_node", "is_aligned", "loan_size_outstanding") %>%
      dplyr::group_by(.data$group_id, .data$middle_node, .data$is_aligned) %>%
      dplyr::summarise(loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$group_id, .data$is_aligned)
  } else {
    data_out <- data_alignment %>%
      dplyr::inner_join(matched_loanbook, by = c("group_id", "name_abcd", "sector")) %>%
      dplyr::mutate(
        is_aligned = dplyr::case_when(
          alignment_metric >= 0 ~ "Aligned",
          alignment_metric < 0 ~ "Not aligned",
          TRUE ~ "Unknown"
        ),
        middle_node = !!rlang::sym(middle_node),
        middle_node2 = !!rlang::sym(middle_node2)
      ) %>%
      dplyr::select("group_id", "middle_node", "middle_node2", "is_aligned", "loan_size_outstanding") %>%
      dplyr::group_by(.data$group_id, .data$middle_node, .data$middle_node2, .data$is_aligned) %>%
      dplyr::summarise(loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$group_id, .data$is_aligned)
  }
  data_out
}

check_prep_sankey <- function(data_alignment,
                              matched_loanbook,
                              region,
                              year,
                              middle_node,
                              middle_node2) {
  names_all <- c("group_id", "name_abcd", "sector")
  names_aggergate <- c("region", "year")
  abort_if_missing_names(data_alignment, c(names_all, names_aggergate))
  abort_if_missing_names(matched_loanbook, c(names_all, "loan_size_outstanding"))
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
