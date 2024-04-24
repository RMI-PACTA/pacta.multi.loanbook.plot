#' Plot alignment scatterplot
#'
#' @param data data.frame. Should have the same format as output of
#'   `prep_scatter()` and contain columns: `'name'`, `'buildout'`, `'phaseout'`,
#'   `'net'`, and any column implied by `group_var`.
#' @param floor_outliers Numeric. Floor which should be applied to the alignment
#'   values in the data. Values smaller than floor are plotted on the border of
#'   the plot.
#' @param cap_outliers Numeric. Cap which should be applied to the alignment
#'   values in the data. Values bigger than cap are plotted on the border of the
#'   plot.
#' @param group_var Character. Character specifying the variable that contains
#'   the groups by which to analyse the loan books.
#' @param currency Character. Currency to display in the plot labels.
#'
#' @return object of type "ggplot"
#' @export
#'
#' @examples
#' # TODO
plot_scatter_alignment_exposure <- function(data,
                                            floor_outliers,
                                            cap_outliers,
                                            group_var,
                                            currency) {
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

  if (!is.null(floor_outliers)) {
    data <- data %>%
      dplyr::mutate(
        exposure_weighted_net_alignment = dplyr::if_else(
          .data$exposure_weighted_net_alignment <= .env$floor_outliers,
          .env$floor_outliers,
          .data$exposure_weighted_net_alignment
        )
      )
  }

  if (!is.null(cap_outliers)) {
    data <- data %>%
      dplyr::mutate(
        exposure_weighted_net_alignment = dplyr::if_else(
          .data$exposure_weighted_net_alignment >= .env$cap_outliers,
          .env$cap_outliers,
          .data$exposure_weighted_net_alignment
        )
      )
  }

  title <- glue::glue("Net Aggregate Alignment By Financial Exposure And Sector")
  subtitle <- ""
  if (any(!is.null(floor_outliers), !is.null(cap_outliers))) {
    subtitle <- glue::glue(
      "{subtitle}Outliers are displayed on the lower and upper boundaries: {floor_outliers} and {cap_outliers}.",
      .trim = FALSE
    )
  }

  plot <- data %>%
    dplyr::mutate(sector = tools::toTitleCase(.data$sector)) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$sum_loan_size_outstanding,
        y = .data$exposure_weighted_net_alignment,
        color = !!rlang::sym(group_var)
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::ylim(-1, 1) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::facet_wrap(
      ~sector
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      color = r2dii.plot::to_title(group_var)
    ) +
    ggplot2::xlab(glue::glue("Financial Exposure (in {currency})")) +
    ggplot2::ylab("Net Aggregate Alignment") +
    r2dii.plot::scale_colour_r2dii() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  plot
}
