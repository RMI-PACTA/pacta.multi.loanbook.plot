#' Make a sankey plot
#'
#' @param data data.frame. Should have the same format as output of
#'   `prep_sankey()` and contain columns: `"middle_node"`, optionally
#'   `"middle_node2"`, `"is_aligned"`, `"loan_size_outstanding"`, and any column
#'   implied by `group_var`.
#' @param group_var Character. Vector of length 1. Variable to group by.
#' @param capitalise_node_labels Logical. Flag indicating if node labels should
#'   be converted into better looking capitalised form.
#' @param save_png_to Character. Path where the output in png format should be
#'   saved
#' @param png_name Character. File name of the output.
#' @param nodes_order_from_data Logical. Flag indicating if nodes order should
#'   be determined by an algorithm (in case of big datasets often results in a
#'   better looking plot) or should they be ordered based on data.
#'
#' @return NULL
#' @export
#'
#' @examples
#' # TODO
plot_sankey <- function(data,
                        group_var,
                        capitalise_node_labels = TRUE,
                        save_png_to = NULL,
                        png_name = "sankey.png",
                        nodes_order_from_data = FALSE) {
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

  check_plot_sankey(
    data = data,
    group_var = group_var,
    capitalise_node_labels = capitalise_node_labels
  )

  if (capitalise_node_labels) {
    data_links <- data %>%
      dplyr::mutate(
        group_var = r2dii.plot::to_title(!!rlang::sym(group_var)),
        middle_node = r2dii.plot::to_title(.data$middle_node)
      )
    if ("middle_node2" %in% names(data_links)) {
      data_links <- data_links %>%
        dplyr::mutate(
          middle_node2 = r2dii.plot::to_title(.data$middle_node2)
        )
    }
  } else {
    data_links <- data
  }

  links_1 <- data_links %>%
    dplyr::select(
      source = .env$group_var,
      target = "middle_node",
      value = "loan_size_outstanding",
      group = "is_aligned"
    )

  if ("middle_node2" %in% names(data_links)) {
    links_2 <- data_links %>%
      dplyr::select(
        .env$group_var,
        source = "middle_node",
        target = "middle_node2",
        value = "loan_size_outstanding",
        group = "is_aligned"
      )

    links_3 <- data_links %>%
      dplyr::select(
        .env$group_var,
        source = "middle_node2",
        target = "is_aligned",
        value = "loan_size_outstanding",
        group = "is_aligned"
      )

    links <- dplyr::bind_rows(links_1, links_2, links_3)
  } else {
    links_2 <- data_links %>%
      dplyr::select(
        .env$group_var,
        source = "middle_node",
        target = "is_aligned",
        value = "loan_size_outstanding",
        group = "is_aligned"
      )

    links <- dplyr::bind_rows(links_1, links_2)
  }

  links <- links %>%
    dplyr::group_by(.data$source, .data$target, .data$group) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$source, .data$group) %>%
    as.data.frame()

  # TODO: colour the companies if fully aligned or not
  nodes <- data.frame(
    name = unique(c(as.character(links$source), as.character(links$target)))
  ) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        .data$name %in% c("Aligned", "Not aligned", "Unknown") ~ .data$name,
        TRUE ~ "other"
      )
    )

  my_color <- 'd3.scaleOrdinal() .domain(["Not aligned", "Aligned", "Unknown", "other"]) .range(["#e10000","#3d8c40", "#808080", "#808080"])'

  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$target, nodes$name) - 1

  if (nodes_order_from_data) {
    n_iter <- 0
  } else {
    n_iter <- 32 # sankeyNetwork() default
  }

  p <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "value",
    NodeID = "name",
    colourScale = my_color,
    LinkGroup = "group",
    NodeGroup = "group",
    fontSize = 14,
    iterations = n_iter
  )

  if (!is.null(save_png_to)) {
    # you save it as an html
    temp_html <- tempfile(fileext = ".html")
    networkD3::saveNetwork(p, temp_html)

    if (webshot::is_phantomjs_installed()) {
      file_name <- file.path(save_png_to, png_name)
      # you convert it as png
      webshot::webshot(temp_html, path.expand(file_name), vwidth = 1000, vheight = 900)
    } else {
      rlang::abort(
        glue::glue(
          "In order to save the plot as .png you need to have `phantomjs`
          installed. Please run `webshot::install_phantomjs()` if you don't and
          try running the function again."
        )
      )
    }
  }
  p
}

check_plot_sankey <- function(data,
                              group_var,
                              capitalise_node_labels) {
  crucial_names <- c(group_var, "middle_node", "is_aligned", "loan_size_outstanding")
  abort_if_missing_names(data, crucial_names)
  if (!is.logical(capitalise_node_labels)) {
    rlang::abort(
      c(
        "`capitalise_node_labels` must have a logical value.",
        x = glue::glue("You provided: {capitalise_node_labels}.")
      )
    )
  }
}
