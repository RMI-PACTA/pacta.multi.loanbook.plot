#' Plot alignment scatterplot
#'
#' @param data data.frame. Should have the same format as output of
#'   `prep_scatter_animated()` and contain columns: 'name', 'buildout',
#'   'phaseout', 'net' and 'year'.
#' @param data_level Character. Level of the plotted data. Can be 'group_var' or
#'   'company'.
#' @param sector Character. Sector name to be used in the plot title.
#' @param scenario_source Character. Scenario source to be used in the plot
#'   caption.
#' @param scenario Character. Scenario name to be used in the plot caption.
#' @param region Character. Region to be used in the plot caption.
#' @param title Character. Custom title if different than default.
#' @param subtitle Character. Custom subtitle if different than default.
#' @param alignment_limit Numeric. Limit to be applied to the x- and y-axis
#'   scales and to alignment values for colouring. By default the maximum
#'   absolute alignment value from data is used.
#' @param cap_outliers Numeric. Cap which should be applied to the alignment
#'   values in the data. Values bigger than cap are plotted on the border of the
#'   plot.
#' @param floor_outliers Numeric. Floor which should be applied to the alignment
#'   values in the data. Values smaller than floor are plotted on the border of
#'   the plot.
#'
#' @return object of type "plotly"
#' @export
#'
#' @examples
#' # TODO
# nolint start: cyclocomp_linter.
plot_scatter_animated <- function(data,
                                  data_level = c("company", "group_var"),
                                  sector = NULL,
                                  scenario_source = NULL,
                                  scenario = NULL,
                                  region = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  alignment_limit = NULL,
                                  cap_outliers = NULL,
                                  floor_outliers = NULL) {
  rlang::arg_match(data_level)

  caption <- ""
  if (!is.null(scenario_source) || !is.null(scenario) || !is.null(region)) {
    if (!is.null(scenario)) {
      caption <- glue::glue("Scenario: {beautify_scenario_label(scenario)}\n", .trim = FALSE)
    }
    if (!is.null(scenario_source)) {
      caption <- glue::glue("{caption}Scenario source: {beautify_scenario_label(scenario_source)}\n", .trim = FALSE)
    }
    if (!is.null(region)) {
      caption <- glue::glue("{caption}Region: {r2dii.plot::to_title(region)}", .trim = FALSE)
    }
  } else {
    rlang::warn("No information to display in caption provided. Please provide scenario_source and/or scenario and/or region if you want them to be included in the graph", frequency = "once")
  }

  if (is.null(title)) {
    if (!is.null(sector)) {
      title <- glue::glue("Build-out vs. Phase-out Alignment \nin the {r2dii.plot::to_title(sector)} Sector")
    } else {
      title <- "Build-out vs. Phase-out Alignment"
    }
  }

  if (data_level == "company") {
    title <- paste0(title, " per Company")
    if (is.null(subtitle)) {
      subtitle <- "Each dot is a company. The companies in the top right quadrant are both building out\n low-carbon technologies and phasing out high-carbon technologies at rates\ngreater or equal to those required by the scenario."
    }
  } else {
    title <- paste0(title, " per group")
    if (is.null(subtitle)) {
      subtitle <- paste0("Each dot is a group. The groups in the top right quadrant are exposed to companies\nwhich on aggregate level are both building out low-carbon technologies and phasing out\nhigh-carbon technologies at rates greater or equal to those required by the scenario.")
    }
  }

  check_plot_scatter_animated(data, alignment_limit, cap_outliers, floor_outliers)

  if (!is.null(floor_outliers)) {
    data <- data %>%
      dplyr::mutate(
        buildout = dplyr::if_else(.data$buildout <= .env$floor_outliers, .env$floor_outliers, .data$buildout),
        phaseout = dplyr::if_else(.data$phaseout <= .env$floor_outliers, .env$floor_outliers, .data$phaseout),
        net = dplyr::if_else(
          .data$buildout <= .env$floor_outliers | .data$phaseout <= .env$floor_outliers,
          .data$buildout + .data$phaseout,
          .data$net
        ) # net is a sum of buildout and phaseout
      )
    subtitle <- glue::glue("{subtitle}\nThe outliers are displayed on the borders of the plot.", .trim = FALSE)
  }
  if (!is.null(cap_outliers)) {
    data <- data %>%
      dplyr::mutate(
        buildout = dplyr::if_else(.data$buildout >= .env$cap_outliers, .env$cap_outliers, .data$buildout),
        phaseout = dplyr::if_else(.data$phaseout >= .env$cap_outliers, .env$cap_outliers, .data$phaseout),
        net = dplyr::if_else(
          .data$buildout >= .env$cap_outliers | .data$phaseout >= .env$cap_outliers,
          .data$buildout + .data$phaseout,
          .data$net
        ) # net is a sum of buildout and phaseout
      )
    if (is.null(floor_outliers)) {
      subtitle <- glue::glue("{subtitle}\nThe outliers are displayed on the borders of the plot.", .trim = FALSE)
    }
  }

  title <- glue::glue("<b>{title}</b>\n\n<sup>{subtitle}</sup>")

  if (is.null(alignment_limit)) {
    alignment_limit <- max(abs(c(data$buildout, data$phaseout)), na.rm = TRUE)
  }

  alignment_limit_net <- max(c(alignment_limit, abs(data$net)), na.rm = TRUE)

  if ("Benchmark" %in% unique(data$datapoint)) {
    p <- plotly::plot_ly(
      x = ~buildout,
      y = ~phaseout,
      frame = ~year,
      showlegend = TRUE,
      color = ~net,
      colors = grDevices::colorRamp(c("#e10000", "#FFFFFF", "#3d8c40")),
      symbol = ~datapoint,
      symbols = c("circle", "circle", "circle", "circle-open"),
      width = 600,
      height = 800
    )
  } else {
    p <- plotly::plot_ly(
      x = ~buildout,
      y = ~phaseout,
      frame = ~year,
      showlegend = FALSE,
      color = ~net,
      colors = grDevices::colorRamp(c("#e10000", "#FFFFFF", "#3d8c40")),
      symbol = ~datapoint,
      symbols = c("circle", "circle", "circle", "circle-open"),
      width = 600,
      height = 800
    )
  }

  p <- p %>%
    plotly::add_markers(
      data = data,
      text = ~name,
      marker = list(
        autocolorscale = FALSE,
        cmin = -alignment_limit_net,
        cmid = 0,
        cmax = alignment_limit_net
      ),
      hovertemplate = paste(
        "<b>%{text}:</b>",
        "<br>Build-out: %{x}<br>",
        "Phase-out: %{y}<br>",
        "Net: %{marker.color:.0%}"
      ),
    ) %>%
    plotly::add_annotations(
      text = "Aligned buildout,\nAligned phaseout",
      x = 0.99,
      xref = "paper",
      y = 0.99,
      yref = "paper",
      showarrow = FALSE,
      align = "right",
      font = list(color = "#c0c0c0")
    ) %>%
    plotly::add_annotations(
      text = "Misaligned buildout,\nAligned phaseout",
      x = 0.49,
      xref = "paper",
      xanchor = "right",
      y = 0.99,
      yref = "paper",
      showarrow = FALSE,
      align = "right",
      font = list(color = "#c0c0c0")
    ) %>%
    plotly::add_annotations(
      text = "Misaligned buildout,\nMisaligned phaseout",
      x = 0.01,
      xref = "paper",
      y = 0.01,
      yref = "paper",
      showarrow = FALSE,
      align = "left",
      font = list(color = "#c0c0c0")
    ) %>%
    plotly::add_annotations(
      text = "Aligned buildout,\nMisaligned phaseout",
      x = 0.51,
      xref = "paper",
      xanchor = "left",
      y = 0.01,
      yref = "paper",
      showarrow = FALSE,
      align = "left",
      font = list(color = "#c0c0c0")
    ) %>%
    plotly::add_annotations(
      text = caption,
      x = 1,
      xanchor = "right",
      align = "right",
      xref = "paper",
      y = -0.6,
      yref = "paper",
      showarrow = FALSE
    ) %>%
    plotly::add_annotations(
      text = "0% net deviation from scenario",
      x = 0.99,
      xanchor = "right",
      align = "right",
      xref = "paper",
      y = 0.43,
      yanchor = "top",
      yref = "paper",
      showarrow = FALSE,
      font = list(color = "#ffffff"),
      textangle = 45
    ) %>%
    plotly::colorbar(
      limits = c(-alignment_limit_net, alignment_limit_net),
      title = "Net\ndeviation",
      tickformat = ",.0%"
    ) %>%
    plotly::layout(
      title = list(
        text = title,
        font = list(color = "#000"),
        pad = list(b = 20),
        yanchor = "top",
        yref = "container",
        y = 0.95
      ),
      xaxis = list(
        title = list(
          text = "Deviation from scenario value\nfor low-carbon technologies build-out",
          font = list(color = "#000")
        ),
        range = c(-alignment_limit, alignment_limit),
        color = "#c0c0c0",
        tickfont = list(color = "#000"),
        showgrid = FALSE,
        tickformat = ",.0%"
      ),
      yaxis = list(
        title = list(
          text = "Deviation from scenario value\nfor high-carbon technologies phase-out",
          font = list(color = "#000")
        ),
        range = c(-alignment_limit, alignment_limit),
        color = "#c0c0c0",
        tickfont = list(color = "#000"),
        showgrid = FALSE,
        tickformat = ",.0%"
      ),
      plot_bgcolor = "#6c6c6c",
      autosize = FALSE,
      margin = list(l = 0, r = 0, t = 170, b = 250),
      shapes = list(
        list(
          type = "line",
          x0 = -alignment_limit,
          y0 = alignment_limit,
          x1 = alignment_limit,
          y1 = -alignment_limit,
          layer = "below",
          line = list(color = "#ffffff", width = 1)
        )
      )
    )

  p$x$layout$sliders[[1]]$pad$t <- 80
  p$x$layout$updatemenus[[1]]$pad$t <- 80

  p
}
# nolint end

check_plot_scatter_animated <- function(data,
                                        alignment_limit,
                                        cap_outliers,
                                        floor_outliers) {
  abort_if_missing_names(data, c(
    "name", "buildout",
    "phaseout", "net", "year"
  ))
  if (!is.null(alignment_limit)) {
    if ((length(alignment_limit) != 1) || (!is.numeric(alignment_limit))) {
      rlang::abort("'alignment_limit' must be a numeric value.")
    }
  }
  if (!is.null(cap_outliers)) {
    if ((length(cap_outliers) != 1) || (!is.numeric(cap_outliers))) {
      rlang::abort("'cap_outliers' must be a numeric value.")
    }
  }
  if (!is.null(floor_outliers)) {
    if ((length(floor_outliers) != 1) || (!is.numeric(floor_outliers))) {
      rlang::abort("'floor_outliers' must be a numeric value.")
    }
  }
}
