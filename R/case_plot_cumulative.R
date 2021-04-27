#' Plot Cumulative Case Time Series
#'
#' @param data Case data (by person) from NBS, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param date The download date of `data`; the default is the most recent
#'
#' @return A `ggplot` object
#'
#' @export
case_plot_cumulative <- function(
  data = coviData::process_positive_people(date = date),
  date = NULL
) {

  # Minimum plot date
  min_date <- lubridate::as_date("2020-03-08")

  # Date for current (and previous) counts
  if (is.null(date)) {
    date <- coviData::path_inv() %>%
      fs::path_file() %>%
      fs::path_ext_remove() %>%
      stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
      lubridate::as_date()
  } else {
    date <- lubridate::as_date(date)
  }

  # Label numbers
  n_total <- NROW(data)
  n_prev <- NROW(coviData::process_positive_people(date = date - 1L))
  n_new <- n_total - n_prev

  # Loading report dates in `case_plot_cumulative()` rather than
  # `prep_cumulative_data()` due to limitations in testing
  rpt_data <- dplyr::as_tibble(coviData::load_report_date())

  data %>%
    prep_cumulative_data(
      min_date = min_date,
      date = date,
      rpt_data = rpt_data
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["report_date"]], y = .data[["n"]])
    ) %>%
    set_cumulative_theme() %>%
    add_cumulative_scale() %>%
    add_cumulative_curve() %>%
    add_cumulative_label(total = n_total, new = n_new) %>%
    add_cumulative_axis_labels() %>%
    add_cumulative_title_caption(date = date)
}

#' Prepare Case Data for Cumulative Plotting
#'
#' `prep_cumulative_data` merges case data with report dates and calculates
#' the \code{\link[base:cumsum]{cumsum()}} for each report date (implicitly
#' missing dates are made explicit, and all missing daily counts are set to `0`
#' before taking the cumulative sum). For data that start after `min_date`,
#' cumulative counts are interpolated from `1` to the count at the minimum
#' report date.
#'
#' @inheritParams case_plot_cumulative
#'
#' @param min_date The minimum date to be plotted
#'
#' @param rpt_data `tibble` of `inv_local_id` and `report_date`
#'
#' @return A `tibble` with columns `report_date` (as `Date`) and `n`
#'   (cumulative counts, incl. interpolated values)
#'
#' @noRd
prep_cumulative_data <- function(
  data,
  min_date,
  date,
  rpt_data = dplyr::as_tibble(coviData::load_report_date())
) {

  # Coerce dates
  min_date <- lubridate::as_date(min_date)
  date <- lubridate::as_date(date)

  gg_data <- data %>%
    dplyr::left_join(rpt_data, by = "inv_local_id") %>%
    dplyr::mutate(report_date = lubridate::as_date(.data[["report_date"]])) %>%
    dplyr::filter(
      {{ min_date }} <= .data[["report_date"]],
      .data[["report_date"]] <= {{ date }}
    ) %>%
    dplyr::arrange(.data[["report_date"]], .data[["inv_local_id"]]) %>%
    dplyr::distinct(.data[["inv_local_id"]], .keep_all = TRUE) %>%
    dplyr::count(.data[["report_date"]]) %>%
    tidyr::complete(
      "report_date" = seq(
        min(.data[["report_date"]], na.rm = TRUE),
        max(.data[["report_date"]], na.rm = TRUE),
        by = 1L
      ),
      fill = list(n = 0L)
    ) %>%
    dplyr::mutate(n = cumsum(.data[["n"]]))

  # Need to ensure data starts at `min_date`
  min_report_date <- min(gg_data[["report_date"]], na.rm = TRUE)

  # Data is already limited to dates after `min_date`
  # Return if no further transformation is needed
  if (min_report_date <= min_date) return(gg_data)

  # Otherwise, interpolate from `min_date` to `min_report_date`
  min_n <- min(gg_data[["n"]], na.rm = TRUE)
  x_in  <- c(min_date, min_report_date)
  y_in  <- c(1L, min_n)
  x_out <- seq(x_in[[1L]], x_in[[2L]] - 1L, by = 1L)

  stats::spline(x = x_in, y = y_in, xout = x_out) %>%
    dplyr::as_tibble() %>%
    dplyr::transmute(
      report_date = lubridate::as_date(.data[["x"]]),
      n = as.integer(round(.data[["y"]]))
    ) %>%
    dplyr::bind_rows(gg_data) %>%
    dplyr::arrange(dplyr::desc(dplyr::row_number())) %>%
    dplyr::distinct(.data[["report_date"]], .keep_all = TRUE) %>%
    dplyr::arrange(.data[["report_date"]])
}

#' Set Theme for Cumulative Case Plot
#'
#' Sets ggplot2 theme using
#' \code{\link[coviData:set_covid_theme]{set_covid_theme()}} and rotates
#' x-axis labels by 45 degrees.
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with adjusted theme
#'
#' @noRd
set_cumulative_theme <- function(gg_obj) {
  set_covid_theme(gg_obj) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
}

#' Add x- and y-axis Scales to Cumulative Case Plot
#'
#' Adds x-axis scale with monthly breaks using
#' \code{\link[coviData:add_scale_month]{add_scale_month()}} and y-axis scale
#' with 10k breaks.
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with scales set
#'
#' @noRd
add_cumulative_scale <- function(gg_obj) {

  breaks <- seq(0L, 1e6L, by = 1e4L)

  labels <- breaks %>% divide_by(1e3L) %>% paste0("k")

  add_scale_month(gg_obj) +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels
    )
}

#' Add Cumulative Case Curve to Plot
#'
#' Adds a \code{\link[ggplot2:geom_col]{geom_col()}} curve to the plot
#'
#' @param gg_obj A `ggplot` object
#'
#' @param The `ggplot` object with the added geom
#'
#' @noRd
add_cumulative_curve <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_col(
      fill = "midnightblue",
      width = 1,
      show.legend = FALSE
    )
}

#' Add Label to Cumulative Case Plot
#'
#' Adds a label in the upper-left with 'Total' and 'New' cases
#'
#' @param gg_obj A `ggplot` object
#'
#' @param total Total cases in data
#'
#' @param new New cases for report date
#'
#' @return The `ggplot` object with the added label
#'
#' @noRd
add_cumulative_label <- function(gg_obj, total, new) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    "Total Cases = ", format(total, big.mark = ","), "\n",
    " New Cases = ", format(new, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = total,
      label = label,
      color = "midnightblue",
      fill = "#f0f0f0",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      label.size = 1
    )
}

#' Add Axis Labels to Cumulative Case Plot
#'
#' Adds `"Report Date"` x-axis label and `"Cumulative Cases"` y-axis label
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with added axis labels
#'
#' @noRd
add_cumulative_axis_labels <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Report Date", ylab = "Cumulative Cases")
}

#' Add Title and Subtitle to Cumulative Case Plot
#'
#' Adds title `"Cumulative COVID-19 Cases by Report Date"` and a subtitle
#' displaying the report date
#'
#' @param gg_obj A `ggplot` object
#'
#' @param date The report date to add as subtitle
#'
#' @return The `ggplot` object with added title and subtitle
#'
#' @noRd
add_cumulative_title_caption <- function(gg_obj, date) {
  add_title_caption(
    gg_obj,
    title = "Cumulative COVID-19 Cases by Report Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y")
  )
}
