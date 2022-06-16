#' Plot Cumulative Case Time Series
#'
#' @param data Case data (by person) from NBS, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param date The download date of `data`; the default is the most recent
#'
#' @return A `ggplot` object
#'
#' @export
case_plot_cumulative_week <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL
) {

  # Minimum plot date
  min_date <- lubridate::as_date("2020-03-08")

  # Date for current (and previous) counts
  date <- date_inv(date)

  # Label numbers
  n_total <- NROW(data)
  n_prev <- NROW(read_inv_id(date = date - 7L))
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
    add_cumulative_weekly_label(total = n_total, new = n_new) %>%
    add_cumulative_axis_labels() %>%
    add_cumulative_title_caption(date = date)
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
add_cumulative_weekly_label <- function(gg_obj, total, new) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    "Total Cases = ", format(total, big.mark = ","), "\n",
    "7-Day New Reported Cases = ", format(new, big.mark = ",")
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













