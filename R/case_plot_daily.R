#' Plot Daily New Cases by Specimen Collection Date
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param date The report date of the data; defaults to the most recent date
#'
#' @param delay Number of days to ignore 7 day average (due to incomplete data);
#'   default is calculated using
#'   \code{\link[covidModel:estimate_delay]{estimate_delay()}}
#'
#' @return A `ggplot` object
#'
#' @export
case_plot_daily <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL,
  delay = NULL
) {

  min_date <- lubridate::as_date("2020-03-08")

  # Date for current (and previous) counts
  date <- date_inv(date)

  if (is.null(delay)) {
    rpt_data <- dplyr::as_tibble(coviData::load_report_date())

    complete_date <- data %>%
      dplyr::left_join(rpt_data, by = "inv_local_id") %>%
      dplyr::mutate(
        collection_date = lubridate::as_date(.data[["collection_date"]])
      ) %>%
      covidModel::estimate_delay(today = date) %>%
      dplyr::pull("collection_date")

    delay <- date - complete_date
  }

  # Label numbers
  n_total <- NROW(data)
  n_prev <- NROW(read_inv_id(date = date - 1L))
  n_new <- n_total - n_prev

  gg_data <- prep_daily_data(
    data,
    min_date = min_date,
    date = date,
    delay = delay
  )

  n_plotted <- sum(gg_data[["n"]], na.rm = TRUE)
  n_missing <- n_total - n_plotted

  gg_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["test_date"]], y = .data[["n"]])
    ) %>%
    set_ts_theme() %>%
    add_daily_scale() %>%
    add_daily_curve() %>%
    add_covid_events(lab_y = 1200L, color = "grey60", size = 3) %>%
    add_daily_label(total = n_total, new = n_new) %>%
    add_daily_axis_labels() %>%
    add_daily_title_caption(date = date, missing = n_missing)
}

#' Prepare Data for Plotting Daily New Cases
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param min_date Minimum plotting date
#'
#' @param date Report date
#'
#' @param delay Number of days to ignore moving average (due to incomplete data)
#'
#' @return A `tibble` with columns `report_date`, `n`, and `avg`
#'
#' @noRd
prep_daily_data <- function(data, min_date, date, delay) {
  data %>%
    dplyr::transmute(
      id = .data[["inv_local_id"]],
      test_date = coviData::std_dates(
        .data[["specimen_coll_dt"]],
        orders = c("ymdT", "ymdHM", "ymd"),
        train = FALSE,
        force = "dt"
      )
    ) %>%
    dplyr::filter(
      {{ min_date }} <= .data[["test_date"]],
      .data[["test_date"]] <= {{ date }}
    ) %>%
    dplyr::arrange(.data[["test_date"]], .data[["id"]]) %>%
    dplyr::distinct(.data[["id"]], .keep_all = TRUE) %>%
    dplyr::count(.data[["test_date"]]) %>%
    dplyr::arrange(.data[["test_date"]]) %>%
    tidyr::complete(
      "test_date" = seq(min_date, date, by = 1L),
      fill = list(n = 0L)
    ) %>%
    timetk::tk_augment_slidify(
      .data[["n"]],
      .period = 7L,
      .f = mean,
      na.rm = TRUE,
      .align = "right",
      .names = "avg"
    ) %>%
    dplyr::mutate(
      avg = vec_assign(
        .data[["avg"]],
        i = (NROW(.) - delay + 1L):NROW(.),
        value = NA_real_
      )
    )
 }

#' Set Theme for Time Series Plots
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
set_ts_theme <- function(gg_obj) {
  set_covid_theme(gg_obj) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
}

#' Add x- and y-axis Scales to Daily Case Plot
#'
#' Adds x-axis scale with monthly breaks using
#' \code{\link[coviData:add_scale_month]{add_scale_month()}} and y-axis scale
#' with breaks by 100.
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with scales set
#'
#' @noRd
add_daily_scale <- function(gg_obj) {

  breaks <- seq(0L, 3500, by = 2e2L)

  label_fn <- rlang::as_function(~ format(.x, big.mark = ","))

  add_scale_month(gg_obj) +
    ggplot2::scale_y_continuous(breaks = breaks, labels = label_fn)
}

#' Add Daily Case Curves to Plot
#'
#' Adds a \code{\link[ggplot2:geom_col]{geom_col()}} curve and a
#' \code{\link[ggplot2:geom_line]{geom_line()}} curve to the plot
#'
#' @param gg_obj A `ggplot` object
#'
#' @param The `ggplot` object with the added geom
#'
#' @noRd
add_daily_curve <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_col(
      fill = "midnightblue",
      width = 1,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["avg"]]),
      color = "darkorange",
      size = 1.25,
      show.legend = FALSE
    )
}

#' Add Label to Daily Case Plot
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
add_daily_label <- function(gg_obj, total, new) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    "Total Cases = ", format(total, big.mark = ","), "\n",
    " New Reported Cases = ", format(new, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = 1600L,
      label = label,
      color = "midnightblue",
      fill = "#f0f0f0",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      label.size = 1
    )
}

#' Add Axis Labels to Daily Case Plot
#'
#' Adds `"Specimen Collection Date"` x-axis label and `"New Cases"` y-axis label
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with added axis labels
#'
#' @noRd
add_daily_axis_labels <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Specimen Collection Date", ylab = "New Cases")
}

#' Add Title, Subtitle, and Caption to Daily Case Plot
#'
#' Adds title `"New COVID-19 Cases by Specimen Collection Date"`, a subtitle
#' displaying the report date, and a caption stating number missing and data
#' source
#'
#' @param gg_obj A `ggplot` object
#'
#' @param date The report date to add as subtitle
#'
#' @param missing Number of observations missing from graphic
#'
#' @return The `ggplot` object with added title and subtitle
#'
#' @noRd
add_daily_title_caption <- function(gg_obj, date, missing) {

  caption <- paste0(
    "Excludes cases with missing specimen collection dates ",
    "(N = ", format(missing, big.mark = ","), ")\n",
    "Data Source: National Electronic Disease Surveillance System (NEDSS)"
  )

  add_title_caption(
    gg_obj,
    title = "New COVID-19 Cases by Specimen Collection Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = caption
  )
}
