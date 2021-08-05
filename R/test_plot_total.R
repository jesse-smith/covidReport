#' Plot 7-Day Rolling Average PCR Tests
#'
#' @param data PCR test data, read from an NBS snapshot file
#'
#' @param date The download date of the data to read; defaults to most recent
#'
#' @param delay `integer`. The reporting delay of the data; test dates within
#'   `delay` days of `date` will be omitted
#'
#' @return A `gt_tbl`
#'
#' @export
test_plot_total <- function(
  data = process_pcr(read_pcr(date)),
  date = NULL,
  delay = 5L
) {
  # Get report date
  date <- date_pcr(date)
  gg_data <- prep_test_total(data, date = date, delay = delay)

  gg_data %>%
    ggplot_test_total() %>%
    coviData::set_covid_theme() %>%
    add_test_total_curve() %>%
    add_axis_labels(ylab = "PCR Tests") %>%
    add_scale_month() %>%
    add_test_total_label() %>%
    add_test_total_scale() %>%
    add_test_total_title_caption(
      n_obs     = attr(gg_data, "n_obs"),
      n_missing = attr(gg_data, "n_missing"),
      date      = date
    )
}

#' Prepare NBS Test Data for Plotting
#'
#' @inheritParams test_plot_total
#'
#' @return A `tibble` with columns `test_date` (`Date`), `n` (`int`), and
#'   `avg` (`dbl`), as well as attributes `n_obs` (total tests) and `n_missing`
#'   (missing `test_date`s)
#'
#' @noRd
prep_test_total <- function(data, min_date = "2020-03-14", date, delay) {
  data_b <- dplyr::bind_rows(
    dplyr::select(pos(data), "specimen_coll_dt"),
    dplyr::select(neg(data), "specimen_coll_dt")
  )
  remove(data)

  data_dt <- data_b %>%
    dplyr::transmute(
      test_date = coviData::std_dates(
        .data[["specimen_coll_dt"]],
        orders = c("ymdT", "ymd"),
        train = FALSE,
        force = "dt"
      )
    ) %>%
    dplyr::filter(
      as.Date("2020-03-05") <= .data[["test_date"]],
      .data[["test_date"]] <= lubridate::as_date({{ date }})
    )

  n_obs <- vec_size(data_b)
  n_missing <- n_obs - vec_size(data_dt)
  remove(data_b)
  gc(verbose = FALSE)

  data_dt %>%
    dplyr::count(.data[["test_date"]]) %>%
    tidyr::complete(
      "test_date" = seq(
        lubridate::as_date("2020-03-05"),
        lubridate::as_date(date),
        by = 1L
      ),
      fill = list(n = 0L)
    ) %>%
    timetk::tk_augment_slidify(
      "n",
      .period = 7L,
      ~ mean(.x, na.rm = TRUE),
      .align = "right",
      .partial = TRUE,
      .names = "avg"
    ) %>%
    dplyr::filter(
      lubridate::as_date({{ min_date }}) <= .data[["test_date"]],
      .data[["test_date"]] <= {{ date }} - {{ delay }}
    ) %>%
    tibble::new_tibble(
      n_obs = n_obs,
      n_missing = n_missing,
      nrow  = vec_size(.),
      class = "n_tbl"
    ) %>%
    tibble::validate_tibble()
}

#' Initialize `ggplot()` for PCR Tests
#'
#' @param data Data prepared for plotting by `prep_test_pos()`
#'
#' @return A `ggplot` object
#'
#' @noRd
ggplot_test_total <- function(data) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[["test_date"]], y = .data[["avg"]])
  )
}

#' Add Columns for Average PCR Tests
#'
#' @param gg_obj A ggplot object w/ data from `prep_test_pos()`
#'
#' @return The `gg_obj` with a `geom_col()` added
#'
#' @noRd
add_test_total_curve <- function(gg_obj) {
  gg_obj + ggplot2::geom_col(fill = "midnightblue", width = 1)
}

#' Add Label for Latest Average PCR Tests
#'
#' @param gg_obj A ggplot object w/ data from `prep_test_total()`
#'
#' @return The `gg_obj` with a label added at the last observed date
#'
#' @noRd
add_test_total_label <- function(gg_obj) {

  last_obs <- dplyr::slice_tail(gg_obj[["data"]], n = 1L)

  gg_obj + ggplot2::annotate(
    "text",
    x = last_obs[["test_date"]],
    y = last_obs[["avg"]] + 0.001,
    label = as.integer(last_obs[["avg"]]),
    fontface = "bold",
    color = "grey30",
    hjust = 0,
    vjust = 0
  )
}

add_test_total_scale <- function(gg_obj) {
  gg_obj + ggplot2::scale_y_continuous(breaks = seq(0, 1e4, by = 500))
}

#' Add Title, Subtitle, and Caption for PCR Test Totals
#'
#' @param gg_obj A ggplot object plotting test totals
#'
#' @param n_obs The total number of tests performed
#'
#' @param n_missing The number of tests missing specimen collection date
#'
#' @param date The download date of the data used to create the plot
#'
#' @return The `gg_obj` with title, a subtitle containing the date, and a
#'   caption containing total and missing observations added
#'
#' @noRd
add_test_total_title_caption <- function(gg_obj, n_obs, n_missing, date) {

  obs <- format(n_obs, big.mark = ",")
  missing <- format(n_missing, big.mark = ",")

  coviData::add_title_caption(
    gg_obj,
    title = "COVID-19 PCR Tests (7-Day Rolling Average)",
    subtitle = format(lubridate::as_date(date), "%m/%d/%y"),
    caption = paste0(
      "Note: Chart excludes tests with missing collection date ",
      "(N = ", missing, "; Total = ", obs, ")\n",
      "Data Source: National Electronic Disease Surveillance System (NEDSS)"
    )
  )
}
