#' Plot 7-Day Rolling Test Positivity Rate
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
test_plot_positivity <- function(
  data = process_pcr(read_pcr(date)),
  date = NULL,
  delay = 5L
) {

  # Get report date
  date <- date_pcr(date)

  gg_data <- prep_test_pos(data, date = date, delay = delay)

  # Prep data
  prep_test_pos(data, date = date, delay = delay) %>%
    ggplot_test_positivity() %>%
    coviData::set_covid_theme() %>%
    add_test_pos_curve() %>%
    add_axis_labels(ylab = "% Positive") %>%
    add_scale_month() %>%
    add_covid_events(lab_y = 0.25, color = "grey60") %>%
    add_test_pos_label() %>%
    add_test_pos_scale() %>%
    add_test_pos_title_caption(
      n_obs   = attr(gg_data, "n_obs"),
      n_missing = attr(gg_data, "n_missing"),
      date    = date
    )
}

#' Convert an NBS Test Dataset to A Count Time Series
#'
#' @inheritParams test_plot_positivity
#'
#' @param status Should positive or negative test counts be returned?
#'
#' @return A `tibble` with columns `test_date` (`Date`) and `n` (`int`), as well
#'   as attributes `n_obs` (total positive/negative observations) and
#'   `n_missing` (missing `test_date`s)
#'
#' @noRd
prep_test_ts <- function(data, date, status = c("+", "-")) {

  status <- rlang::arg_match(status)[[1L]]

  if (status == "+") {
    status_full <- "positive"
    min_dt <- lubridate::as_date("2020-03-05")
  } else {
    status_full <- "negative"
    min_dt <- lubridate::as_date("2020-03-06")
  }

  data_status <- purrr::when(
    data,
    status == "+" ~ pos(data),
    status == "-" ~ neg(data),
    ~ rlang::abort("`status` must be '+' or '-'")
  )
  remove(data)
  gc(verbose = FALSE)

  data_dt <- data_status %>%
    dplyr::transmute(
      test_date = coviData::std_dates(
        .data[["specimen_coll_dt"]],
        orders = c("ymdT", "ymd"),
        train = FALSE,
        force = "dt"
      )
    ) %>%
    dplyr::filter(
      {{ min_dt }} <= .data[["test_date"]],
      .data[["test_date"]] <= lubridate::as_date({{ date }})
    )

  n_obs     <- vec_size(data_status)
  n_missing <- n_obs - vec_size(data_dt)
  remove(data_status)
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
    dplyr::rename({{ status_full }} := "n") %>%
    tibble::new_tibble(
      n_obs = n_obs,
      n_missing = n_missing,
      nrow  = vec_size(.),
      class = "n_tbl"
    ) %>%
    tibble::validate_tibble()
}

#' Prepare NBS Test Data for Plotting
#'
#' @inheritParams test_plot_positivity
#'
#' @return A `tibble` with columns `test_date` (`Date`), `positive` (`int`),
#'   `negative` (`int`), `total` (`int`), `pct_pos` (`dbl`), and `avg` (`dbl`),
#'   as well as attributes `n_obs` (total tests) and `n_missing`
#'   (missing `test_date`s)
#'
#' @noRd
prep_test_pos <- function(data, date, delay) {
  positive <- prep_test_ts(data, date = date, status = "+")
  negative <- prep_test_ts(data, date = date, status = "-")
  remove(data)
  gc(verbose = FALSE)

  n_obs   <- attr(positive, "n_obs")     + attr(negative, "n_obs")
  n_missing <- attr(positive, "n_missing") + attr(negative, "n_missing")

  dplyr::left_join(positive, negative, by = "test_date") %>%
    dplyr::mutate(
      total   = .data[["positive"]] + .data[["negative"]],
      pct_pos = .data[["positive"]] / .data[["total"]],
      avg = purrr::map2_dbl(
        1:(dplyr::n()-6),
        7:dplyr::n(),
        ~ magrittr::divide_by(
          sum(.data[["positive"]][.x:.y], na.rm = TRUE),
          sum(.data[["total"]][.x:.y], na.rm = TRUE)
        )
      ) %>% purrr::prepend(rep(NA_real_, 6L))
    ) %>%
    dplyr::filter(
      lubridate::as_date("2020-03-14") <= .data[["test_date"]],
      .data[["test_date"]] <= {{ date }} - {{ delay }}
    ) %>%
    tibble::new_tibble(
      nrow = vec_size(.),
      n_obs = n_obs,
      n_missing = n_missing,
      class = "n_tbl"
    ) %>%
    tibble::validate_tibble()
}

#' Initialize `ggplot()` for Test Positivity
#'
#' @param data Data prepared for plotting by `prep_test_pos()`
#'
#' @return A `ggplot` object
#'
#' @noRd
ggplot_test_positivity <- function(data) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[["test_date"]], y = .data[["avg"]])
  )
}

#' Add Columns for Test Positivity
#'
#' @param gg_obj A ggplot object w/ data from `prep_test_pos()`
#'
#' @return The `gg_obj` with a `geom_col()` added
#'
#' @noRd
add_test_pos_curve <- function(gg_obj) {
  gg_obj + ggplot2::geom_col(fill = "midnightblue", width = 1)
}

#' Add Label for Latest Test Positivity Observation
#'
#' @param gg_obj A ggplot object w/ data from `prep_test_pos()`
#'
#' @return The `gg_obj` with a label added at the last observed date
#'
#' @noRd
add_test_pos_label <- function(gg_obj) {

  last_obs <- dplyr::slice_tail(gg_obj[["data"]], n = 1L)

  gg_obj + ggplot2::annotate(
    "text",
    x = last_obs[["test_date"]],
    y = last_obs[["avg"]] + 0.001,
    label = scales::percent(last_obs[["avg"]], accuracy = 0.1),
    fontface = "bold",
    color = "grey30",
    hjust = 0,
    vjust = 0
  )
}

#' Add Y Scale for Test Positivity
#'
#' @param gg_obj A ggplot object w/ data from `prep_test_pos()`
#'
#' @return The `gg_obj` with a percent scale on the y axis
#'
#' @noRd
add_test_pos_scale <- function(gg_obj) {

  pct_fn <- rlang::as_function(~ scales::percent(.x, accuracy = 1))
  gg_obj + ggplot2::scale_y_continuous(labels = pct_fn)
}

#' Add Title, Subtitle, and Caption for Test Positivity
#'
#' @param gg_obj A ggplot object plotting test positivity
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
add_test_pos_title_caption <- function(gg_obj, n_obs, n_missing, date) {

  obs <- format(n_obs, big.mark = ",")
  missing <- format(n_missing, big.mark = ",")

  coviData::add_title_caption(
    gg_obj,
    title = "COVID-19 Test Positivity Rate (7-Day Rolling)",
    subtitle = format(lubridate::as_date(date), "%m/%d/%y"),
    caption = paste0(
      "Note: Chart excludes tests with missing collection date ",
      "(N = ", missing, "; Total = ", obs, ")\n",
      "Data Source: National Electronic Disease Surveillance System (NEDSS)"
    )
  )
}
