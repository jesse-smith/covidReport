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

  # Prep data for plotting - output is `report_date` and cumulative `n`

  prep_cumulative_data(data, min_date = min_date, date = date) %>%
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

prep_cumulative_data <- function(data, min_date, date) {

  # Coerce dates
  min_date <- lubridate::as_date(min_date)
  date <- lubridate::as_date(date)

  # Load report dates
  inv_report_dates <- dplyr::as_tibble(coviData::load_report_date())

  gg_data <- data %>%
    dplyr::left_join(inv_report_dates, by = "inv_local_id") %>%
    dplyr::mutate(report_date = lubridate::as_date(.data[["report_date"]])) %>%
    dplyr::filter(
      {{ min_date }} <= .data[["report_date"]],
      .data[["report_date"]] <= {{ date }}
    ) %>%
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

  spline(x = x_in, y = y_in, xout = x_out) %>%
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

set_cumulative_theme <- function(gg_obj) {
  set_covid_theme(gg_obj) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
}

add_cumulative_scale <- function(gg_obj) {

  breaks <- seq(0L, 1e6L, by = 1e4L)

  labels <- breaks %>% divide_by(1e3L) %>% paste0("k")

  add_scale_month(gg_obj) +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels
    )
}

add_cumulative_curve <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_col(
      fill = "midnightblue",
      width = 1,
      show.legend = FALSE
    )
}

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

add_cumulative_axis_labels <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Report Date", ylab = "Cumulative Cases")
}

add_cumulative_title_caption <- function(gg_obj, date) {
  add_title_caption(
    gg_obj,
    title = "Cumulative COVID-19 Cases by Report Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y")
  )
}
