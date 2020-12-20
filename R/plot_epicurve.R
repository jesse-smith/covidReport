plot_epicurve <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date()
) {

  collect_nm <- select_colnames(.data, .collection_date)
  report_nm <- select_colnames(.data, .report_date)

  gg_data <- .data %>%
    check_linelist_dates(
      .collection_date = collect_nm,
      .report_date = report_nm,
      today = today
    ) %>%
    count_epicurve(
      .collection_date = collect_nm,
      .report_date = report_nm,
      today = today
    )

  remove(.data)

  gg_data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[".t"]])) %>%
    set_covid_theme() %>%
    add_epicurve_scale()

}

count_epicurve <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date()
) {
  collection_counts <- .data %>%
    count_daily(.collection_date) %>%
    dplyr::transmute(.t = .data[[.collection_date]], collected = .data[["n"]])
  new_counts <- .data %>%
    count_new_daily(.collection_date, .report_date, today = today) %>%
    dplyr::transmute(.t = .data[[.collection_date]], new = .data[["n"]])

  dplyr::full_join(
    collection_counts,
    new_counts,
    by = ".t"
  )
}

add_epicurve_scale <- function(gg_obj) {

  breaks <- seq(0L, 1e4L, by = 1e2L)

  labels <- format(breaks, big.mark = ",")

  add_scale_month(gg_obj) +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels
    )
}

add_epicurve_bars <- function(gg_obj) {

}
