count_daily <- function(.data, .col, cumulative = FALSE) {

  .col <- select_colnames(.data, .col)
  assert_cols(.data, .col, n = 1L)

  .data %>%
    dplyr::count(.data[[.col]]) %>%
    timetk::pad_by_time(.data[[.col]], .by = "day", .pad_value = 0) %>%
    purrr::when(
      rlang::is_true(cumulative) ~ dplyr::mutate(., n = cumsum(.data[["n"]])),
      ~ .
    )
}

count_new_daily <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date()
) {

  collect_nm <- select_colnames(.data, .collection_date)
  report_nm <- select_colnames(.data, .report_date)

  assert_cols(.data, collect_nm, ptype = lubridate::Date(), n = 1L)
  assert_cols(.data, report_nm, ptype = lubridate::Date(), n = 1L)

  start_date <- min(
    .data[[collect_nm]],
    .data[[report_nm]],
    na.rm = TRUE
  )

  end_date <- max(
    .data[[collect_nm]],
    .data[[report_nm]],
    na.rm = TRUE
  )

  .data %>%
    dplyr::transmute(
      .data[[collect_nm]],
      .new = dplyr::if_else(.data[[report_nm]] == today, TRUE, FALSE)
    ) %>%
    dplyr::filter(.data[[".new"]]) %>%
    dplyr::count(.data[[collect_nm]]) %>%
    timetk::pad_by_time(
      .data[[collect_nm]],
      .by = "day",
      .pad_value = 0L,
      .start_date = start_date,
      .end_date = end_date
    )
}
