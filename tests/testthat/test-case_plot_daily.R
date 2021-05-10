test_that("`case_plot_daily()` matches doppelganger", {
  mockery::stub(
    case_plot_daily,
    "coviData::process_positive_people",
    tibble::tibble(.rows = 93895L)
  )

  report_date <- readRDS(testthat::test_path("../data/test_report_data.rds"))

  mockery::stub(
    case_plot_daily,
    "coviData::load_report_date",
    report_date
  )

  data <- dplyr::left_join(
    readRDS(testthat::test_path("../data/test_cumulative_data.rds")),
    report_date,
    by  = "inv_local_id"
  ) %>%
    dplyr::transmute(
      .data[["inv_local_id"]],
      specimen_coll_dt = lubridate::as_date(.data[["collection_date"]])
    )

  plt <- suppressMessages(case_plot_daily(data, date = "2021-04-19"))

  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "daily cases",
      fig = plt,
      path = "case-plot-daily"
    )
  )
})
