test_that("`case_plot_cumulative()` returns expected graphic", {
  mockery::stub(
    case_plot_cumulative,
    "coviData::pos",
    tibble::tibble(.rows = 93895L)
  )

  mockery::stub(
    case_plot_cumulative,
    "coviData::load_report_date",
    readRDS(testthat::test_path("../data/test_report_data.rds"))
  )

  mockery::stub(
    case_plot_cumulative,
    "coviData::date_inv",
    lubridate::as_date
  )

  plt <- case_plot_cumulative(
    readRDS(testthat::test_path("../data/test_cumulative_data.rds")),
    date = "2021-04-19"
  )

  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "cumulative cases",
      fig = plt,
      path = "case-plot-cumulative"
    )
  )
})
