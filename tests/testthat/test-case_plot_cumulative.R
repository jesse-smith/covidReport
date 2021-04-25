test_that("`case_plot_cumulative()` returns expected graphic", {
  mockery::stub(
    case_plot_cumulative,
    "coviData::process_positive_people",
    tibble::tibble(.rows = 93895L)
  )

  mockery::stub(
    case_plot_cumulative,
    "coviData::load_report_date",
    covidReport::test_report_data
  )

  plt <- case_plot_cumulative(
    covidReport::test_cumulative_data,
    date = "2021-04-19"
  )

  vdiffr::expect_doppelganger(title = "cumulative cases", fig = plt)
})
