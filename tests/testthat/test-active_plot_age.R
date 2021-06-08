test_that("`active_plot_age()` matches doppelganger", {
  tbl_mock <- mockery::mock(tibble::tribble(
    ~ age_grp, ~ n, ~ rate, ~ percent,
    "0-17", 100,    0.2,       0.1,
    "18-24", 100,    0.1,       0.1,
    "25-34", 100,    0.1,       0.1,
    "35-44", 100,    0.1,       0.1,
    "45-54", 100,    0.1,       0.1,
    "55-64", 100,    0.1,       0.1,
    "65-74", 100,    0.1,       0.1,
    "75-84", 100,    0.1,       0.1,
    "85+", 100,    0.1,       0.1,
  ))

  mockery::stub(
    active_plot_age,
    "active_calc_age",
    tbl_mock
  )

  mockery::stub(
    active_plot_age,
    "date_inv",
    lubridate::as_date
  )

  plt <- active_plot_age(tibble::tibble(), date = "2021-04-01")

  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "active case rate by age",
      fig = plt,
      path = "active-plot-age"
    )
  )
})
