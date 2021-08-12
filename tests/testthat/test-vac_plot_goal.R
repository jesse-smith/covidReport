test_that("`vac_plot_goal()` matches doppelganger", {
  data <- tibble::tibble(
    asiis_pat_id_ptr = 1:1e5,
    resident = c(rep(TRUE, 9e4), rep(FALSE, 1e4)),
    dose_count = c(rep(1, 1e4), rep(2, 9e4)),
    recip_fully_vacc = c(rep(TRUE, 3e4), rep(FALSE, 7e4)),
    max_doses = dplyr::if_else(recip_fully_vacc, dose_count, dose_count + 1)
  ) %>% dplyr::slice_sample(prop = 1)

  mockery::stub(
    vac_plot_goal,
    "date_vac",
    lubridate::as_date
  )

  suppressWarnings(
    vdiffr::expect_doppelganger(
      "vaccination goal",
      vac_plot_goal(data, date = "2021-04-19"),
      path = "vac-goal"
    )
  )
})
