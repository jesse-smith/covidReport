test_that("`vac_plot_age()` matches doppelganger", {
  data <- tibble::tibble(
    asiis_pat_id_ptr = 1:1e5,
    resident = c(rep(TRUE, 9e4), rep(FALSE, 1e4)),
    age_at_admin = rep(1:100, 1e3),
    vacc_date = c(rep(lubridate::NA_Date_, 99999), as.Date("2021-04-19")),
    dose_count = c(rep(1, 1e4), rep(2, 9e4)),
    recip_fully_vacc = c(rep(TRUE, 3e4), rep(FALSE, 7e4))
  ) %>% dplyr::slice_sample(prop = 1)

  suppressWarnings(
    vdiffr::expect_doppelganger(
      "vaccination rate by age (no under 12)",
      vac_plot_age(data),
      path = "vac-age-pop-12plus"
    )
  )

  suppressWarnings(
    vdiffr::expect_doppelganger(
      "vaccination distribution by age (no under 12)",
      vac_plot_age(data, by_pop = FALSE),
      path = "vac-age-distr-12plus"
    )
  )

  suppressWarnings(
    vdiffr::expect_doppelganger(
      "vaccination rate by age (w/ under 12)",
      vac_plot_age(data, incl_under_12 = TRUE),
      path = "vac-age-rate-under12"
    )
  )

  suppressWarnings(
    vdiffr::expect_doppelganger(
      "vaccination distribution by age (w/ under 12)",
      vac_plot_age(data, by_pop = FALSE, incl_under_12 = TRUE),
      path = "vac-age-distr-under12"
    )
  )

})
