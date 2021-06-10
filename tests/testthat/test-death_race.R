test_that("`death_plot_race()` matches doppelganger", {
  tbl_mock <- tibble::tribble(
    ~ grp, ~ n, ~ percent, ~ rate,
    "Black/African American", 500,       0.5,    0.1,
    "White", 300,       0.3,    0.1,
    "Other", 100,       0.1,    0.1,
    "Missing", 100,       0.1,    0.1,
  ) %>%
    dplyr::mutate(grp = forcats::as_factor(grp)) %>%
    mockery::mock()

  mockery::stub(
    death_plot_race,
    "death_calc_race",
    tbl_mock
  )

  m_dt <- mockery::mock(as.Date("2021-04-01"), cycle = TRUE)
  mockery::stub(
    death_plot_race,
    "date_inv",
    m_dt
  )

  plt <- death_plot_race(tibble::tibble(), date = "2021-04-01")

  mockery::expect_called(m_dt, n = 1L)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "death case rate by race",
      fig = plt,
      path = "death-plot-race"
    )
  )
})

test_that("`void(death_table_race())` matches snapshot", {
  tbl_mock <- mockery::mock(tibble::tribble(
    ~ grp, ~ n, ~ percent, ~ rate,
    "Black/African American", 500,       0.5,    0.1,
    "White", 300,       0.3,    0.1,
    "Other", 100,       0.1,    0.1,
    "Missing", 100,       0.1,    0.1,
  ))

  mockery::stub(
    death_table_race,
    "death_calc_race",
    tbl_mock
  )

  data <- tibble::tibble()

  tbl_void <- flextable::void(
    death_table_race(data, date = "2021-04-01"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`death_calc_race()` matches snapshot", {

  data <- tibble::tibble(
    die_from_illness_ind = rep(
      c("Y", rep("N", 3L), rep("U", 3L), rep(NA, 3L)),
      1e4L
    ),
    inv_death_dt = seq(
      as.Date("2020-03-26"),
      as.Date("2021-03-26"),
      length.out = 99999L
    ) %>% purrr::prepend(lubridate::NA_Date_),
    illness_onset_dt = c(rep(as.Date("2020-03-10"), 99e3L), rep("2021-03-26", 1e3L)),
    specimen_coll_dt = illness_onset_dt,
    inv_start_dt = specimen_coll_dt,
    patient_race_calc = withr::with_seed(
      200L,
      sample(
        c("Black", "White", "Asian", "Native", "None", NA_character_),
        size = NROW(inv_start_dt),
        replace = TRUE,
        prob = c(0.6, 0.3, 0.03, 0.02, 0.01, 0.04)
      )
    )
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date) & !"specimen_coll_dt",
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  mockery::stub(
    filter_deaths,
    "date_inv",
    lubridate::as_date
  )

  data_mock <- mockery::mock(filter_deaths(data))

  mockery::stub(
    death_calc_race,
    "filter_deaths",
    data_mock
  )

  tbl_death <- death_calc_race(data, date = "2021-04-01")

  expect_snapshot(tbl_death)
})
