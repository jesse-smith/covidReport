test_that("`active_plot_sex()` matches doppelganger", {
  tbl_mock <- mockery::mock(tibble::tribble(
        ~ grp, ~ n, ~ percent, ~ rate,
     "Female", 500,       0.5,    0.1,
       "Male", 400,       0.4,    0.1,
    "Missing", 100,       0.1,    0.1,
  ))

  mockery::stub(
    active_plot_sex,
    "active_calc_sex",
    tbl_mock
  )

  m_dt <- mockery::mock(as.Date("2021-04-01"), cycle = TRUE)
  mockery::stub(
    active_plot_sex,
    "date_inv",
    m_dt
  )

  plt <- active_plot_sex(tibble::tibble(), date = "2021-04-01")

  mockery::expect_called(m_dt, n = 1L)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "active case rate by sex",
      fig = plt,
      path = "active-plot-sex"
    )
  )
})

test_that("`void(active_table_sex())` matches snapshot", {
  tbl_mock <- mockery::mock(tibble::tribble(
        ~ grp, ~ n, ~ percent, ~ rate,
     "Female", 500,       0.5,    0.1,
       "Male", 400,       0.4,    0.1,
    "Missing", 100,       0.1,    0.1,
  ))

  mockery::stub(
    active_table_sex,
    "active_calc_sex",
    tbl_mock
  )

  data <- tibble::tibble()

  tbl_void <- flextable::void(
    active_table_sex(data, date = "2021-04-01"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`active_calc_sex()` matches snapshot", {

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
    patient_current_sex = withr::with_seed(
      200L,
      sample(
        c("F", "M", "U", "B", NA_character_),
        size = NROW(inv_start_dt),
        replace = TRUE,
        prob = c(0.5, 0.49, 0.001, 0.001, 0.008)
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
    filter_active,
    "date_inv",
    lubridate::as_date
  )

  data_mock <- mockery::mock(filter_active(data, date = "2021-04-01"))

  mockery::stub(
    active_calc_sex,
    "filter_active",
    data_mock
  )

  tbl_active <- active_calc_sex(data, date = "2021-04-01")

  expect_snapshot(tbl_active)
})
