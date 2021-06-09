test_that("`void(active_table_race())` matches snapshot", {
  tbl_mock <- mockery::mock(tibble::tribble(
                       ~ grp, ~ n, ~ percent, ~ rate,
    "Black/African American", 500,       0.5,    0.1,
                     "White", 300,       0.3,    0.1,
                     "Other", 100,       0.1,    0.1,
                   "Missing", 100,       0.1,    0.1,
  ))

  mockery::stub(
    active_table_race,
    "active_calc_race",
    tbl_mock
  )

  data <- tibble::tibble()

  tbl_void <- flextable::void(
    active_table_race(data, date = "2021-04-01"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`active_calc_race()` matches snapshot", {

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
    filter_active,
    "date_inv",
    lubridate::as_date
  )

  data_mock <- mockery::mock(filter_active(data, date = "2021-04-01"))

  mockery::stub(
    active_calc_race,
    "filter_active",
    data_mock
  )

  tbl_active <- active_calc_race(data, date = "2021-04-01")

  expect_snapshot(tbl_active)
})
