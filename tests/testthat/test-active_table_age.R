test_that("`void(active_table_age())` matches snapshot", {
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
    active_table_age,
    "active_calc_age",
    tbl_mock
  )

  data <- tibble::tibble()

  tbl_void <- flextable::void(
    active_table_age(data, date = "2021-04-01"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`active_calc_age()` matches snapshot", {

  data <- tibble::tibble(
    patient_dob = rep(
      as.Date("2021-04-01") - seq(1, 36e3, length.out = 100),
      times = 1000L
    ),
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
    age_in_years = (specimen_coll_dt - patient_dob) %>%
      lubridate::as.duration() %>%
      magrittr::divide_by_int(lubridate::dyears(1L)) %>%
      as.character() %>%
      vctrs::vec_assign(
        i = sample(seq_along(.), size = 1e3),
        value = NA_character_
      )
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date) & !c("patient_dob", "specimen_coll_dt"),
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
    active_calc_age,
    "filter_active",
    data_mock
  )

  tbl_active <- active_calc_age(data, date = "2021-04-01")

  expect_snapshot(tbl_active)
})
