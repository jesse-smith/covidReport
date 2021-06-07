test_that("`void(case_table_active())` matches snapshot", {

  mockery::stub(
    case_table_active,
    "coviData::date_inv",
    lubridate::as_date
  )

  tbl_calc <- tibble::tribble(
      ~ status,       ~ n, ~ percent,
      "Active",       900,     0.009,
    "Deceased",      9999,     0.100,
    "Inactive",     89101,     0.891
  )

  mockery::stub(
    case_table_active,
    "case_calc_active",
    tbl_calc
  )

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
    inv_start_dt = specimen_coll_dt
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date),
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_void <- flextable::void(
    case_table_active(data, date = "2021-04-01"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`case_calc_active()` matches reference (with active cases)", {

  mockery::stub(
    case_calc_active,
    "coviData::date_inv",
    lubridate::as_date
  )

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
    inv_start_dt = specimen_coll_dt
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date),
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_active <- case_calc_active(data, date = "2021-04-01") %>%
    set_attr("tabyl_type", NULL) %>%
    set_attr("core", NULL)

  tbl_ref <- tibble::tribble(
      ~ status,       ~ n, ~ percent,
      "Active",       900,     0.009,
    "Deceased",      9999,     0.100,
    "Inactive",     89101,     0.891
  )

  expect_equal(tbl_active, tbl_ref)
})

test_that("`case_calc_active()` matches reference (with no active cases)", {

  mockery::stub(
    case_calc_active,
    "coviData::date_inv",
    lubridate::as_date
  )

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
    illness_onset_dt = vctrs::vec_assign(
      rep(as.Date("2020-04-01"), times = 1e5L),
      i = seq(1, 1e5, by = 1e3),
      lubridate::NA_Date_
    ),
    specimen_coll_dt = vctrs::vec_assign(
      rep(as.Date("2020-04-10"), times = 1e5L),
      i = seq(7, 9e4, by = 1234),
      lubridate::NA_Date_
    ),
    inv_start_dt = dplyr::coalesce(
      specimen_coll_dt,
      rep(as.Date("2020-04-15"), times = 1e5L)
    )
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date),
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_active <- case_calc_active(data, date = "2021-04-01") %>%
    set_attr("tabyl_type", NULL) %>%
    set_attr("core", NULL)

  tbl_ref <- tibble::tribble(
      ~ status,    ~ n, ~ percent,
      "Active",      0,       0.0,
    "Deceased",   9999,       0.1,
    "Inactive",  90001,       0.9
  )

  expect_equal(tbl_active, tbl_ref)
})
