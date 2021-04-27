test_that("`case_table_active()` matches snapshot", {
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
      inv_death_dt,
      i = seq(1, 1e5, by = 1e3),
      lubridate::NA_Date_
    ),
    specimen_coll_dt = vctrs::vec_assign(
      inv_death_dt,
      i = seq(7, 9e4, by = 1234),
      lubridate::NA_Date_
    ),
    inv_start_dt = dplyr::coalesce(
      specimen_coll_dt,
      inv_death_dt
    )
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date),
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_active <- suppressWarnings(
    gt::as_raw_html(case_table_active(data, date = "2021-04-01"))
  )

  expect_snapshot(tbl_active)
})

test_that("`case_table_active()` works with no active cases", {
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

  tbl_active <- suppressWarnings(
    gt::as_raw_html(case_table_active(data, date = "2021-04-01"))
  )

  expect_snapshot(tbl_active)
})
