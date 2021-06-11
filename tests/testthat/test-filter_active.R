test_that("`filter_active()` is equal to snapshot", {

  mockery::stub(
    filter_active,
    "date_inv",
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

  tbl_active <- filter_active(data, date = "2021-04-01") %>%
    dplyr::arrange(
      .data[["die_from_illness_ind"]],
      .data[["inv_death_dt"]],
      .data[["illness_onset_dt"]],
      .data[["specimen_coll_dt"]],
      .data[["inv_start_dt"]]
    )

  expect_snapshot(tbl_active)
})
