test_that("`active_map_rate()` matches snapshot", {

  zips <- coviData::shelby_zip %>%
    rep(times = 1e5L %/% NROW(.)) %>%
    append(rep(NA_character_, times = 1e5L - NROW(.)))

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
    patient_zip = {{ zips }}
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

  mockery::stub(
    active_map_rate,
    "date_inv",
    lubridate::as_date
  )

  active_data <- filter_active(data, date = "2021-04-01")

  mockery::stub(
    active_map_rate,
    "filter_active",
    active_data
  )

  plt <- active_map_rate(data, date = "2021-04-01")

  # sf plots can't be tested with {vdiffr} due to external dependencies
  # Just snapshot the object instead - harder to debug changes, but workable
  expect_snapshot(plt[["data"]])
  expect_snapshot(plt[["layers"]])
  expect_snapshot(plt[["scales"]])
  expect_snapshot(plt[["mapping"]])
  expect_snapshot(plt[["theme"]])
  expect_snapshot(plt[["coordinates"]])
  expect_snapshot(plt[["facet"]])
  expect_snapshot(plt[["labels"]])
})
