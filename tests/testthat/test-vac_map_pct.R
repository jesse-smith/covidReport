test_that("`vac_map_pct()` matches snapshot", {

  zips <- coviData::shelby_zip %>%
    rep(times = 5e5L %/% NROW(.)) %>%
    append(rep(NA_character_, times = 5e5L - NROW(.)))

  data <- tibble::tibble(
    asiis_pat_id_ptr = 1:5e5,
    resident = c(rep(TRUE, 5*9e4), rep(FALSE, 5e4)),
    dose_count = c(rep(1, 5e4), rep(2, 5*9e4)),
    address_zip = {{ zips }}
  ) %>% dplyr::slice_sample(prop = 1)

  mockery::stub(
    vac_map_pct,
    "date_vac",
    lubridate::as_date
  )

  plt <- vac_map_pct(data, date = "2021-04-01")

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
