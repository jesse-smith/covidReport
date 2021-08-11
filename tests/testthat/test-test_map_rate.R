test_that("`test_map_rate()` matches snapshot", {

  data_rds <- readRDS(test_path("../data/test_positivity_data.rds"))
  zips <- coviData::shelby_zip %>%
    rep(times = NROW(data_rds) %/% NROW(.)) %>%
    append(rep(NA_character_, times = NROW(data_rds) - NROW(.)))

  data <- data_rds %>%
    dplyr::mutate(
      positive = .data[["inv_case_status"]] %in% c("C", "P"),
      patient_zip = {{ zips }}
    ) %>%
    {withr::with_seed(200L, dplyr::slice_sample(., n = 1e4))} %>%
    dplyr::nest_by(.data[["positive"]]) %>%
    dplyr::arrange(dplyr::desc(.data[["positive"]])) %>%
    set_attr("date", as.Date("2021-04-19"))

  mockery::stub(
    test_map_rate,
    "date_pcr",
    lubridate::as_date
  )

  plt <- test_map_rate(data, date = "2021-04-19")

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
