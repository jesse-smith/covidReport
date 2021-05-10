test_that("`test_plot_positivity()` matches doppelganger", {
  # Stub `path_pcr()`
  mockery::stub(
    test_plot_positivity,
    "coviData::path_pcr",
    fs::path(
      "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR PCR",
      "MSR - All PCRs_04192021.csv"
    )
  )

  # Stub processing functions
  data <- readRDS(test_path("../data/test_positivity_data.rds"))

  mockery::stub(
    prep_test_ts,
    "coviData::process_positive_tests",
    dplyr::filter(data, .data[["inv_case_status"]] %in% c("C", "P"))
  )

  mockery::stub(
    prep_test_ts,
    "coviData::process_negative_tests",
    dplyr::filter(data, .data[["inv_case_status"]] == "N")
  )

  positive_ts <- prep_test_ts(
    data,
    date = lubridate::as_date("2021-04-19"),
    result = "positive"
  )

  negative_ts <- prep_test_ts(
    data,
    date = lubridate::as_date("2021-04-19"),
    result = "negative"
  )

  prep_test_fn <- function(
    data,
    date = NULL,
    result = c("positive", "negative")
  ) {
    result <- rlang::arg_match(result)[[1L]]
    if (result == "positive") positive_ts else negative_ts
  }
  mockery::stub(
    prep_test_pos,
    "prep_test_ts",
    prep_test_fn
  )

  test_pos <- prep_test_pos(data, date = as.Date("2021-04-19"), delay = 7L)

  mockery::stub(
    test_plot_positivity,
    "prep_test_pos",
    test_pos
  )

  vdiffr::expect_doppelganger(
    title = "test positivity",
    fig = test_plot_positivity(data, date = "2021-04-19"),
    path = "test-plot-positivity"
  )
})

test_that("`prep_test_ts()` output is correct for positive tests", {
  data <- tibble::tibble(
    specimen_coll_dt = rep(
      seq(as.Date("2020-03-01"), as.Date("2021-04-20"), by = 1L),
      times = 200L
    ) %>%
      append(rep(NA_character_, 800L)) %>%
      format("%Y-%m-%dTH:M:SZ")
  ) %>% dplyr::slice_sample(prop = 1)

  mockery::stub(
    prep_test_ts,
    "coviData::process_positive_tests",
    data
  )

  positive_ts <- prep_test_ts(
    data,
    date = lubridate::as_date("2021-04-19"),
    result = "positive"
  )

  expect_s3_class(positive_ts, "n_tbl")
  expect_vector(positive_ts[["test_date"]], lubridate::Date())
  expect_vector(positive_ts[["positive"]], integer())
  expect_equal(attr(positive_ts, "n_obs", exact = TRUE), 84000L)
  expect_equal(attr(positive_ts, "n_missing", exact = TRUE), 1800L)
  expect_equal(min(positive_ts[["test_date"]]), as.Date("2020-03-05"))
  expect_equal(max(positive_ts[["test_date"]]), as.Date("2021-04-19"))
})

test_that("`prep_test_ts()` output is correct for negative tests", {
  data <- tibble::tibble(
    specimen_coll_dt = rep(
      seq(as.Date("2020-03-01"), as.Date("2021-04-20"), by = 1L),
      times = 200L
    ) %>%
      append(rep(NA_character_, 800L)) %>%
      format("%Y-%m-%dTH:M:SZ")
  ) %>% dplyr::slice_sample(prop = 1)

  mockery::stub(
    prep_test_ts,
    "coviData::process_negative_tests",
    data
  )

  negative_ts <- prep_test_ts(
    data,
    date = lubridate::as_date("2021-04-19"),
    result = "negative"
  )

  n_03_05 <- negative_ts %>%
    dplyr::filter(test_date == as.Date("2020-03-05")) %>%
    dplyr::pull("negative")

  expect_s3_class(negative_ts, "n_tbl")
  expect_vector(negative_ts[["test_date"]], lubridate::Date())
  expect_vector(negative_ts[["negative"]], integer())
  expect_equal(attr(negative_ts, "n_obs", exact = TRUE), 84000L)
  expect_equal(attr(negative_ts, "n_missing", exact = TRUE), 2000L)
  expect_equal(as.character(min(negative_ts[["test_date"]])), "2020-03-05")
  expect_equal(as.character(max(negative_ts[["test_date"]])), "2021-04-19")
  expect_equal(n_03_05, 0L)
})

test_that("`prep_test_pos()` output is correct", {
  data <- tibble::tibble(
    specimen_coll_dt = rep(
      seq(as.Date("2020-03-01"), as.Date("2021-04-20"), by = 1L),
      times = 200L
    ) %>%
      append(rep(NA_character_, 800L)) %>%
      format("%Y-%m-%dTH:M:SZ")
  ) %>% dplyr::slice_sample(prop = 1)

  mockery::stub(
    prep_test_ts,
    "coviData::process_positive_tests",
    data
  )
  mockery::stub(
    prep_test_ts,
    "coviData::process_negative_tests",
    data
  )

  positive_ts <- prep_test_ts(
    data,
    date = lubridate::as_date("2021-04-19"),
    result = "positive"
  )

  negative_ts <- prep_test_ts(
    data,
    date = lubridate::as_date("2021-04-19"),
    result = "negative"
  )

  prep_test_fn <- function(
    data,
    date = NULL,
    result = c("positive", "negative")
  ) {
    result <- rlang::arg_match(result)[[1L]]
    if (result == "positive") positive_ts else negative_ts
  }
  mockery::stub(
    prep_test_pos,
    "prep_test_ts",
    prep_test_fn
  )

  test_pos <- prep_test_pos(data, date = as.Date("2021-04-19"), delay = 7L)

  expect_s3_class(test_pos, "n_tbl")
  expect_vector(test_pos[["test_date"]], lubridate::Date())
  expect_vector(test_pos[["positive"]], integer())
  expect_vector(test_pos[["negative"]], integer())
  expect_vector(test_pos[["total"]], integer())
  expect_vector(test_pos[["pct_pos"]], double())
  expect_vector(test_pos[["avg"]], double())
  expect_equal(attr(test_pos, "n_obs", exact = TRUE), 2L*84000L)
  expect_equal(attr(test_pos, "n_missing", exact = TRUE), 3800L)
  expect_equal(as.character(min(test_pos[["test_date"]])), "2020-03-14")
  expect_equal(as.character(max(test_pos[["test_date"]])), "2021-04-12")
})