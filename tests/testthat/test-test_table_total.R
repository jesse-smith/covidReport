test_that("`test_table_total()` info matches reference", {
  # Make fake data to pass to function
  data <- tibble::tibble(.rows = 1e6L)

  # Stub `process_tests_*` functions

  mockery::stub(
    test_table_total,
    "coviData::process_positive_tests",
    tibble::tibble(.rows = 1e5L)
  )

  mockery::stub(
    test_table_total,
    "coviData::process_negative_tests",
    tibble::tibble(.rows = 9e5L)
  )

  tbl_test <- suppressWarnings(
    test_table_total(data) %>%
      gt::as_raw_html() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble()
  )

  tbl_ref <- tibble::tribble(
    ~ `PCR Result`,         ~ N,         ~ `%`,
        "Positive",   "100,000",  "10&percnt;",
        "Negative",   "900,000",  "90&percnt;",
           "Total", "1,000,000", "100&percnt;"
  )

  expect_equal(tbl_test, tbl_ref)
})

test_that("`test_table_total()` html matches snapshot", {
  # Make fake data to pass to function
  data <- tibble::tibble(.rows = 1e6L)

  # Stub `process_tests_*` functions

  mockery::stub(
    test_table_total,
    "coviData::process_positive_tests",
    tibble::tibble(.rows = 1e5L)
  )

  mockery::stub(
    test_table_total,
    "coviData::process_negative_tests",
    tibble::tibble(.rows = 9e5L)
  )

  tbl_html <- suppressWarnings(
    test_table_total(data) %>%
      gt::as_raw_html() %>%
      xml2::read_html()
  )

  expect_snapshot(tbl_html)
})
