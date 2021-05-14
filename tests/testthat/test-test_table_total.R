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

  tbl_test <- test_table_total(data) %>%
      flextable::flextable_to_rmd() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble() %>%
      set_colnames(janitor::make_clean_names(.[1L,])) %>%
      dplyr::filter(dplyr::row_number() > 1L)

  tbl_ref <- tibble::tribble(
    ~ `pcr_result`,         ~ n, ~ `percent`,
        "Positive",   "100,000",       "10.0%",
        "Negative",   "900,000",       "90.0%",
           "Total", "1,000,000",      "100.0%"
  )

  expect_equal(tbl_test, tbl_ref)
})

test_that("`void(test_table_total())` matches snapshot", {
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

  tbl_void <- flextable::void(
    test_table_total(data),
    part = "all"
  )

  expect_snapshot(tbl_void)
})
