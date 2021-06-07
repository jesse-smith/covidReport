test_that("`void(test_table_total())` matches snapshot", {
  # Make fake data to pass to function
  data <- tibble::tibble(.rows = 1e6L)

  # Make calculated data
  calc_total <- tibble::tribble(
      ~ result, ~ n, ~ percent,
    "Positive", 1e5,       0.1,
    "Negative", 9e5,       0.9,
       "Total", 1e6,       1.0
  )

  # Stub `test_calc_total()`

  mockery::stub(
    test_table_total,
    "test_calc_total",
    calc_total
  )

  tbl_void <- flextable::void(
    test_table_total(data),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`test_calc_total()` matches reference", {
  # Make fake data to pass to function
  data <- tibble::tibble(positive = c(rep(TRUE, 1e5L), rep(FALSE, 9e5L))) %>%
    dplyr::nest_by(.data[["positive"]]) %>%
    dplyr::arrange(dplyr::desc(.data[["positive"]]))

  tbl_test <- test_calc_total(data, date = "2021-04-19")

  tbl_ref <- tibble::tribble(
      ~ result, ~ n, ~ percent,
    "Positive", 1e5,       0.1,
    "Negative", 9e5,       0.9,
       "Total", 1e6,       1.0
  )

  expect_equal(tbl_test, tbl_ref)
})
