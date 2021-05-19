test_that("`skip_reports()` handles bad inputs correctly", {
  expect_error(skip_reports("true"))
  expect_error(skip_reports(1L))
})

test_that("`skip_reports()` sets `SKIP_REPORTS` environment variable", {
  # Get initial env var
  SKIP_REPORTS <- getOption("SKIP_REPORTS")
  # This should set to `TRUE`
  skip_reports(TRUE)
  SKIP_REPORTS_TRUE <- getOption("SKIP_REPORTS")
  # This should set to `FALSE`
  skip_reports(FALSE)
  SKIP_REPORTS_FALSE <- getOption("SKIP_REPORTS")
  # Set back to original value
  options(SKIP_REPORTS = SKIP_REPORTS)

  expect_true(SKIP_REPORTS_TRUE)
  expect_false(SKIP_REPORTS_FALSE)
})

test_that("`skip_reports()` skips correctly", {
  # Skips when `NULL`
  withr::local_options(SKIP_REPORTS = NULL)
  expect_condition(skip_reports(), class = "skip")
  # Skips when `TRUE`
  withr::local_options(SKIP_REPORTS = TRUE)
  expect_condition(skip_reports(), class = "skip")
  # Does not skip when `FALSE`
  withr::local_options(SKIP_REPORTS = FALSE)
  expect_condition(skip_reports(), NA, class = "skip")
  # Errors otherwise
  withr::local_options(SKIP_REPORTS = "true")
  expect_error(skip_reports())
})
