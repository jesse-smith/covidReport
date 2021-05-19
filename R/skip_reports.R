#' Skip Reporting Function Tests
#'
#' Reporting functions are time-consuming to run; `skip_reports()` sets an
#' option that allows the user to skip these tests. Component functions should
#' be tested separately.
#'
#' When `skip = NULL`, this is used as a skipping function within a test. When
#' a value is supplied, it sets the `SKIP_REPORTS` option that controls
#' behavior during testing.
#'
#' @param skip Should the reporting tests be skipped? If
#'   `getOption(SKIP_REPORTS)` is unset during testing, `NULL` defaults to
#'   `TRUE`.
#'
#' @return Either the previous value of `SKIP_REPORTS` (if setting the option)
#'   or a `skip` condition
#'
#' @keywords internal
skip_reports <- function(skip = NULL) {
  # Set `SKIP_REPORTS` if provided
  if (!is.null(skip)) {
    coviData::assert(
      rlang::is_bool(skip),
      message = "`skip` must be a boolean value"
    )
    old_skip <- getOption("SKIP_REPORTS")
    options(SKIP_REPORTS = skip)
    return(old_skip)
  }

  # Get `SKIP_REPORTS`
  SKIP_REPORTS <- getOption("SKIP_REPORTS")
  coviData::assert_any(
    rlang::is_bool(SKIP_REPORTS),
    is.null(SKIP_REPORTS),
    message = "`skip` must be a boolean value or `NULL`"
  )
  SKIP_REPORTS <- !rlang::is_false(SKIP_REPORTS)

  # Skip if `SKIP_REPORTS = TRUE`
  testthat::skip_if(SKIP_REPORTS, "Skipping report tests")
}
