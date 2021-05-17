#' Skip Reporting Function Tests
#'
#' Reporting functions are time-consuming to run; `skip_reports()` sets an
#' environment variable that allows the user to skip these tests. Component
#' functions should be tested separately.
#'
#' When `skip = NULL`, this is used as a skipping function within a test. When
#' a value is supplied, it sets the `SKIP_REPORTS` environment variable that
#' controls behavior during testing.
#'
#' @param skip Should the reporting tests be skipped? If `SKIP_REPORTS` is unset
#'   during testing, `NULL` defaults to `TRUE`.
#'
#' @return `NULL`
#'
#' @keywords internal
skip_reports <- function(skip = NULL) {
  # Set `SKIP_REPORTS` if provided
  if (!is.null(skip)) {
    coviData::assert(
      rlang::is_bool(skip),
      message = "`skip` must be a boolean value"
    )
    Sys.setenv(SKIP_REPORTS = skip)
  }

  # Get `SKIP_REPORTS`
  SKIP_REPORTS <- Sys.getenv("SKIP_REPORTS")
  coviData::assert_any(
    rlang::is_bool(SKIP_REPORTS),
    is.null(SKIP_REPORTS),
    message = "`skip` must be a boolean value or `NULL`"
  )
  SKIP_REPORTS <- !rlang::is_false(SKIP_REPORTS)

  # Skip if `SKIP_REPORTS = TRUE`
  skip_if(SKIP_REPORTS, "Skipping report tests")
}
