#' Create Timeseries for Google Sheets Data
#'
#' @param inv Investigation data from
#'   \code{\link[coviData:process_inv]{process_inv()}}
#'
#' @param pcr PCR test data from
#'   \code{\link[coviData:process_inv]{process_pcr()}}
#'
#' @param delay The number of days that reporting is delayed; will truncate
#'   percent positive numbers at this date
#'
#' @param date Download date of the data
#'
#' @return A `tibble` with columns
#'   `dt` (`Date`), `inv_pos` (`int`), `inv_neg` (`int`), `inv_pct_pos` (`dbl`),
#'   `inv_pct_pos_avg` (`dbl`), `pcr_pos` (`int`), `pcr_neg` (`int`),
#'   `deaths` (`int`)
#'
#' @export
#'
#' @keywords internal
gs_timeseries <- function(
  inv = process_inv(read_inv(date = date)),
  delay = 5L,
  date = NULL
) {
  date <- date_inv(date)

  new_rpt_dt <- NROW(read_inv_id(date)) - NROW(read_inv_id(date-1L))

  pos(inv) %>%
    prep_daily_data(
      min_date = lubridate::as_date("2020-03-05"),
      date = date,
      delay = delay
    ) %>%
    dplyr::select(dt = "test_date", case_test_dt_avg = "avg")
}
