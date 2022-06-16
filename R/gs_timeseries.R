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
  pcr = process_pcr(read_pcr(date = date), inv = inv),
  delay = 5L,
  date = NULL
) {
  date <- date_inv(date)

  new_rpt_dt <- NROW(read_inv_id(date)) - NROW(read_inv_id(date-1L))

  inv_dt <- pos(inv) %>%
    prep_daily_data(
      min_date = lubridate::as_date("2020-03-05"),
      date = date,
      delay = delay
    ) %>%
    dplyr::select(dt = "test_date", case_test_dt_avg = "avg")

  pcr_dt <- pcr %>%
    prep_test_pos(min_date = "2020-03-05", date = date, delay = delay) %>%
    dplyr::group_by(
      epiweek = paste0(
        lubridate::epiyear(.data[["test_date"]]),
        "-",
        lubridate::epiweek(.data[["test_date"]])
      )
    ) %>%
    dplyr::mutate(
      test_pct_pos_wk = divide_by(
        sum(.data[["positive"]], na.rm = TRUE),
        sum(.data[["total"]], na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dt = "test_date", "test_pct_pos_wk")

 # dplyr::left_join(inv_dt, pcr_dt, by = "dt")




  date1 <- date-6
  date2 <- date-5
  date3 <- date-4
  date4 <- date-3
  date5 <- date-2
  date6 <- date-1
  date7 <- date

  dt <- c(date1, date2, date3, date4, date5, date6, date7)

  n1 <- NROW(pos(process_inv(read_inv(date = date1))))
  n2 <- NROW(pos(process_inv(read_inv(date = date2))))
  n3 <- NROW(pos(process_inv(read_inv(date = date3))))
  n4 <- NROW(pos(process_inv(read_inv(date = date4))))
  n5 <- NROW(pos(process_inv(read_inv(date = date5))))
  n6 <- NROW(pos(process_inv(read_inv(date = date6))))
  n7 <- NROW(pos(process_inv(read_inv(date = date7))))


  cumulative_count <- c(n1, n2, n3, n4, n5, n6, n7)

  total_counts <- data.frame(dt, cumulative_count)


  dplyr::left_join(total_counts, inv_dt, by = "dt")%>%
    dplyr::left_join(pcr_dt, by = "dt")

}
