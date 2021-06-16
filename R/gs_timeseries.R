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

  inv_dt <- inv %>%
    prep_test_pos(min_date = "2020-03-05", date = date, delay = 0L) %>%
    dplyr::mutate(
      dplyr::across(
        where(rlang::is_double) & !where(lubridate::is.Date),
        ~ dplyr::if_else(is.nan(.x), 0, .x) %>%
          vec_assign(i = tail(vec_seq_along(.), n = delay), value = NA_real_)
      )
    ) %>%
    dplyr::select(
      dt = "test_date",
      inv_pos = "positive",
      inv_neg = "negative",
      inv_pct_pos = "pct_pos",
      inv_pct_pos_avg = "avg"
    )

  test_dt <- pcr %>%
    prep_test_pos(min_date = "2020-03-05", date = date, delay = 0L) %>%
    dplyr::mutate(
      dplyr::across(
        where(rlang::is_double) & !where(lubridate::is.Date),
        ~ dplyr::if_else(is.nan(.x), 0, .x) %>%
          vec_assign(i = tail(vec_seq_along(.), n = delay), value = NA_real_)
      )
    ) %>%
    dplyr::select(dt = "test_date", pcr_pos = "positive", pcr_neg = "negative")

  death_dt <- pos(inv) %>%
    filter_deaths() %>%
    dplyr::mutate(
      inv_death_dt = std_dates(.data[["inv_death_dt"]], orders = "ymdT")
    ) %>%
    dplyr::filter(
      lubridate::as_date("2020-03-05") <= .data[["inv_death_dt"]],
      .data[["inv_death_dt"]] <= {{ date }}
    ) %>%
    dplyr::count(dt = .data[["inv_death_dt"]]) %>%
    complete_dt_n("dt", from = "2020-03-05", to = date) %>%
    dplyr::rename(deaths = "n")

  inv_dt %>%
    dplyr::left_join(test_dt, by = "dt") %>%
    dplyr::left_join(death_dt, by = "dt")
}
