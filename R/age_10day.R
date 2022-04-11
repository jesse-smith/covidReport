#' Plot Active Case Rates by Age
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
active_plot_age_10day <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    active_calc_age_10day(date = date) %>%
    demog_plot_("10-Day Case Rates", grp = "age", date = date)
}

#' Tabulate Active Cases by Age
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
active_table_age_10day <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    active_calc_age_10day(date = date) %>%
    demog_table_(grp_lbl = "Age") %>%
    flextable::autofit(add_h = 0.075)
}

#' Calculate Active Case Rates and Percentages by Age
#'
#' @inheritParams active_table_age
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
active_calc_age_10day <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_active(date = date, days = 10L) %>%
    active_trans_age() %>%
    demog_calc_("age")
}

