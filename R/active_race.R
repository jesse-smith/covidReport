#' Plot Active Case Rates by Race
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
active_plot_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    active_calc_race(date = date) %>%
    demog_plot_("14-Day Case Rates", grp = "race", date = date)
}

#' Tabluate Active Cases by Race
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
active_table_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    active_calc_race(date = date) %>%
    demog_table_(grp_lbl = "Race") %>%
    flextable::autofit()
}

#' Calculate Active Case Rates and Percentages by Race
#'
#' @inheritParams active_table_sex
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
active_calc_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_active(date = date) %>%
    active_trans_race() %>%
    demog_calc_("race") %>%
    demog_relevel_race()
}

active_trans_race <- function(data) {
  dplyr::transmute(data, grp = demog_race_grp_(.data[["patient_race_calc"]]))
}
