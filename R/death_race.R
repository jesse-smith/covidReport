#' Plot Death Rates by Race
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
death_plot_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    death_calc_race(date = date) %>%
    demog_plot_("Death Rates", grp = "race", date = date)
}

#' Tabluate Death by Race
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
death_table_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    death_calc_race(date = date) %>%
    demog_table_(grp_lbl = "Race") %>%
    flextable::autofit()
}

#' Calculate Death Rates and Percentages by Race
#'
#' @inheritParams death_table_sex
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
death_calc_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_deaths() %>%
    death_trans_race() %>%
    demog_calc_("race") %>%
    demog_relevel_race()
}

death_trans_race <- function(data) {
  dplyr::transmute(data, grp = demog_race_grp_(.data[["patient_race_calc"]]))
}
