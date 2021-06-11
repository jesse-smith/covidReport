#' Plot Pediatric Case Rates by Race
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
peds_plot_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    peds_calc_race(date = date) %>%
    demog_plot_(
      "Pediatric Case Rates",
      grp = "race",
      date = date,
      color = "darkorchid4"
    )
}

#' Tabluate Pediatric Cases by Race
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
peds_table_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    peds_calc_race(date = date) %>%
    demog_table_(grp_lbl = "Race", color = "darkorchid4") %>%
    flextable::autofit()
}

#' Calculate Pediatric Case Rates and Percentages by Race
#'
#' @inheritParams peds_table_race
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
peds_calc_race <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_peds() %>%
    peds_trans_race() %>%
    demog_calc_("race") %>%
    demog_relevel_race()
}

peds_trans_race <- function(data) {
  dplyr::transmute(data, grp = demog_race_grp_(.data[["patient_race_calc"]]))
}
