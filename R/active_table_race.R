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
    active_table_(grp_lbl = "Race") %>%
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
    active_calc_("race") %>%
    active_relevel_race()
}

active_trans_race <- function(data) {
  dplyr::transmute(data, grp = active_race_grp_(.data[["patient_race_calc"]]))
}

active_relevel_race <- function(data) {
  data %>%
    dplyr::mutate(
      grp = forcats::fct_relevel(
        .data[["grp"]],
        "Black/African American",
        "White",
        "Other",
        "Missing"
      )
    ) %>%
    dplyr::arrange(.data[["grp"]])
}
