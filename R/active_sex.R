#' Plot Active Case Rates by Sex
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
active_plot_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    active_calc_sex(date = date) %>%
    active_plot_("sex", date = date)
}

#' Tabluate Active Cases by Sex
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
active_table_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    active_calc_sex(date = date) %>%
    active_table_(grp_lbl = "Sex") %>%
    flextable::autofit()
}

#' Calculate Active Case Rates and Percentages by Sex
#'
#' @inheritParams active_table_sex
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
active_calc_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_active(date = date) %>%
    active_trans_sex() %>%
    active_calc_(grp = "sex")
}

active_trans_sex <- function(data) {
  dplyr::transmute(
    data,
    grp = .data[["patient_current_sex"]] %>%
      stringr::str_to_upper() %>%
      stringr::str_remove_all("[^FMALE]") %>%
      stringr::str_extract("^[MF]"),
    grp = dplyr::case_when(
      .data[["grp"]] == "F" ~ "Female",
      .data[["grp"]] == "M" ~ "Male"
    )
  )
}
