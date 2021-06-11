#' Plot Death Rates by Sex
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
death_plot_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    death_calc_sex(date = date) %>%
    demog_plot_(
      "Death Rates",
      grp = "sex",
      date = date,
      color = "grey30"
    )
}

#' Tabluate Deaths by Sex
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
death_table_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    death_calc_sex(date = date) %>%
    demog_table_(grp_lbl = "Sex", color = "grey30") %>%
    flextable::autofit()
}

#' Calculate Death Rates and Percentages by Sex
#'
#' @inheritParams active_table_sex
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
death_calc_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_deaths() %>%
    death_trans_sex() %>%
    demog_calc_(grp = "sex")
}

death_trans_sex <- function(data) {
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
