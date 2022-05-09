#' Plot Active Case Rates by Ethnicity
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
active_plot_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    active_calc_ethnicity(date = date) %>%
    demog_plot_("14-Day Case Rates", grp = "ethnicity", date = date)
}

#' Tabluate Active Cases by Ethnicity
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
active_table_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    active_calc_ethnicity(date = date) %>%
    demog_table_(grp_lbl = "Ethnicity") %>%
    flextable::autofit()
}

#' Calculate Active Case Rates and Percentages by Ethnicity
#'
#' @inheritParams active_table_sex
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
active_calc_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_active(date = date) %>%
    active_trans_ethnicity() %>%
    demog_calc_("ethnicity")
}

active_trans_ethnicity <- function(data) {
  dplyr::transmute(
    data,
    grp = dplyr::case_when(
      .data[["patient_ethnicity"]] == "2135-2" ~ "Hispanic/Latino",
      .data[["patient_ethnicity"]] == "2186-5" ~ "Not Hispanic/Latino",
      TRUE ~ NA_character_
    )
  )
}
