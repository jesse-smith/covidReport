#' Plot Pediatric Case Rates by Sex
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
peds_plot_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    peds_calc_sex(date = date) %>%
    demog_plot_(
      "Pediatric Case Rates",
      grp = "sex",
      date = date,
      color = "darkorchid4"
    )
}


#' Plot Active Pediatric Cases by Sex
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{filter_active(pos(process_inv()))}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
act_peds_plot_sex <- function(
  data = filter_active(pos(process_inv(read_inv(date)))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    peds_calc_sex(date = date) %>%
    demog_plot_(
      "Pediatric Case Rates",
      grp = "sex",
      date = date,
      color = "darkorchid1"
    )
}

#' Tabluate Pediatric Cases by Sex
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
peds_table_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    peds_calc_sex(date = date) %>%
    demog_table_(grp_lbl = "Sex", color = "darkorchid4", peds = TRUE) %>%
    flextable::autofit()
}




#' Tabluate Active Pediatric Cases by Sex
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{filter_active(pos(process_inv()))}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
act_peds_table_sex <- function(
  data = filter_active(pos(process_inv(read_inv(date)))),
  date = NULL
) {
  data %>%
    peds_calc_sex(date = date) %>%
    demog_table_(grp_lbl = "Sex", color = "darkorchid1", peds = TRUE) %>%
    flextable::autofit()
}


#' Calculate Pediatric Case Rates and Percentages by Sex
#'
#' @inheritParams peds_table_sex
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
peds_calc_sex <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_peds() %>%
    peds_trans_sex() %>%
    demog_calc_(grp = "sex", peds = TRUE)
}

peds_trans_sex <- function(data) {
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
