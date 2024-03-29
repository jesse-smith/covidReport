#' Plot Pediatric Case Rates by Ethnicity
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
peds_plot_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    peds_calc_ethnicity(date = date) %>%
    demog_plot_(
      "Pediatric Case Rates",
      grp = "ethnicity",
      date = date,
      color = "darkorchid4"
    )
}



#' Plot Active Pediatric Case Rates by Ethnicity
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{filter_active(pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
act_peds_plot_ethnicity <- function(
  data = filter_active(pos(process_inv(read_inv(date)))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    peds_calc_ethnicity(date = date) %>%
    demog_plot_(
      "Pediatric Case Rates",
      grp = "ethnicity",
      date = date,
      color = "darkorchid1"
    )
}




#' Tabluate Pediatric Cases by Ethnicity
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
peds_table_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    peds_calc_ethnicity(date = date) %>%
    demog_table_(grp_lbl = "Race", color = "darkorchid4", peds = TRUE) %>%
    flextable::autofit()
}





#' Tabluate Active Pediatric Cases by Ethnicity
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{filter_active(pos(process_inv()))}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
act_peds_table_ethnicity <- function(
  data = filter_active(pos(process_inv(read_inv(date)))),
  date = NULL
) {
  data %>%
    peds_calc_ethnicity(date = date) %>%
    demog_table_(grp_lbl = "Race", color = "darkorchid1", peds = TRUE) %>%
    flextable::autofit()
}




#' Calculate Pediatric Case Rates and Percentages by Ethnicity
#'
#' @inheritParams peds_table_ethnicity
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
peds_calc_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_peds() %>%
    peds_trans_ethnicity() %>%
    demog_calc_("ethnicity", peds = TRUE)
}

peds_trans_ethnicity <- function(data) {
  dplyr::transmute(
    data,
    grp = dplyr::case_when(
      .data[["patient_ethnicity"]] == "2135-2" ~ "Hispanic/Latino",
      .data[["patient_ethnicity"]] == "2186-5" ~ "Not Hispanic/Latino",
      TRUE ~ NA_character_
    )
  )
}
