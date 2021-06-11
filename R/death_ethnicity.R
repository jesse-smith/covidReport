#' Plot Death Rates by Ethnicity
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
death_plot_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    death_calc_ethnicity(date = date) %>%
    demog_plot_(
      "Death Rates",
      grp = "ethnicity",
      date = date,
      color = "firebrick4"
    )
}

#' Tabluate Death Rates by Ethnicity
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
death_table_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    death_calc_ethnicity(date = date) %>%
    demog_table_(grp_lbl = "Race", color = "firebrick4") %>%
    flextable::autofit()
}

#' Calculate Death Rates and Percentages by Ethnicity
#'
#' @inheritParams death_table_sex
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
death_calc_ethnicity <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_deaths() %>%
    death_trans_ethnicity() %>%
    demog_calc_("ethnicity")
}

death_trans_ethnicity <- function(data) {
  dplyr::transmute(
    data,
    grp = dplyr::case_when(
      .data[["patient_ethnicity"]] == "2135-2" ~ "Hispanic/Latino",
      .data[["patient_ethnicity"]] == "2186-5" ~ "Not Hispanic/Latino",
      TRUE ~ NA_character_
    )
  )
}
