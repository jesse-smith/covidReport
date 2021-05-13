#' Tabulate Confirmed & Probable Cases/Deaths
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable` object
#'
#' @export
case_table_confirmed_probable <- function(
  data = coviData::process_positive_people(date = date),
  date = NULL
) {
  cases <- data %>%
    dplyr::count(.data[["inv_case_status"]]) %>%
    tidyr::pivot_wider(names_from = "inv_case_status", values_from = "n") %>%
    dplyr::mutate(
      Type = "Cases",
      Total = sum(., na.rm = TRUE),
      .before = 1L
    )

  deaths <- data %>%
    filter_deaths() %>%
    dplyr::count(.data[["inv_case_status"]]) %>%
    tidyr::pivot_wider(names_from = "inv_case_status", values_from = "n") %>%
    dplyr::mutate(
      Type = "Deaths",
      Total = sum(., na.rm = TRUE),
      .before = 1L
    )

  dplyr::bind_rows(cases, deaths) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(Type = "", C = "Confirmed", P = "Probable") %>%
    fmt_covid_table() %>%
    flextable::autofit()
}
