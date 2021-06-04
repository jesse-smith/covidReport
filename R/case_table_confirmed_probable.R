#' Tabulate Confirmed & Probable Cases/Deaths
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable` object
#'
#' @export
case_table_confirmed_probable <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL
) {
  case_calc_confirmed_probable(data, date = date) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      type = "",
      total = "Total",
      C = "Confirmed",
      P = "Probable"
    ) %>%
    fmt_covid_table() %>%
    flextable::autofit()
}

#' Tabulate Confirmed & Probable Cases/Deaths
#'
#' @inheritParams case_table_confirmed_probable
#'
#' @return A `tibble`
#'
#' @keywords internal
case_calc_confirmed_probable <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL
){
  cases <- data %>%
    dplyr::count(.data[["inv_case_status"]]) %>%
    tidyr::pivot_wider(names_from = "inv_case_status", values_from = "n") %>%
    dplyr::mutate(
      type = "Cases",
      total = sum(., na.rm = TRUE),
      .before = 1L
    )

  deaths <- data %>%
    filter_deaths() %>%
    dplyr::count(.data[["inv_case_status"]]) %>%
    tidyr::pivot_wider(names_from = "inv_case_status", values_from = "n") %>%
    dplyr::mutate(
      type = "Deaths",
      total = sum(., na.rm = TRUE),
      .before = 1L
    )

  dplyr::bind_rows(cases, deaths)
}
