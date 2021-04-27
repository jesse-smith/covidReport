#' Tabulate Confirmed & Probable Cases/Deaths
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `gt_tbl` object
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
      `COVID-19` = "Cases",
      `T` = sum(., na.rm = TRUE),
      .before = 1L
    )

  deaths <- data %>%
    filter_deaths() %>%
    dplyr::count(.data[["inv_case_status"]]) %>%
    tidyr::pivot_wider(names_from = "inv_case_status", values_from = "n") %>%
    dplyr::mutate(
      `COVID-19` = "Deaths",
      `T` = sum(., na.rm = TRUE),
      .before = 1L
    )

  dplyr::bind_rows(cases, deaths) %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::cols_label(`T` = "Total", C = "Confirmed", P = "Probable") %>%
    gt::cols_align("right") %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_body("COVID-19")
      )
    ) %>%
    gt::fmt_number(c("T", "C", "P"), decimals = 0L)
}
