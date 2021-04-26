#' Tabulated Death and Survival Outcomes from COVID-19
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param date The download data of the data; defaults to most recent
#'
#' @return A `gt_tbl`
#'
#' @export
death_table_total <- function(
  data = coviData::process_positive_people(date = date),
  date = NULL
) {
  data %>%
    dplyr::mutate(.id_tmp_ = dplyr::row_number()) %>%
    dplyr::transmute(
      .died_tmp_ = .data[[".id_tmp_"]] %in% filter_deaths(.)[[".id_tmp_"]],
      died = dplyr::if_else(.data[[".died_tmp_"]], "Yes", "No")
    ) %>%
    janitor::tabyl(.data[["died"]]) %>%
    janitor::adorn_totals() %>%
    dplyr::arrange(c(2L, 1L, 3L)) %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::cols_label(died = "COVID-19 Deaths", n = "N", percent = "%") %>%
    gt::fmt_number("n", decimals = 0L) %>%
    gt::fmt_percent("percent", decimals = 1L) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_body("died")
      )
    ) %>%
    gt::cols_align("right") %>%
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_column_labels(c("n", "percent"))
    )
}
