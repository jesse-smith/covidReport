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
    dplyr::mutate(percent = 100 * .data[["percent"]]) %>%
    dplyr::arrange(c(2L, 1L, 3L)) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      died = "COVID-19 Deaths",
      n = "N",
      percent = "%"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::colformat_double(j = "percent", digits = 1L, suffix = "%") %>%
    flextable::autofit()
}
