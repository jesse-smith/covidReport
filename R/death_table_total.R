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
    gt::cols_align("right") %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold", align = "center"),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_body("died")
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = c("top", "bottom"), weight = NULL),
      locations = gt::cells_body(rows = 1:2)
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = c("left", "right"), weight = NULL),
      locations = list(
        gt::cells_column_labels("n"),
        gt::cells_body(columns = "n")
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "right", weight = NULL),
      locations = list(
        gt::cells_column_labels("died"),
        gt::cells_body(columns = "died")
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "left", weight = NULL),
      locations = list(
        gt::cells_column_labels("percent"),
        gt::cells_body(columns = "percent")
      )
    )
}
