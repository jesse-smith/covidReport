death_table_total <- function(
  data = coviData::process_positive_people(date = date),
  date = NULL
) {
  data %>%
    dplyr::transmute(
      died = .data[["inv_death_dt"]] %>%
        is.na() %>%
        dplyr::if_else(FALSE, .data[["die_from_illness_ind"]] %in% "Y") %>%
        dplyr::if_else("Yes", "No")
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
