death_table_age <- function(
  data = coviData::process_positive_people(),
  date = NULL
) {
  age <- data %>%
    filter_deaths() %>%
    dplyr::transmute(age = as.integer(.data[["age_in_years"]])) %>%
    dplyr::summarize(
      median = median(.data[["age"]], na.rm = TRUE),
      range = paste0(range(.data[["age"]], na.rm = TRUE), collapse = "-")
    )

  age %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::cols_label(median = "Median", range = "Range") %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(gt::everything())
    )
}
