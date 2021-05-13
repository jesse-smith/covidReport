#' Tabulate Age Median and Range for COVID-19 Deaths
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
death_table_age <- function(
  data = coviData::process_positive_people(),
  date = NULL
) {
  age <- data %>%
    filter_deaths() %>%
    dplyr::transmute(age = as.integer(.data[["age_in_years"]])) %>%
    dplyr::summarize(
      median = stats::median(.data[["age"]], na.rm = TRUE),
      range = paste0(range(.data[["age"]], na.rm = TRUE), collapse = "-")
    )

  age %>%
    flextable::flextable() %>%
    flextable::set_header_labels(median = "Median Age", range = "Age Range") %>%
    fmt_covid_table(align_label = "center") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::bold(bold = FALSE) %>%
    flextable::autofit()
}
