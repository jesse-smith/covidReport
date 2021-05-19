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
  data = coviData::process_positive_people(date = date),
  date = NULL
) {
  age <- data %>%
    filter_deaths() %>%
    dplyr::transmute(age = as.integer(.data[["age_in_years"]])) %>%
    dplyr::summarize(
      median = stats::median(.data[["age"]], na.rm = TRUE),
      range = paste0(range(.data[["age"]], na.rm = TRUE), collapse = "-")
    )

  data %>%
    death_calc_age(date = date) %>%
    dplyr::transmute(
      .data[["median"]],
      range = paste0(.data[["min"]], "-", .data[["max"]])
    ) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(median = "Median Age", range = "Age Range") %>%
    fmt_covid_table(align_label = "center") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::bold(bold = FALSE) %>%
    flextable::autofit()
}

#' Calculate Age Minimum, Median, and Maximum for COVID-19 Deaths
#'
#' @inheritParams death_table_age
#'
#' @return A `tibble`
#'
#' @keywords internal
death_calc_age <- function(
  data = coviData::process_positive_people(date = date),
  date = NULL
) {
  data %>%
    filter_deaths() %>%
    dplyr::transmute(age = as.integer(.data[["age_in_years"]])) %>%
    dplyr::summarize(
      median = stats::median(.data[["age"]], na.rm = TRUE),
      min = min(.data[["age"]], na.rm = TRUE),
      max = max(.data[["age"]], na.rm = TRUE)
    )
}
