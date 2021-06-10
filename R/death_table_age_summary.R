#' Tabulate Age Median and Range for COVID-19 Deaths
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
death_table_age_summary <- function(
  data = pos(process_inv(read_inv(date = date))),
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
    death_calc_age_summary(date = date) %>%
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
death_calc_age_summary <- function(
  data = pos(process_inv(read_inv(date = date))),
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
