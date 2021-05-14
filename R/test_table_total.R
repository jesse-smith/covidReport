#' Tabulate PCR Test Totals
#'
#' @param data PCR test data, read from an NBS snapshot file
#'
#' @param date The download date of the data to read; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
test_table_total <- function(
  data = coviData::read_file_delim(coviData::path_pcr(date)),
  date = NULL
) {
  n_positive <- NROW(coviData::process_positive_tests(data, date = date))
  n_negative <- NROW(coviData::process_negative_tests(data, date = date))
  n_total    <- n_positive + n_negative

  tibble::tibble(
    result = c("Positive", "Negative", "Total"),
    N = c(n_positive, n_negative, n_total),
    percent = 100 * .data[["N"]] / n_total
  ) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(result = "PCR Result", percent = "%") %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::colformat_double(j = "percent", digits = 1L, suffix = "%") %>%
    flextable::autofit()
}
