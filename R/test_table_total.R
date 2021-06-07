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
  data = process_pcr(read_pcr(date)),
  date = NULL
) {
  data %>%
    test_calc_total(date = date) %>%
    dplyr::mutate(percent = 100 * .data[["percent"]]) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      result = "PCR Result",
      n = "N",
      percent = "%"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::colformat_double(j = "percent", digits = 1L, suffix = "%") %>%
    flextable::autofit()
}

#' Calculate PCR Test Totals
#'
#' @inheritParams test_table_total
#'
#' @return A `tibble`
#'
#' @keywords internal
test_calc_total <- function(
  data = process_pcr(read_pcr(date)),
  date = NULL
) {
  n_positive <- NROW(pos(data))
  n_negative <- NROW(neg(data))
  n_total    <- n_positive + n_negative

  tibble::tibble(
    result = c("Positive", "Negative", "Total"),
    n = c(n_positive, n_negative, n_total),
    percent = .data[["n"]] / n_total
  )
}
