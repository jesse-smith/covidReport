#' Tabulate PCR Test Totals
#'
#' @param data PCR test data, read from an NBS snapshot file
#'
#' @param date The download date of the data to read; defaults to most recent
#'
#' @return A `gt_tbl`
#'
#' @export
pcr_table_total <- function(
  data = coviData::read_file_delim(coviData::path_pcr(date)),
  date = NULL
) {
  n_positive <- NROW(coviData::process_positive_tests(data, date = date))
  n_negative <- NROW(coviData::process_negative_tests(data, date = date))
  n_total    <- n_positive + n_negative

  tibble::tibble(
    result = c("Positive", "Negative", "Total"),
    n = c(n_positive, n_negative, n_total),
    percent = .data[["n"]] / n_total
  ) %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::cols_label(result = "PCR Result", n = "N", percent = "%") %>%
    gt::fmt_number("n", decimals = 0L) %>%
    gt::fmt_percent("percent", decimals = 1L, drop_trailing_zeros = TRUE) %>%
    gt::cols_align("right") %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_body(rows = 3L)
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_column_labels(gt::everything())
    )
}
