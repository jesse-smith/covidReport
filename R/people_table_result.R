#' Create Tables for Test Result
#'
#' @param date The download date of the data to use; defaults to the most recent
#'   data
#'
#' @param by Units to report- either `"person"` or `"test"`
#'
#' @return A `gt_tbl` object
#'
#' @export
result_table <- function(date = NULL, by = c("person", "test")) {

  by <- rlang::arg_match(by)[[1L]]

  if (by == "person") {
    data <- coviData::read_file_delim(coviData::path_inv(date))
    n_pos <- NROW(coviData::process_positive_people(data))
    n_neg <- NROW(coviData::process_negative_people(data))

    pos_lbl <- "+ People"
    neg_lbl <- "- People"
    tot_lbl <- "Total People"
  } else {
    data <- coviData::read_file_delim(coviData::path_pcr(date))
    n_pos <- NROW(coviData::process_positive_tests(data))
    n_neg <- NROW(coviData::process_negative_tests(data))

    pos_lbl <- "+ Test"
    neg_lbl <- "- Test"
    tot_lbl <- "Total Tests"
  }
  remove(data)
  n <- n_pos + n_neg

  tibble::tibble(pos = n_pos, neg = n_neg, tot = n) %>%
    gt::gt() %>%
    gt::cols_label(pos = pos_lbl, neg = neg_lbl, tot = tot_lbl) %>%
    fmt_covid_table() %>%
    gt::fmt_number(gt::everything(), decimals = 0L)
}
