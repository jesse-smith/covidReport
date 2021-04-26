#' Summarize Cases for Weekend Reporting
#'
#' Summary numbers for weekend reporting. This outputs the first and second
#' rows of the table separately (using the `row` parameter). The first row
#' contains _Total Cases_, _Total Tests_, and _Active Cases_; the second row
#' contains _New Cases_, _Inactive Cases_, and _Total Deaths_.
#'
#' @param date Report date to summarize
#'
#' @param row Row of summary table to output; either `1` or `2`
#'
#' @return A `gt_tbl`
#'
#' @export
case_table_summary <- function(date = NULL, row = c(1L, 2L)) {

  coviData::assert(
    as.integer(row) %in% c(1L, 2L),
    message = "`row` must be `1` or `2`"
  )

  row <- row[[1L]]

  # Some code needs to be run on either row
  pos_ppl <- coviData::process_positive_people(date = date)
  n_pos_ppl <- NROW(pos_ppl)

  rpt_dt <- path_pcr(date = date) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
    std_dates(orders = "mdy", force = "dt")

  n_active <- pos_ppl %>%
    dplyr::mutate(
      active_dt = dplyr::coalesce(
        .data[["illness_onset_dt"]],
        .data[["specimen_coll_dt"]],
        .data[["inv_start_dt"]]
      ) %>% std_dates(orders = "ymdT", force = "dt", train = FALSE),
      active_days = as.integer(rpt_dt - .data[["active_dt"]])
    ) %>%
    dplyr::filter(
      0L <= .data[["active_days"]],
      .data[["active_days"]] <= 14L
    ) %>%
    NROW()

  if (row == 1) {
    pcr <- coviData::read_file_delim(coviData::path_pcr(date))
    n_pos_pcr <- NROW(coviData::process_positive_tests(pcr))
    n_neg_pcr <- NROW(coviData::process_negative_tests(pcr))
    n_tests <- n_pos_pcr + n_neg_pcr

    n1 <- n_pos_ppl
    n2 <- n_tests
    n3 <- n_active

    labels <- c(
      n1 = "Total Confirmed & Probable<br>COVID-19 Cases",
      n2 = "Total COVID-19<br>Tests Performed",
      n3 = "Active<br>COVID-19 Cases"
    )
  } else if (row == 2) {
    n_pos_ppl_prev <- NROW(coviData::process_positive_people(date = rpt_dt-1L))
    n_new <- n_pos_ppl - n_pos_ppl_prev

    n_deaths <- NROW(filter_deaths(pos_ppl))

    n_inactive <- n_pos_ppl - n_active - n_deaths

    n1 <- n_new
    n2 <- n_inactive
    n3 <- n_deaths

    labels <- c(
      n1 = "Newly Confirmed<br>COVID-19 Cases",
      n2 = "Inactive/Recovered<br>COVID-19 Cases",
      n3 = "COVID-19<br>Deaths"
    )
  }

  tibble::tibble(n1 = n1, n2 = n2, n3 = n3) %>%
    gt::gt() %>%
    gt::cols_label(
      n1 = gt::html(labels[[1L]]),
      n2 = gt::html(labels[[2L]]),
      n3 = gt::html(labels[[3L]])
    ) %>%
    fmt_covid_table() %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body()
    ) %>%
    gt::fmt_number(gt::everything(), decimals = 0L)
}
