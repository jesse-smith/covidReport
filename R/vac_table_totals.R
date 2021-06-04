#' Create a Table of Vaccination Totals
#'
#' @param data TennIIS vaccination data, as output by
#'   \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param date The download date of the data to use; defaults to most recent
#'   file
#'
#' @return A `gt_tbl`
#'
#' @export
vac_table_totals <- function(
  data = coviData::vac_prep(coviData::read_vac(date = date)),
  date = NULL
) {

  today <- vac_date(date)

  title <- paste0(
    "People Vaccinated (", format(today, "%m/%d/%y"), ")"
  )
  vac_count(.data = data) %>%
    dplyr::summarize(
      total = sum(.data[["n"]], na.rm = TRUE),
      partial = sum(
        vctrs::vec_slice(
          .data[["n"]],
          i = !.data[["recip_fully_vacc"]] | is.na(.data[["recip_fully_vacc"]])
        ),
        na.rm = TRUE
      ),
      full1  = sum(
        vctrs::vec_slice(
          .data[["n"]],
          i = .data[["recip_fully_vacc"]] & .data[["dose_count"]] == 1L
        ),
        na.rm = TRUE
      ),
      full2 = sum(
        vctrs::vec_slice(
          .data[["n"]],
          i = .data[["recip_fully_vacc"]] & .data[["dose_count"]] == 2L
        ),
        na.rm = TRUE
      )
    ) %>%
    dplyr::mutate(full = .data[["full1"]] + .data[["full2"]]) %>%
    dplyr::select(-c("full1", "full2")) %>%
    gt::gt() %>%
    gt::cols_label(
      total = gt::html("<b>Total</b>"),
      partial = gt::html("<b>Partial</b>"),
      full = gt::html("<b>Full</b>")
    ) %>%
    gt::tab_header(gt::html("<b>", title, "</b>")) %>%
    gt::fmt_number(gt::everything(), decimals = 0L) %>%
    fmt_covid_table()
}
