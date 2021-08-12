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

  pop <- 937166

  today <- date_vac(date)

  title <- paste0(
    "People Vaccinated (", format(today, "%m/%d/%y"), ")"
  )
  vac_count(data) %>%
    dplyr::mutate(
      status = dplyr::if_else(
        .data[["recip_fully_vacc"]] %in% TRUE,
        "Completed",
        "Initiated"
      ),
      .before = 1L
    ) %>%
    dplyr::group_by(.data[["status"]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(.data[["status"]])) %>%
    dplyr::mutate(pct_pop = .data[["n"]] / {{ pop }}) %>%
    janitor::adorn_totals() %>%
    gt::gt() %>%
    gt::cols_label(
      status = gt::html("<b>Status</b>"),
      n = gt::html("<b>N</b>"),
      pct_pop = gt::html("<b>% Population</b>")
    ) %>%
    gt::tab_header(gt::html("<b>", title, "</b>")) %>%
    gt::fmt_number("n", decimals = 0L) %>%
    gt::fmt_percent("pct_pop", decimals = 1L) %>%
    fmt_covid_table(total = TRUE)
}
