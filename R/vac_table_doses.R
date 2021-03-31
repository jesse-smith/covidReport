#' Create Table of Vaccination Dose Totals
#'
#' @param data TennIIS data
#'
#' @param date Download date
#'
#' @return `gt_tbl`
#'
#' @export
vac_table_doses <- function(
  data = coviData::vac_prep(coviData::vac_load(date = date)),
  date = NULL
) {

  today <- vac_date(data = data)

  vac_count(.data = data, by = "dose", filter_2nd_dose = FALSE) %>%
    dplyr::summarize(
      total = sum(.data[["n"]], na.rm = TRUE),
      dose1 = sum(
        vctrs::vec_slice(.data[["n"]], i = .data[["dose_count"]] == 1L),
        na.rm = TRUE
      ),
      dose2 = sum(
        vctrs::vec_slice(.data[["n"]], i = .data[["dose_count"]] == 2L),
        na.rm = TRUE
      )
    ) %>%
    gt::gt() %>%
    gt::cols_label(
      total = gt::html("<b>Vaccinations<br>(Total)</b>"),
      dose1 = gt::html("<b>Vaccinations<br>(1st doses)</b>"),
      dose2 = gt::html("<b>Vaccinations<br>(2nd doses)</b>")
    ) %>%
    gt::fmt_number(gt::everything(), decimals = 0L) %>%
    fmt_covid_table() %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = "total")
    )
}
