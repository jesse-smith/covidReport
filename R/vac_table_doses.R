vac_table_doses <- function(
  data = coviData::vac_prep(coviData::vac_load(date = date)),
  date = NULL
) {

  today <- vac_date(data = data)

  title <- paste0(
    "People Vaccinated (", format(today, "%m/%d/%y"), ")"
  )
  vac_count(.data = data) %>%
    dplyr::summarize(
      total = sum(.data[["n"]], na.rm = TRUE),
      partial = sum(
        vctrs::vec_slice(.data[["n"]], i = !.data[["recip_fully_vacc"]]),
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
    gt::gt() %>%
    gt::tab_header(gt::html("<b>", title, "</b>")) %>%
    gt::cols_label(
      total = gt::html("<b>Total</b>"),
      partial = gt::html("<b>Partial<br</b>"),
      full1 = gt::html("<b>1 dose</b>"),
      full2 = gt::html("<b>2 dose</b>")
    ) %>%
    gt::tab_spanner(label = gt::html("<b>Full</b>"), columns = c("full1", "full2")) %>%
    gt::fmt_number(gt::everything(), decimals = 0L) %>%
    fmt_covid_table() %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = "total")
    )
}
