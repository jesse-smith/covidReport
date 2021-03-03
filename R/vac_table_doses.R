vac_table_doses <- function(
  data = coviData::vac_prep(coviData::vac_load(date = date)),
  date = NULL
) {

  today <- vac_date(data = data)

  title <- paste0(
    "COVID-19 Vaccinations by Doses (", format(today, "%m/%d/%y"), ")"
  )
  vac_count(.data = data) %>%
    janitor::as_tabyl() %>%
    janitor::adorn_totals() %>%
    tidyr::pivot_wider(names_from = "dose_count", values_from = "n") %>%
    dplyr::select(total = "Total", n1 = "1", n2 = "2") %>%
    gt::gt() %>%
    gt::tab_header(gt::html("<b>", title, "</b>")) %>%
    gt::cols_label(
      total = gt::html("<b>Vaccinations<br>(Total)</b>"),
      n1 = gt::html("<b>Vaccinations<br>(1st doses)</b>"),
      n2 = gt::html("<b>Vaccinations<br>(2nd doses)</b>")
    ) %>%
    gt::fmt_number(gt::everything(), decimals = 0L) %>%
    fmt_covid_table()
}
