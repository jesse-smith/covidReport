vac_table_recent <- function(
  data = coviData::vac_prep(coviData::vac_load(date = date)),
  date = NULL
) {

  today <- vac_date(data = data)

  residents <- data %>%
    vac_residents() %>%
    dplyr::mutate(
      vacc_date = coviData::std_dates(
        .data[["vacc_date"]],
        orders = "mdy",
        force = "dt",
        train = FALSE
      )
    )
  remove(data)

  n_total <- NROW(residents)
  n_last_day <- NROW(dplyr::filter(residents, .data[["vacc_date"]] == today))
  n_last_week <- NROW(
    dplyr::filter(residents, .data[["vacc_date"]] > today - 7L)
  )

  title <- paste0("COVID-19 Vaccinations (", format(today, "%m/%d/%y"), ")")

  tibble::tibble(n_total, n_last_day, n_last_week) %>%
    gt::gt() %>%
    gt::tab_header(gt::html("<b>", title, "</b>")) %>%
    gt::cols_label(
      n_total = gt::html("<b>Total Vaccinations</b>"),
      n_last_day = gt::html("<b>Vaccinations Reported<br>Within Last Day</b>"),
      n_last_week = gt::html("<b>Vaccinations Reported<br>Within Last 7 Days</b>")
    ) %>%
    gt::fmt_number(columns = gt::everything(), decimals = 0L) %>%
    fmt_covid_table()
}
