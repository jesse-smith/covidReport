
#' Create a NEW Table of Vaccination Totals
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
vac_table_totals_email <- function(
  vacs = coviData:::vac_prep(coviData::read_vac(date)),
  people = coviData:::vac_prep(coviData::read_vac(date), distinct = TRUE),
  date = NULL
) {

  pop <- 937166

  today <- date_vac(date)

library("dplyr")

  title <- paste0(
    "People Vaccinated (", format(today, "%m/%d/%y"), ")"
  )
  count_people <- people %>%
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(.data[["recip_fully_vacc"]]) ~ "Initiated",
        .data[["recip_fully_vacc"]] == FALSE ~ "Initiated",
        .data[["recip_fully_vacc"]] == TRUE & is.na(.data[["boost_date"]]) ~ "Completed",
        .data[["recip_fully_vacc"]] == TRUE & !is.na(.data[["boost_date"]]) ~ "Additional Dose"
      ),
      .before = 1L
    )  %>%
    dplyr::group_by(.data[["status"]]) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::arrange(dplyr::desc(.data[["status"]])) %>%
    dplyr::mutate(pct_pop = .data[["n"]] / {{ pop }})

  count_people$pct_pop <- round(count_people$pct_pop*100, 1)

  count_people%>%
    janitor::adorn_totals()%>%
      gt::gt() %>%
      gt::cols_label(
        status = gt::html("<b>Status</b>"),
        n = gt::html("<b>N</b>"),
        pct_pop = gt::html("<b>% Population</b>")
      ) %>%
      gt::tab_header(gt::html("<b>", title, "</b>")) %>%
      gt::fmt_number("n", decimals = 0L) %>%
      fmt_covid_table(total = TRUE)

}










#' Create NEW Table of Recent and Total Vaccine Doses
#'
#' @param data TennIIS vaccination data, as output from
#'   \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param date The download date of the data to use; defaults to the most recent
#'   file
#'
#' @return A `gt_tbl`
#'
#' @export
vac_table_recent_email <- function(
  data = coviData:::vac_prep(coviData::read_vac(date)),
  date = NULL
) {

  today <- vac_date(date)

  n_total <- data %>%
    vac_count(by = "dose", filter_2nd_dose = FALSE) %>%
    dplyr::pull("n") %>%
    sum(na.rm = TRUE)

  n_last_week <- data %>%
    dplyr::mutate(
      vacc_date = coviData::std_dates(
        .data[["vacc_date"]],
        orders = "mdy",
        force = "dt",
        train = FALSE
      )
    ) %>%
    dplyr::filter(.data[["vacc_date"]] > today - 7L) %>%
    vac_count(by = "dose", filter_2nd_dose = FALSE) %>%
    dplyr::pull("n") %>%
    sum(na.rm = TRUE)

  title <- paste0("COVID-19 Vaccinations (", format(today, "%m/%d/%y"), ")")

  tibble::tibble(
    n_total,
    n_last_week
  ) %>%
    gt::gt() %>%
    gt::tab_header(gt::html("<b>", title, "</b>")) %>%
    gt::cols_label(
      n_total = gt::html("<b>Total Doses</b>"),
      n_last_week = gt::html("<b>Doses Reported<br>Within Last 7 Days</b>")
    ) %>%
    gt::fmt_number(columns = gt::everything(), decimals = 0L) %>%
    fmt_covid_table() %>%
    # Remove bold weighting of labels
    gt::tab_style(
      gt::cell_text(weight = "normal"),
      locations = gt::cells_body(rows = 1L)
    )

}
