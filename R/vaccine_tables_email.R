
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
  data_all = coviData:::vac_prep_all(coviData::read_vac(date = date)),
  data_12 = coviData:::vac_prep(coviData::read_vac(date = date)),
  date = NULL
) {

  pop <- 937166

  today <- date_vac(date)



  title <- paste0(
    "People Vaccinated (", format(today, "%m/%d/%y"), ")"
  )
  dose_12 <- vac_count(data_12) %>%
    dplyr::mutate(
      status = dplyr::if_else(
        .data[["recip_fully_vacc"]] %in% TRUE,
        "Completed",
        "Initiated"
      ),
      .before = 1L
    )  %>%
    dplyr::group_by(.data[["status"]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(.data[["status"]])) %>%
    dplyr::mutate(pct_pop = .data[["n"]] / {{ pop }})

  add_doses <- vac_count(data_all, filter_2nd_dose = FALSE) %>%
    dplyr::mutate(
      status = dplyr::if_else(
        .data[["dose_count"]] %in% 3,
        "Additional Dose",
        "Drop"
      ),
      .before = 1L
    )  %>%
    dplyr::group_by(.data[["status"]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(.data[["status"]])) %>%
    dplyr::mutate(pct_pop = .data[["n"]] / {{ pop }}) %>%
    subset(status != "Drop")

  additional_doses <- sum(add_doses$n)
  additional_doses_pct <- sum(add_doses$pct_pop)

  joined_doses <- dplyr::full_join(dose_12, add_doses)

  joined_doses$n <- ifelse(joined_doses$status == "Completed", joined_doses$n-additional_doses, joined_doses$n)

  joined_doses$pct_pop <- ifelse(joined_doses$status == "Completed", joined_doses$pct_pop-additional_doses_pct, joined_doses$pct_pop)


  joined_doses$pct_pop <- round(joined_doses$pct_pop*100, 1)

  joined_doses%>%
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
  data = coviData:::vac_prep_all(coviData::read_vac(date = date)),
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
