#' Create Table of Recent and Total Vaccine Doses
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
vac_table_recent <- function(
  data = coviData::vac_prep(coviData::read_vac(date = date)),
  date = NULL
) {

  today <- vac_date(date)

  n_total <- data %>%
    vac_count(by = "dose") %>%
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
    vac_count(by = "dose") %>%
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
