#' Tabulate Active, Deceased, and Inactive/Recovered COVID-19 Cases
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param date Download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
case_table_active <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL
) {
   case_calc_active(data, date = date) %>%
    janitor::adorn_totals() %>%
    dplyr::mutate(percent = 100 * .data[["percent"]]) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      status = "Status",
      n = "N",
      percent = "%"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::colformat_double(j = "percent", digits = 1L, suffix = "%") %>%
    flextable::autofit()
}

#' Calculate Active, Deceased, and Inactive/Recovered COVID-19 Cases
#'
#' @inheritParams case_table_active
#'
#' @return A `tibble`
#'
#' @keywords internal
case_calc_active <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL
) {
  date <- date_inv(date)

  a_cols <- c("illness_onset_dt", "specimen_coll_dt", "inv_start_dt")

  data %>%
    dplyr::mutate(.id_tmp_ = dplyr::row_number()) %>%
    dplyr::mutate(
      active = .data[[".id_tmp_"]] %in% filter_active(.)[[".id_tmp_"]],
      died = .data[[".id_tmp_"]] %in% filter_deaths(.)[[".id_tmp_"]]
    ) %>%
    dplyr::transmute(
      status = dplyr::case_when(
        .data[["died"]] ~ "Deceased",
        .data[["active"]] ~ "Active",
        TRUE ~ "Inactive"
      )
    ) %>%
    janitor::tabyl(.data[["status"]]) %>%
    purrr::when(
      "Active" %in% dplyr::pull(., "status") ~ .,
      ~ dplyr::add_row(
        .,
        status = "Active",
        n = 0L,
        percent = 0
      )
    ) %>%
    purrr::when(
      "Deceased" %in% dplyr::pull(., "status") ~ .,
      ~ dplyr::add_row(
        .,
        status = "Deceased",
        n = 0L,
        percent = 0
      )
    ) %>%
    purrr::when(
      "Inactive" %in% dplyr::pull(., "status") ~ .,
      ~ dplyr::add_row(
        .,
        status = "Inactive",
        n = 0L,
        percent = 0
      )
    ) %>%
    dplyr::arrange(.data[["status"]]) %>%
    dplyr::mutate(
      percent = .data[["percent"]] %>%
        round(digits = 3) %>%
        vec_assign(i = 3L, 1 - vec_slice(., 1L) - vec_slice(., 2L))
    ) %>%
    dplyr::as_tibble()
}
