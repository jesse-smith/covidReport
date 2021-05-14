#' Tabulate Active, Deceased, and Inactive/Recovered COVID-19 Cases
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param date Download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
case_table_active <- function(
  data = coviData::process_positive_people(date = date),
  date = NULL
) {
  date <- coviData::path_inv(date = date) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
    coviData::std_dates(orders = "ymd", force = "dt")

  a_cols <- c("illness_onset_dt", "specimen_coll_dt", "inv_start_dt")

  data %>%
    dplyr::mutate(.id_tmp_ = dplyr::row_number()) %>%
    dplyr::mutate(
      active_dt = dplyr::across(
        {{ a_cols }},
        ~ coviData::std_dates(.x, orders = "ymdT", train = FALSE, force = "dt")
      ) %>%
        dplyr::transmute(dt = coviData::coalesce_across({{ a_cols }})) %>%
        dplyr::pull("dt"),
      active_days = as.integer({{ date }} - .data[["active_dt"]]),
      active = dplyr::between(.data[["active_days"]], 0L, 14L),
      died = .data[[".id_tmp_"]] %in% filter_deaths(.)[[".id_tmp_"]]
    ) %>%
    dplyr::transmute(
      Status = dplyr::case_when(
        .data[["died"]] ~ "Deceased",
        .data[["active"]] ~ "Active",
        TRUE ~ "Inactive"
      )
    ) %>%
    janitor::tabyl(.data[["Status"]]) %>%
    purrr::when(
      "Active" %in% dplyr::pull(., "Status") ~ .,
      ~ dplyr::add_row(
        .,
        Status = "Active",
        n = 0L,
        percent = 0
      )
    ) %>%
    purrr::when(
      "Deceased" %in% dplyr::pull(., "Status") ~ .,
      ~ dplyr::add_row(
        .,
        Status = "Deceased",
        n = 0L,
        percent = 0
      )
    ) %>%
    purrr::when(
      "Inactive" %in% dplyr::pull(., "Status") ~ .,
      ~ dplyr::add_row(
        .,
        Status = "Inactive",
        n = 0L,
        percent = 0
      )
    ) %>%
    dplyr::arrange(.data[["Status"]]) %>%
    dplyr::mutate(
      percent = .data[["percent"]] %>%
        round(digits = 3) %>%
        vec_assign(i = 3L, 1 - vec_slice(., 1L) - vec_slice(., 2L))
    ) %>%
    janitor::adorn_totals() %>%
    dplyr::rename(N = "n") %>%
    dplyr::mutate(percent = 100 * .data[["percent"]]) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(percent = "%") %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::colformat_double(j = "percent", digits = 1L, suffix = "%") %>%
    flextable::autofit()
}
