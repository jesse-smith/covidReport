#' Tabulate Active, Deceased, and Inactive/Recovered COVID-19 Cases
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param date Download date of the data; defaults to most recent
#'
#' @return A `gt_tbl`
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
      status = dplyr::case_when(
        .data[["died"]] ~ "Deceased",
        .data[["active"]] ~ "Active",
        TRUE ~ "Inactive/Recovered"
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
      "Inactive/Recovered" %in% dplyr::pull(., "status") ~ .,
      ~ dplyr::add_row(
        .,
        status = "Inactive/Recovered",
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
    janitor::adorn_totals() %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::cols_label(status = "Status", n = "N", percent = "%") %>%
    gt::fmt_number("n", decimals = 0L) %>%
    gt::fmt_percent("percent", decimals = 1L) %>%
    gt::cols_align("right") %>%
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_body("status")
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_body(columns = "status"),
        gt::cells_body(rows = 4L)
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = c("top", "bottom"), weight = NULL),
      locations = gt::cells_body(rows = 1:3)
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = c("left", "right"), weight = NULL),
      locations = list(
        gt::cells_column_labels("n"),
        gt::cells_body(columns = "n")
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "right", weight = NULL),
      locations = list(
        gt::cells_column_labels("status"),
        gt::cells_body(columns = "status")
      )
    ) %>%
     gt::tab_style(
       style = gt::cell_borders(sides = "left", weight = NULL),
       locations = list(
         gt::cells_column_labels("percent"),
         gt::cells_body(columns = "percent")
       )
     )
}
