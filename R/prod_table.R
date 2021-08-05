#' Productivity Summary Table
#'
#' @param assigned Assigned case data from `read_prod()`
#'
#' @param nit_token API token to access NIT RedCAP project
#'
#' @param days Number of days to summarize over
#'
#' @param lag Number of days back to end the calculation
#'
#' @return A `flextable`
#'
#' @export
prod_table <- function(
  assigned = read_prod(sheet = "assign_dt_n"),
  nit_token = Sys.getenv("redcap_NIT_token"),
  days = 14,
  lag = 6
) {
  prod_calc(
    assigned = assigned,
    nit_token = nit_token,
    days = days,
    lag = lag
  ) %>%
    dplyr::add_row(
      measure = "% Reached",
      value = .[["value"]][[2L]]/.[["value"]][[1L]]
    ) %>%
    dplyr::mutate(
      measure = .data[["measure"]] %>%
        stringr::str_replace("assigned", "Cases") %>%
        stringr::str_to_title(),
      value = dplyr::if_else(
        .data[["measure"]] == "% Reached",
        paste0(round(100*.data[["value"]], 1), "%"),
        format(round(.data[["value"]]), big.mark = ",")
      )
    ) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      measure = "Metric",
      value = "Value"
    ) %>%
    fmt_covid_table() %>%
    flextable::autofit()
}

#' Calculate Productivity Summary Statistics
#'
#' @inheritParams prod_table
#'
#' @return A `tibble`
#'
#' @export
prod_calc <- function(
  assigned = read_prod(sheet = "assign_dt_n"),
  nit_token = Sys.getenv("redcap_NIT_token"),
  days = 14,
  lag = 6
) {

  # Date range
  dt_max_data <- max(assigned[["date"]], na.rm = TRUE)
  dt_max <- dt_max_data - lag + 1
  dt_min <- dt_max - days + 1

  # Case assignment summary
  asg_tbl <- assigned %>%
    dplyr::filter(
      dplyr::between(.data[["date"]], {{ dt_min }}, {{ dt_max }})
    ) %>%
    dplyr::summarize(
      assigned = sum(.data[["assigned"]], na.rm = TRUE),
      reached = sum(.data[["reached"]], na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "measure") %>%
    dplyr::mutate(grp = "Cases", .before = 1L)

  # Contacts summary
  cont_tbl <- redcap_contacts(
    dt_max,
    dt_min,
    contacts_only = FALSE,
    nit_token = nit_token
  ) %>%
    dplyr::filter(
      dplyr::between(.data[["date"]], {{ dt_min }}, {{ dt_max }})
    ) %>%
    dplyr::summarize(contacts = sum(.data[["n"]], na.rm = TRUE)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "measure") %>%
    dplyr::mutate(grp = "Contacts", .before = 1L)

  dplyr::bind_rows(asg_tbl, cont_tbl) %>%
    dplyr::filter(.data[["grp"]] %in% c("Cases", "Contacts")) %>%
    dplyr::select(-"grp") %>%
    set_attr("dt_range", c(dt_min, dt_max))
}

#' Read Productivity Data
#'
#' @param sheet Excel sheet to read
#'
#' @param file Name of productivity file - must be xlsx or xls
#'
#' @param dir Directory location of productivity file
#'
#' @return A `tibble`
#'
#' @export
read_prod <- function(
  sheet = c("assign_dt_n", "assign_dt_pct_assign", "assign_dt_pct_complete", "interview_dt"),
  file = "productivity_report.xlsx",
  dir = "V:/Productivity/Daily Report for Administration"
) {
  path <- coviData::path_create(dir, file)
  if (!fs::path_ext(path) %in% c("xls", "xlsx")) {
    rlang::abort("`file` must be an xls or xlsx file")
  }
  sheet_fml <- eval(rlang::fn_fmls()[["sheet"]])
  sheet <- rlang::arg_match(sheet)[[1L]]
  sheet_num <- which(sheet_fml == sheet)

  path %>%
    readxl::read_excel(sheet = sheet_num) %>%
    janitor::clean_names() %>%
    dplyr::mutate(date = lubridate::as_date(date))
}
