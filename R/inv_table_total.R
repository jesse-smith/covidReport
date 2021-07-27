#' Tabluate Summary Investigation Numbers
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param nit_token API token/key for accessing the NIT REDcap project
#'
#' @param prior_contacts Number of contacts recorded prior to REDcap
#'   implementation
#'
#' @param date The download date of `data`; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
inv_table_total <- function(
  data = pos(process_inv(read_inv(date = date))),
  nit_token = Sys.getenv("redcap_NIT_token"),
  prior_contacts = 45669L,
  date = NULL
) {

  inv_opened_lbl <- "Opened Investigations"
  inv_closed_lbl <- "Closed Investigations"
  contacts_total_lbl <- "Contacts Identified to Date"
  contacts_14_lbl <- "Contacts Identified in Last 14 Days"

  data %>%
    inv_calc_total(
      nit_token = nit_token,
      prior_contacts = prior_contacts,
      date = date
    ) %>%
    dplyr::mutate(
      measure = dplyr::case_when(
        .data[["measure"]] == "inv_opened" ~ {{ inv_opened_lbl }},
        .data[["measure"]] == "inv_closed" ~ {{ inv_closed_lbl }},
        .data[["measure"]] == "contacts_total" ~ {{ contacts_total_lbl }},
        .data[["measure"]] == "contacts_14" ~ contacts_14_lbl,
        TRUE ~ NA_character_
      )
    ) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(measure = "Number of...", n = "N") %>%
    flextable::add_footer_lines(
      values = "Data Source: Shelby County Health Department"
    ) %>%
    fmt_covid_table() %>%
    flextable::autofit()
}

#' Calculate Summary Investigation Numbers
#'
#' @inheritParams inv_table_total
#'
#' @return A `tibble`
#'
#' @keywords internal
inv_calc_total <- function(
  data = pos(process_inv(read_inv(date = date))),
  nit_token = Sys.getenv("redcap_NIT_token"),
  prior_contacts = 45669L,
  date = NULL
) {
  opened_inv <- NROW(data)
  closed_inv <- NROW(
    dplyr::filter(data, .data[["investigation_status_cd"]] == "C")
  )

  if (is.null(date)) {
    date <- date_inv(date)
  }

  date <- lubridate::as_date(date)

  contacts <- redcap_contacts(date)

  contacts_total <- sum(contacts[["n"]], na.rm = TRUE) + prior_contacts
  contacts_14 <- contacts %>%
    dplyr::filter({{ date }} - 13L <= .data[["date"]]) %>%
    dplyr::pull("n") %>%
    sum(na.rm = TRUE)

  tibble::tribble(
    ~ measure,            ~ n,
    "inv_opened",     opened_inv,
    "inv_closed",     closed_inv,
    "contacts_total", contacts_total,
    "contacts_14",    contacts_14
  )
}

#' Download and Summarize REDcap Contacts by Date
#'
#' @param date The date through which to summarize data
#'
#' @param nit_token API token/key for NIT REDcap project
#'
#' @return A `tibble` with columns `date` and `n` and rows for each day from
#'   `2021-02-02` to the date given in the `date` parameter
#'
#' @noRd
redcap_contacts <- function(
  date,
  min_date = as.Date("2021-02-02"),
  contacts_only = TRUE,
  nit_token = Sys.getenv("redcap_NIT_token")
) {

  min_dt <- lubridate::as_date(min_date)
  max_dt <- lubridate::as_date(date)

  filter <- paste0(
    if (contacts_only) '[n_contacts] > 0 AND ' else '',
    '"', min_dt, ' 00:00:00" <= [date] AND [date] <= "', max_dt, ' 23:59:59"'
  )

  api_url <- "https://redcap.shelbycountytn.gov/api/"

  params <- list(
    token             = nit_token,
    content           = "record",
    format            = "json",
    rawOrLabels       = "raw",
    rawOrLabelHeaders = "raw",
    filterLogic       = filter,
    `fields[0]`       = "date",
    `fields[1]`       = "n_contacts"
  )

  httr::RETRY(
    "POST",
    url = api_url,
    body = params,
    encode = "form",
    httr::progress(con = stderr())
  ) %>%
    httr::stop_for_status(paste("RedCAP download:", httr::content(.))) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    dplyr::transmute(
      date = coviData::std_dates(
        dplyr::na_if(.data[["date"]], ""),
        orders = c("ymdR", "ymdT", "ymd"),
        train = FALSE,
        force = "dt"
      ),
      n = tidyr::replace_na(as.integer(.data[["n_contacts"]]), replace = 0L)
    ) %>%
    dplyr::filter(
      {{ min_dt }} <= .data[["date"]],
      .data[["date"]] <= {{ max_dt }}
    ) %>%
    dplyr::group_by(.data[["date"]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    tidyr::complete(
      "date" = seq(min_dt, max_dt, by = 1L),
      fill = list(n = 0L)
    )
}
