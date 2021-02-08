vac_date <- function(
  date = NULL,
  distinct = FALSE,
  resident_only = TRUE
) {
  coviData::vac_load(date = date) %>%
    coviData::vac_prep(distinct = distinct) %>%
    purrr::when(
      resident_only ~ dplyr::filter(., .data[["resident"]]),
      ~ .
    ) %>%
    dplyr::pull("vacc_date") %>%
    coviData::std_dates() %>%
    lubridate::as_date() %>%
    max(na.rm = TRUE)
}
