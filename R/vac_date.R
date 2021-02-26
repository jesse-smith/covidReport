#' Get Latest Date Contained in Vaccination File
#'
#' @param date The date of the file to use; defaults to most recent
#'
#' @param resident_only Should non-residents be filtered from the data?
#'
#' @return A `Date`
#'
#' @export
vac_date <- function(
  date = NULL,
  resident_only = TRUE
) {
  coviData::vac_load(date = date) %>%
    coviData::vac_prep() %>%
    purrr::when(
      resident_only ~ dplyr::filter(., .data[["resident"]]),
      ~ .
    ) %>%
    dplyr::pull("vacc_date") %>%
    coviData::std_dates(orders = "mdY", force = "dt") %>%
    max(na.rm = TRUE)
}
