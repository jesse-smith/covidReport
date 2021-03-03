#' Get Latest Date Contained in Vaccination File
#'
#' @param data Vaccination data
#'
#' @param date The date of the file to use; defaults to most recent
#'
#' @param resident_only Should non-residents be filtered from the data?
#'
#' @return A `Date`
#'
#' @export
vac_date <- function(
  data = NULL,
  date = NULL,
  resident_only = TRUE
) {
  if (vec_is_empty(data)) {
    data <- coviData::vac_prep(coviData::vac_load(date = date))
  }
  data %>%
    purrr::when(
      resident_only ~ dplyr::filter(., .data[["resident"]]),
      ~ .
    ) %>%
    dplyr::pull("vacc_date") %>%
    coviData::std_dates(orders = "mdY", force = "dt") %>%
    max(na.rm = TRUE)
}
