#' Count Vaccinations by Dose
#'
#' @param .data Vaccination data; default loads and parses the latest file
#'
#' @param resident_only Should counts include Shelby County residents only? If
#'   so, persons of unknown residency will still be included.
#'
#' @param date The date of the vaccination data to load; defaults to latest
#'   data
#'
#' @return A `tibble` with columns `dose_count` and `n`
#'
#' @export
vac_count <- function(
  .data = coviData::vac_prep(coviData::vac_load(date = date)),
  resident_only = TRUE,
  date = NULL
) {

  .data %>%
    purrr::when(
      resident_only ~ dplyr::filter(
        .,
        .data[["resident"]] | is.na(.data[["resident"]])
      ),
      ~ .
    ) %>%
    dplyr::count(.data[["dose_count"]])
}
