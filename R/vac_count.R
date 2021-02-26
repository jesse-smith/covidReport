#' Count Vaccinations by Dose
#'
#' @param .data Vaccination data; default loads and parses the latest file
#'
#' @param resident_only Should counts include Shelby County residents only? If
#'   so, persons of unknown residency will still be included.
#'
#' @return A `tibble` with columns `dose_count` and `n`
#'
#' @export
vac_count <- function(
  .data = coviData::vac_load() %>% coviData::vac_prep(),
  resident_only = TRUE
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
