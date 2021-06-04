#' Count Vaccinations by Dose
#'
#' @param .data Vaccination data; default loads and parses the latest file
#'
#' @param by Should counts be by person, or by dose?
#'
#' @param resident_only Should counts include Shelby County residents only? If
#'   so, persons of unknown residency will still be included.
#'
#' @param filter_2nd_dose Should 2nd doses of single-dose vaccines be filtered
#'   out? For backwards compatibility only- will be removed in the future.
#'
#' @param date The date of the vaccination data to load; defaults to latest
#'   data
#'
#' @return A `tibble` with columns `dose_count` and `n`
#'
#' @export
vac_count <- function(
  .data = coviData::vac_prep(coviData::read_vac(date = date)),
  by = c("person", "dose"),
  resident_only = TRUE,
  filter_2nd_dose = TRUE,
  date = NULL
) {

  by <- rlang::arg_match(by)[[1L]]

  .data %>%
    purrr::when(resident_only ~ coviData::vac_filter_residents(.), ~ .) %>%
    purrr::when(filter_2nd_dose ~ coviData::vac_filter_doses(.), ~ .) %>%
    purrr::when(by == "person" ~ coviData::vac_distinct(.), ~ .) %>%
    dplyr::count(.data[["recip_fully_vacc"]], .data[["dose_count"]])
}
