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
  by = c("person", "dose"),
  resident_only = TRUE,
  date = NULL
) {

  by <- rlang::arg_match(by)[[1L]]

  .data %>%
    purrr::when(
      resident_only ~ dplyr::filter(
        .,
        .data[["resident"]] | is.na(.data[["resident"]])
      ),
      ~ .
    ) %>%
    dplyr::mutate(
      max_doses = dplyr::case_when(
        .data[["cvx_code"]] %in% c("210", "212") ~ 1L,
        .data[["cvx_code"]] %in% c("207", "208") ~ 2L,
        TRUE ~ NA_integer_
      ),
      recip_fully_vacc = .data[["dose_count"]] == .data[["max_doses"]]
    ) %>%
    dplyr::filter(
      .data[["dose_count"]] <= .data[["max_doses"]] |
        is.na(.data[["max_doses"]])
    ) %>%
    purrr::when(by == "person" ~ vac_distinct(.), ~ .) %>%
    dplyr::count(.data[["recip_fully_vacc"]], .data[["dose_count"]])
}

vac_distinct <- function(data) {
  data %>%
    dplyr::mutate(.row_id_tmp_ = dplyr::row_number()) %>%
    dplyr::arrange(.data[["asiis_pat_id_ptr"]], dplyr::desc(.data[["dose_count"]])) %>%
    dplyr::distinct(.data[["asiis_pat_id_ptr"]], .keep_all = TRUE) %>%
    dplyr::arrange(.data[[".row_id_tmp_"]]) %>%
    dplyr::select(-".row_id_tmp_")
}
