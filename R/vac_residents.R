vac_residents <- function(
  data = coviData::vac_prep(coviData::vac_load(date = date)),
  date = NULL
) {
  dplyr::filter(data, .data[["resident"]] | is.na(.data[["resident"]]))
}
