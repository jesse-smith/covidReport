#' Get Latest Date Contained in Vaccination File
#'
#' @param date The date of the file to use; defaults to most recent. If you
#'   know this, you don't actually need this function; it's here for
#'   compatiblity reasons only.
#'
#' @export
vac_date <- function(date = NULL) {
  coviData::path_vac(date = date) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
    coviData::std_dates(orders = "ymd", force = "dt", train = FALSE)
}
