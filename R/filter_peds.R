#' Filter to Pediatric COVID-19 Cases
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @return `data` filtered to cases less than 18
#'
#' @export
filter_peds <- function(data) {
  data %>%
    dplyr::mutate(
      .dt_start_tmp_ = std_dates(
        .data[["inv_start_dt"]],
        orders = "ymdT",
        force = "dt"
      ),
      .age_in_years_tmp_ = as.double(.data[["age_in_years"]]),
      .age_test_tmp_ = (.data[["specimen_coll_dt"]]-.data[["patient_dob"]]) %>%
        lubridate::as.duration() %>%
        divide_by(lubridate::dyears(1L)),
      .age_start_tmp_ = (.data[[".dt_start_tmp_"]]-.data[["patient_dob"]]) %>%
        lubridate::as.duration() %>%
        divide_by(lubridate::dyears(1L)),
      # `std_age()` is defined in vac_plot_age.R
      dplyr::across(dplyr::starts_with("'.age_"), std_age)
    ) %>%
    dplyr::mutate(
      .age_tmp_ = dplyr::coalesce(
        .data[[".age_in_years_tmp_"]],
        .data[[".age_test_tmp_"]],
        .data[[".age_start_tmp_"]]
      )
    ) %>%
    dplyr::filter(.data[[".age_tmp_"]] < 18, !is.na(.data[[".age_tmp_"]])) %>%
    dplyr::select(-dplyr::matches(c(".age_.*tmp_", ".dt_.*tmp_")))
}
