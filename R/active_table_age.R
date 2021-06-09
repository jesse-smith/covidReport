#' Tabulate Active Cases by Age
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `flextable`
#'
#' @export
active_table_age <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    active_calc_age(date = date) %>%
    active_table_(grp_lbl = "Age") %>%
    flextable::autofit()
}

#' Calculate Active Case Rates and Percentages by Age
#'
#' @inheritParams active_table_age
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
active_calc_age <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_active(date = date) %>%
    active_trans_age() %>%
    active_calc_("age")
}

active_trans_age <- function(data) {
  data %>%
    dplyr::transmute(
      inv_start_dt = std_dates(
        .data[["inv_start_dt"]],
        orders = "ymdT",
        force = "dt"
      ),
      age_in_years = as.double(.data[["age_in_years"]]),
      age_test = (.data[["specimen_coll_dt"]] - .data[["patient_dob"]]) %>%
        lubridate::as.duration() %>%
        divide_by(lubridate::dyears(1L)),
      age_start_dt = (.data[["inv_start_dt"]] - .data[["patient_dob"]]) %>%
        lubridate::as.duration() %>%
        divide_by(lubridate::dyears(1L)),
      # `std_age()` is defined in vac_plot_age.R
      dplyr::across(dplyr::starts_with("'age_"), std_age)
    ) %>%
    dplyr::transmute(
      grp = dplyr::coalesce(
        .data[["age_in_years"]],
        .data[["age_test"]],
        .data[["age_start_dt"]]
      ) %>%
        pmin(85) %>%
        as.integer()
    )
}
