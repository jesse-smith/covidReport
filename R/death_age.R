#' Plot Active Case Rates by Age
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
death_plot_age <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    death_calc_age(date = date) %>%
    demog_plot_(
      "Death Rates",
      grp = "age",
      date = date,
      color = "grey30"
    )
}

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
death_table_age <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    death_calc_age(date = date) %>%
    demog_table_(grp_lbl = "Age", color = "grey30") %>%
    flextable::autofit(add_h = 0.075)
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
death_calc_age <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_deaths() %>%
    death_trans_age() %>%
    demog_calc_("age")
}

death_trans_age <- function(data) {
  data %>%
    dplyr::transmute(
      death_dt = std_dates(
        .data[["patient_deceased_dt"]],
        orders = "ymdT",
        force = "dt"
      ),
      inv_death_dt = std_dates(
        .data[["inv_death_dt"]],
        orders = "ymdT",
        force = "dt"
      ),
      inv_start_dt = std_dates(
        .data[["inv_start_dt"]],
        orders = "ymdT",
        force = "dt"
      ),
      age_death = (.data[["death_dt"]] - .data[["patient_dob"]]) %>%
        lubridate::as.duration() %>%
        divide_by(lubridate::dyears(1L)),
      age_inv_death = (.data[["inv_death_dt"]] - .data[["patient_dob"]]) %>%
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
        .data[["age_death"]],
        .data[["age_inv_death"]],
        .data[["age_start_dt"]]
      ) %>%
        pmin(85) %>%
        as.integer()
    )
}
