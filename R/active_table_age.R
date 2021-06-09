#' Tabluate Active Cases by Age
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
      age_yrs = dplyr::coalesce(
        .data[["age_in_years"]],
        .data[["age_test"]],
        .data[["age_start_dt"]]
      ),
      grp = active_age_grp(.data[["age_yrs"]])
    ) %>%
    dplyr::count(.data[["grp"]]) %>%
    active_join_age_pop() %>%
    dplyr::transmute(
      .data[["grp"]],
      n = .data[["n_active"]],
      percent = .data[["n"]] / sum(.data[["n"]], na.rm = TRUE),
      rate = .data[["n"]] / .data[["n_pop"]]
    ) %>%
    dplyr::as_tibble()
}

active_age_grp <- function(dbl) {
  vctrs::vec_assert(dbl, ptype = double())
  breaks <- c(0, 18, seq(25, 85, by = 10), 115)
  lbls <- c(
    "0-17",
    "18-24",
    "25-34",
    "35-44",
    "45-54",
    "55-64",
    "65-74",
    "75-84",
    "85+"
  )

  cut(
    dbl,
    breaks = breaks,
    labels = lbls,
    right = FALSE,
    ordered_result = TRUE
  ) %>% as.character()
}

active_join_age_pop <- function(data) {
  pop_age <- covidReport::pop_2019 %>%
    dplyr::mutate(
      grp = .data[["age"]] %>%
        as.double() %>%
        active_age_grp()
    ) %>%
    dplyr::group_by(.data[["grp"]]) %>%
    dplyr::summarize(n = sum(.data[["population"]]))
  data %>%
    dplyr::left_join(
      pop_age,
      by = "grp",
      suffix = c("_active", "_pop")
    )
}
