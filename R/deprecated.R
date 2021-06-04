
#' Get date of a TennIIS vaccination file
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use the
#' \code{\link[coviData:date-data]{date_vac()}} function, which is now the
#' backend for this function.
#'
#' @inherit coviData::date_vac params return
#'
#' @export
#'
#' @keywords internal
vac_date <- function(date = NULL) coviData::date_vac(date = date)

#' Filter Vaccination Data to Possible Shelby County Residents
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use the
#' \code{\link[coviData:vac_filter_residents]{vac_filter_residents()}} function,
#' which is now the backend for this function.
#'
#' @inherit coviData::vac_filter_residents params return
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @keywords internal
vac_residents <- function(
  data = coviData::vac_prep(coviData::read_vac(date = date)),
  date = NULL
) {
  coviData::vac_filter_residents(data)
}

#' Plot Vaccinations by Age
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use the
#' \code{\link[covidReport:vac_plot_age]{vac_plot_age()}} function, which is now
#' the backend for this function.
#'
#' @inherit vac_plot_age params return
#'
#' @param pct Deprecated.
#'
#' @param incl_under_15 Deprecated.
#'
#' @export
#'
#' @keywords internal
plot_vac_age <- function(
  .data = coviData::vac_prep(coviData::read_vac()),
  pct = TRUE,
  by_pop = TRUE,
  incl_under_15 = FALSE,
  incl_under_12 = incl_under_15
) {
  vac_plot_age(
    .data = .data,
    by_pop = by_pop,
    incl_under_12 = incl_under_12
  )
}

#' Plot Vaccination Goal
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use the
#' \code{\link[covidReport:vac_plot_goal]{vac_plot_goal()}} function, which is
#' now the backend for this function.
#'
#' @inherit vac_plot_goal params return
#'
#' @export
#'
#' @keywords internal
plot_vaccinations <- function(
  data = coviData::vac_prep(coviData::read_vac(date = date)),
  date = NULL,
  n_vaccinated = NULL,
  n_first = NULL,
  n_second = NULL,
  n_goal = 700000,
  n_max  = 937166,
  date_updated = NULL,
  resident_only = TRUE
) {
  vac_plot_goal(
    data = data,
    date = date,
    n_vaccinated = n_vaccinated,
    n_first = n_first,
    n_second = n_second,
    n_goal = n_goal,
    n_max = n_max,
    date_updated = date_updated,
    resident_only = resident_only
  )
}
