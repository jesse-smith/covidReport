#' Create Table of Active Cases by A Categorical Variable
#'
#' @param data Data from the associated `active_calc_*()` function
#'
#' @param grp_lbl Label for grouping variable
#'
#' @return A `flextable`
#'
#' @keywords internal
active_table_ <- function(
  data,
  grp_lbl
) {
  data %>%
    janitor::adorn_totals() %>%
    dplyr::mutate(
      percent = 100 * .data[["percent"]],
      rate = 1e5 * .data[["rate"]],
      rate = vec_assign(.data[["rate"]], i = vec_size(.), value = NA_real_)
    ) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      grp = grp_lbl,
      n = "N",
      rate = "Rate per 100k",
      percent = "% Total"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::colformat_double(j = "rate", digits = 1L) %>%
    flextable::colformat_double(j = "percent", digits = 1L, suffix = "%")
}
