#' Create Demographic Table
#'
#' @param data Data from the associated `*_calc_*()` function
#'
#' @param grp_lbl Label for grouping variable
#'
#' @return A `flextable`
#'
#' @keywords internal
demog_table_ <- function(
  data,
  grp_lbl,
  color = "midnightblue",
  peds = FALSE
) {
  total_pop <- sum(count_pop(peds = peds)[["n"]], na.rm = TRUE)
  data %>%
    janitor::adorn_totals() %>%
    dplyr::mutate(
      percent = 100 * .data[["percent"]],
      rate = 1e5 * .data[["rate"]],
      rate = vec_assign(
        .data[["rate"]],
        i = vec_size(.),
        value = 1e5 * .data[["n"]][[vec_size(.)]] / {{ total_pop }}
      )
    ) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      grp = grp_lbl,
      n = "N",
      rate = "Rate per 100k",
      percent = "% Total"
    ) %>%
    fmt_covid_table(total = TRUE, color = color) %>%
    flextable::fontsize(size = 16, part = "all") %>%
    flextable::colformat_double(j = "rate", digits = 1L) %>%
    flextable::colformat_double(j = "percent", digits = 1L, suffix = "%")
}

