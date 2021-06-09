#' Summarize Population Demopgraphics
#'
#' @param cols `character`. The column(s) to summarize.
#'
#' @return A `tibble` with a column for each
#'   `cols` (`int` of `cols = "age"`, `fct` otherwise) and corresponding
#'   `population` (`int`)
#'
#' @keywords internal
count_pop <- function(cols = c("age", "sex", "race", "ethnicity")) {
  # Force evaluation to avoid a {tidyselect} warning below
  force(cols)
  cols <- as.list(select_colnames(covidReport::pop_2019, {{ cols }}))
  cols <- rlang::syms(cols)

  covidReport::pop_2019 %>%
    dplyr::group_by(!!!cols) %>%
    dplyr::summarize(n = sum(.data[["population"]], na.rm = TRUE)) %>%
    dplyr::ungroup()
}
