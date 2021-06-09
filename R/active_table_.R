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

active_join_pop_ <- function(data, grp = c("age", "sex", "race", "ethnicity")) {
  g <- rlang::arg_match(grp)[[1L]]
  pop_cnt <- purrr::when(
    count_pop(g),
    g == "age"  ~ active_collapse_age_(.),
    g == "race" ~ active_collapse_race_(.),
    ~ .
  )
  pop <- dplyr::transmute(
    pop_cnt,
    grp = if (is.factor(.data[[g]])) as.character(.data[[g]]) else .data[[g]],
    .data[["n"]]
  )
  dplyr::left_join(data, pop, by = "grp", suffix = c("_active", "_pop"))
}

#' Calculate Active Cases by A Categorical Variable
#'
#' @inheritParams active_table_
#'
#' @param grp The grouping structure to use; only single-variable grouping is
#'   currently supported
#'
#' @return A `tibble` with columns `grp` (`fct`), `n` (`int`),
#'   `percent` (`dbl`), and `rate` (`dbl`)
#'
#' @keywords internal
active_calc_ <- function(data, grp = c("age", "sex", "race", "ethnicity")) {
  grp <- rlang::arg_match(grp)[[1L]]
  data %>%
    dplyr::count(.data[["grp"]]) %>%
    purrr::when(
      grp == "age"  ~ active_collapse_age_(.),
      grp == "race" ~ active_collapse_race_(.),
      ~ .
    ) %>%
    active_join_pop_(grp) %>%
    dplyr::transmute(
      grp = .data[["grp"]] %>%
        factor() %>%
        forcats::fct_explicit_na("Missing"),
      n = .data[["n_active"]],
      percent = .data[["n"]] / sum(.data[["n"]], na.rm = TRUE),
      rate = .data[["n"]] / .data[["n_pop"]]
    ) %>%
    dplyr::arrange(.data[["grp"]]) %>%
    dplyr::as_tibble()
}

#' Collapse Age Groupings
#'
#' @param data Data with `numeric` ages
#'
#' @return `data` with age groups collapsed
#'
#' @keywords internal
active_collapse_age_ <- function(data) {

  v <- c("age", "grp")
  age_var <- v[v %in% colnames(data)]
  vec_assert(age_var, ptype = character(), size = 1L)

  data %>%
    dplyr::mutate(
      {{ age_var }} := .data[[age_var]] %>%
        as.double() %>%
        active_age_grp_()
    ) %>%
    dplyr::group_by(.data[[age_var]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    dplyr::ungroup()
}

#' Collapse Race Groupings
#'
#' @param data Data with `character` race variable
#'
#' @return A `tibble` with collapsed race variable
#'
#' @keywords internal
active_collapse_race_ <- function(data) {
  v <- c("race", "grp")
  race_var <- v[v %in% colnames(data)]
  vec_assert(race_var, ptype = character(), size = 1L)

  data %>%
    dplyr::mutate(
      {{ race_var }} := .data[[race_var]] %>%
        as.character() %>%
        active_race_grp_()
    ) %>%
    dplyr::group_by(.data[[race_var]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    dplyr::ungroup()
}

#' Calculate Age Grouping
#'
#' @param dbl A `double` vector of ages
#'
#' @return A `factor` of age groups
#'
#' @keywords internal
active_age_grp_ <- function(dbl) {
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

#' Calculate Race Grouping
#'
#' @param dbl A `character` vector of race
#'
#' @return A `factor` of racial groups
#'
#' @keywords internal
active_race_grp_ <- function(chr) {
  chr <- chr %>%
    stringr::str_to_upper() %>%
    stringr::str_squish()

  baa <- "Black/African American"
  w <- "White"
  # api <- "Asian/Pacific Islander"
  # aian <- "American Indian/Alaskan Native"

  dplyr::case_when(
    stringr::str_detect(chr, "(BLACK)|(AFRICAN)") ~ baa,
    stringr::str_detect(chr, "(WHITE)|(CAUCASIAN)") ~ w,
    stringr::str_detect(chr, "(ASIAN)|(PACIFIC)") ~ "Other",
    stringr::str_detect(chr, "(INDIAN)|(NATIVE)") ~ "Other",
    chr == "OTHER" ~ "Other",
    TRUE ~ NA_character_
  )
}
