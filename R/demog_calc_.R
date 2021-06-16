#' Calculate Demographic Summary
#'
#' @inheritParams demog_table_
#'
#' @param grp The grouping structure to use; only single-variable grouping is
#'   currently supported
#'
#' @return A `tibble` with columns `grp` (`fct`), `n` (`int`),
#'   `percent` (`dbl`), and `rate` (`dbl`)
#'
#' @keywords internal
demog_calc_ <- function(data, grp = c("age", "sex", "race", "ethnicity"), peds = FALSE) {
  grp <- rlang::arg_match(grp)[[1L]]
  data %>%
    dplyr::count(.data[["grp"]]) %>%
    purrr::when(
      grp == "age"  ~ demog_collapse_age_(.),
      grp == "race" ~ demog_collapse_race_(.),
      ~ .
    ) %>%
    demog_join_(grp, peds = peds) %>%
    dplyr::transmute(
      grp = .data[["grp"]] %>%
        factor() %>%
        forcats::fct_explicit_na("Missing"),
      .data[["n"]],
      percent = .data[["n"]] / sum(.data[["n"]], na.rm = TRUE),
      rate = .data[["n"]] / .data[["n_pop"]]
    ) %>%
    dplyr::arrange(.data[["grp"]]) %>%
    dplyr::as_tibble()
}

# Top-level Helpers ------------------------------------------------------------

#' Relevel Race for Appropriate Ordering
#'
#' @param data Data from a `_calc_race()` function
#'
#' @return `data` with `grp` levels (and rows) reordered
#'
#' @keywords internal
demog_relevel_race <- function(data) {
  data %>%
    dplyr::mutate(
      grp = forcats::fct_relevel(
        .data[["grp"]],
        "Black/African American",
        "White",
        "Other",
        "Missing"
      )
    ) %>%
    dplyr::arrange(.data[["grp"]])
}

# Internal Helpers -------------------------------------------------------------

#' Join to Population Demographics
#'
#' @param data Data summarized by demographic
#'
#' @param grp Demographic group
#'
#' @return A `tibble` with column `n_pop`
#'
#' @keywords internal
demog_join_ <- function(
  data,
  grp = c("age", "sex", "race", "ethnicity"),
  peds = FALSE
) {
  g <- rlang::arg_match(grp)[[1L]]
  pop <- count_pop(g, peds = peds) %>%
    purrr::when(
      g == "age"  ~ demog_collapse_age_(.),
      g == "race" ~ demog_collapse_race_(.),
      ~ .
    ) %>%
    dplyr::transmute(
      grp = if (is.factor(.data[[g]])) as.character(.data[[g]]) else .data[[g]],
      .data[["n"]]
    ) %>%
    dplyr::filter(grp != "Other")
  dplyr::left_join(data, pop, by = "grp", suffix = c("", "_pop"))
}

#' Collapse Age Groupings
#'
#' @param data Data with `numeric` ages
#'
#' @return `data` with age groups collapsed
#'
#' @keywords internal
demog_collapse_age_ <- function(data) {

  v <- c("age", "grp")
  age_var <- v[v %in% colnames(data)]
  vec_assert(age_var, ptype = character(), size = 1L)

  data %>%
    dplyr::mutate(
      {{ age_var }} := .data[[age_var]] %>%
        as.double() %>%
        demog_age_grp_()
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
demog_collapse_race_ <- function(data) {
  v <- c("race", "grp")
  race_var <- v[v %in% colnames(data)]
  vec_assert(race_var, ptype = character(), size = 1L)

  data %>%
    dplyr::mutate(
      {{ race_var }} := .data[[race_var]] %>%
        as.character() %>%
        demog_race_grp_()
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
demog_age_grp_ <- function(dbl) {
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
demog_race_grp_ <- function(chr) {
  chr <- chr %>%
    stringr::str_to_upper() %>%
    stringr::str_squish() %>%
    stringr::str_extract("[A-Z ]+()")

  baa <- "Black/African American"
  w <- "White"
  # api <- "Asian/Pacific Islander"
  # aian <- "American Indian/Alaskan Native"

  dplyr::case_when(
    stringr::str_detect(chr, "(BLACK)|(AFRICAN)") ~ baa,
    stringr::str_detect(chr, "(WHITE)|(CAUCASIAN)") ~ w,
    stringr::str_detect(chr, "(INDIAN)|(NATIVE)") ~ "Other",
    stringr::str_detect(chr, "(ASIAN)|(PACIFIC)") ~ "Other",
    stringr::str_detect(chr, "OTHER") ~ "Other",
    TRUE ~ NA_character_
  )
}
