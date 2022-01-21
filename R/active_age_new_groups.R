demog_age_grp_2 <- function(dbl) {

  vctrs::vec_assert(dbl, ptype = double())

  breaks <- c(0, 5, 12, 18, seq(25, 85, by = 10), 115)
  lbls <- c(
    "00-04",
    "05-11",
    "12-17",
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


demog_collapse_age_2 <- function(data) {

  v <- c("age", "grp")
  age_var <- v[v %in% colnames(data)]
  vec_assert(age_var, ptype = character(), size = 1L)

  data %>%
    dplyr::mutate(
      {{ age_var }} := .data[[age_var]] %>%
        as.double() %>%
        demog_age_grp_2()
    ) %>%
    dplyr::group_by(.data[[age_var]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    dplyr::ungroup()
}


demog_join_2 <- function(
  data,
  grp = c("age", "sex", "race", "ethnicity"),
  peds = FALSE
) {
  g <- rlang::arg_match(grp)[[1L]]
  pop <- count_pop(g, peds = peds) %>%
    purrr::when(
      g == "age"  ~ demog_collapse_age_2(.),
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




demog_calc_2 <- function(data, grp = c("age", "sex", "race", "ethnicity"), peds = FALSE) {
  grp <- rlang::arg_match(grp)[[1L]]
  data %>%
    dplyr::count(.data[["grp"]]) %>%
    purrr::when(
      grp == "age"  ~ demog_collapse_age_2(.),
      grp == "race" ~ demog_collapse_race_(.),
      ~ .
    ) %>%
    demog_join_2(grp, peds = peds) %>%
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



active_calc_age2 <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    filter_active(date = date) %>%
    active_trans_age() %>%
    demog_calc_2("age")
}


active_table_age2 <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  data %>%
    active_calc_age2(date = date) %>%
    covidReport:::demog_table_(grp_lbl = "Age") %>%
    flextable::autofit(add_h = 0.075)
}


active_plot_age2 <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {
  date <- date_inv(date)
  data %>%
    active_calc_age2(date = date) %>%
    demog_plot_("Active Case Rates", grp = "age", date = date)
}

active_table_age2()
active_plot_age2()
