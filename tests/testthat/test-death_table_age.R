test_that("`death_table_age()` info matches reference", {
  data <- tibble::tibble(
    die_from_illness_ind = rep(
      c("Y", rep("N", 3L), rep("U", 3L), rep(NA, 3L)),
      1e4L
    ),
    inv_death_dt = seq(as.Date("2020-03-26"), as.Date("2021-03-26"), by = 1) %>%
      as.character() %>%
      sample(size = 99999L, replace = TRUE) %>%
      purrr::prepend(NA_character_),
    age_in_years = as.character(round(seq(10, 100, length.out = 1e5L)))
  ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_ref <- tibble::tribble(
      ~ median_age, ~ age_range,
              "55",    "10-100"
  )

  capture.output(
    {tbl_age <- death_table_age(data, date = "2021-03-27") %>%
      flextable::flextable_to_rmd() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble() %>%
      set_colnames(janitor::make_clean_names(.[1L,])) %>%
      dplyr::filter(dplyr::row_number() > 1L)},
    file = "NUL"
  )

  expect_equal(tbl_age, tbl_ref)
})

test_that("`void(death_table_age())` matches snapshot", {
  data <- tibble::tibble(
    die_from_illness_ind = rep(
      c("Y", rep("N", 3L), rep("U", 3L), rep(NA, 3L)),
      1e4L
    ),
    inv_death_dt = seq(as.Date("2020-03-26"), as.Date("2021-03-26"), by = 1) %>%
      as.character() %>%
      sample(size = 99999L, replace = TRUE) %>%
      purrr::prepend(NA_character_),
    age_in_years = as.character(round(seq(10, 100, length.out = 1e5L)))
  ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_void <- flextable::void(
    death_table_age(data, date = "2021-03-27"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})
