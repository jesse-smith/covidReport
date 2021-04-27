test_that("`death_table_age()` matches snapshot", {
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

  age_tbl <- suppressWarnings(
    gt::as_raw_html(death_table_age(data, date = "2021-03-27"))
  )

  expect_snapshot(age_tbl)
})
