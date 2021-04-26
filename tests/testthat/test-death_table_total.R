test_that("`death_table_total()` output expected table", {
  data <- tibble::tibble(
    die_from_illness_ind = rep(
      c("Y", rep("N", 3L), rep("U", 3L), rep(NA, 3L)),
      1e4L
    ),
    inv_death_dt = seq(as.Date("2021-03-26"), as.Date("2021-03-26"), by = 1) %>%
      as.character() %>%
      sample(size = 99999L, replace = TRUE) %>%
      purrr::prepend(NA_character_)
  ) %>%
    dplyr::slice_sample(prop = 1)

  tbl <- suppressWarnings(
    gt::as_raw_html(death_table_total(data, date = "2021-03-27"))
  )

  expect_snapshot(tbl)
})
