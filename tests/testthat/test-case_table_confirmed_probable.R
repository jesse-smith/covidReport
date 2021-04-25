test_that("`case_table_confirmed_probable()` returns expected table", {

  data <- tibble::tibble(
    inv_case_status = c(rep("C", 8e4L), rep("P", 2e4L)),
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

  tbl <- case_table_confirmed_probable(data, date = "2021-03-27") %>%
    gt::as_raw_html()

  expect_snapshot(tbl)
})
