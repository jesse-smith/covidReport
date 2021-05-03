test_that("`case_table_confirmed_probable()` info matches reference", {

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

  tbl_ref <- tibble::tribble(
    ~ `COVID-19`,   ~ Total, ~ Confirmed, ~ Probable,
         "Cases", "100,000",    "80,000",   "20,000",
        "Deaths",   "9,999",     "7,999",    "2,000"
  )

  tbl_confirmed_probable <- suppressWarnings(
    case_table_confirmed_probable(data, date = "2021-03-27") %>%
      gt::as_raw_html() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble()
  )

  expect_equal(tbl_confirmed_probable, tbl_ref)
})

test_that("`case_table_confirmed_probable()` html matches snapshot", {

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

  tbl_ref <- tibble::tribble(
    ~ `COVID-19`,   ~ Total, ~ Confirmed, ~ Probable,
    "Cases", "100,000",    "80,000",   "20,000",
    "Deaths",   "9,999",     "7,999",    "2,000"
  )

  tbl_html <- suppressWarnings(
    case_table_confirmed_probable(data, date = "2021-03-27") %>%
      gt::as_raw_html() %>%
      xml2::read_html()
  )

  expect_snapshot(tbl_html)
})
