test_that("`death_table_total()` info matches reference", {
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

  tbl_ref <- tibble::tribble(
    ~ `COVID-19 Deaths`,       ~ N,           ~ `%`,
                  "Yes",   "9,999",  "10.0&percnt;",
                   "No",  "90,001",  "90.0&percnt;",
                "Total", "100,000", "100.0&percnt;"
  )


  tbl_deaths <- suppressWarnings(
    death_table_total(data, date = "2021-03-27") %>%
      gt::as_raw_html() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble()
  )

  expect_equal(tbl_deaths, tbl_ref)
})

test_that("`death_table_total()` html matches snapshot", {
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


  tbl_html <- suppressWarnings(
    death_table_total(data, date = "2021-03-27") %>%
      gt::as_raw_html() %>%
      xml2::read_html()
  )

  expect_snapshot(tbl_html)
})
