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
    ~ `covid_19_deaths`,       ~ n,       ~ percent,
                  "Yes",   "9,999",  "10.0%",
                   "No",  "90,001",  "90.0%",
                "Total", "100,000", "100.0%"
  )


  capture.output(
    {tbl_deaths <- death_table_total(data, date = "2021-03-27") %>%
      flextable::flextable_to_rmd() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble() %>%
      set_colnames(janitor::make_clean_names(.[1L,])) %>%
      dplyr::filter(dplyr::row_number() > 1L)},
    file = "NUL"
  )

  expect_equal(tbl_deaths, tbl_ref)
})

test_that("`void(death_table_total())` matches snapshot", {
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


  tbl_void <- flextable::void(
    death_table_total(data, date = "2021-03-27"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})
