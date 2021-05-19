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
             ~ x,   ~ total, ~ confirmed, ~ probable,
         "Cases", "100,000",    "80,000",   "20,000",
        "Deaths",   "9,999",     "7,999",    "2,000"
  )

  capture.output(
    {tbl_cp <- case_table_confirmed_probable(data, date = "2021-03-27") %>%
      flextable::flextable_to_rmd() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble() %>%
      set_colnames(janitor::make_clean_names(.[1L,])) %>%
      dplyr::filter(dplyr::row_number() > 1L)},
    file = "NUL"
  )

  expect_equal(tbl_cp, tbl_ref)
})

test_that("`void(case_table_confirmed_probable())` matches snapshot", {

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

  tbl_void <- flextable::void(
    case_table_confirmed_probable(data, date = "2021-03-27"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})
