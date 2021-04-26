test_that("`filter_deaths()` removes invalid rows", {
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

  deaths <- filter_deaths(data)

  expect_true(all(deaths$die_from_illness_ind == "Y"))
  expect_true(all(!is.na(deaths$inv_death_dt)))
  expect_snapshot(deaths)
})
