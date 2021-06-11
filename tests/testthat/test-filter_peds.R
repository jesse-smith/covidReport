test_that("`filter_peds()` is equal to snapshot", {

  data <- tibble::tibble(
    patient_dob = rep(
      as.Date("2021-04-01") - seq(1, 36e3, length.out = 100),
      times = 1000L
    ),
    specimen_coll_dt = c(rep(as.Date("2020-03-10"), 99e3L), rep("2021-03-26", 1e3L)),
    inv_start_dt = specimen_coll_dt,
    age_in_years = (specimen_coll_dt - patient_dob) %>%
      lubridate::as.duration() %>%
      magrittr::divide_by_int(lubridate::dyears(1L)) %>%
      as.character() %>%
      vctrs::vec_assign(
        i = sample(seq_along(.), size = 1e3),
        value = NA_character_
      )
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date) & !c("patient_dob", "specimen_coll_dt"),
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_peds <- filter_peds(data) %>%
    dplyr::arrange(
      .data[["patient_dob"]],
      .data[["specimen_coll_dt"]],
      .data[["inv_start_dt"]],
      .data[["age_in_years"]]
    )

  expect_snapshot(tbl_peds)
})
