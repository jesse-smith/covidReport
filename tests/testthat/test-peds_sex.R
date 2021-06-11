test_that("`peds_plot_sex()` matches doppelganger", {
  tbl_mock <- mockery::mock(tibble::tribble(
        ~ grp, ~ n, ~ percent, ~ rate,
     "Female", 500,       0.5,    0.1,
       "Male", 400,       0.4,    0.1,
    "Missing", 100,       0.1,    0.1,
  ))

  mockery::stub(
    peds_plot_sex,
    "peds_calc_sex",
    tbl_mock
  )

  m_dt <- mockery::mock(as.Date("2021-04-01"), cycle = TRUE)
  mockery::stub(
    peds_plot_sex,
    "date_inv",
    m_dt
  )

  plt <- peds_plot_sex(tibble::tibble(), date = "2021-04-01")

  mockery::expect_called(m_dt, n = 1L)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "peds case rate by sex",
      fig = plt,
      path = "peds-plot-sex"
    )
  )
})

test_that("`void(peds_table_sex())` matches snapshot", {
  tbl_mock <- mockery::mock(tibble::tribble(
        ~ grp, ~ n, ~ percent, ~ rate,
     "Female", 500,       0.5,    0.1,
       "Male", 400,       0.4,    0.1,
    "Missing", 100,       0.1,    0.1,
  ))

  mockery::stub(
    peds_table_sex,
    "peds_calc_sex",
    tbl_mock
  )

  data <- tibble::tibble()

  tbl_void <- flextable::void(
    peds_table_sex(data, date = "2021-04-01"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`peds_calc_sex()` matches snapshot", {

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
      ),
    patient_current_sex = withr::with_seed(
      200L,
      sample(
        c("F", "M", "U", "B", NA_character_),
        size = NROW(inv_start_dt),
        replace = TRUE,
        prob = c(0.5, 0.49, 0.001, 0.001, 0.008)
      )
    )
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date) & !c("patient_dob", "specimen_coll_dt"),
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_peds <- peds_calc_sex(data, date = "2021-04-01")

  expect_snapshot(tbl_peds)
})
