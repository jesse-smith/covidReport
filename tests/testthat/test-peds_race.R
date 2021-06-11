test_that("`peds_plot_race()` matches doppelganger", {
  tbl_mock <- tibble::tribble(
                       ~ grp, ~ n, ~ percent, ~ rate,
    "Black/African American", 500,       0.5,    0.1,
                     "White", 300,       0.3,    0.1,
                     "Other", 100,       0.1,    0.1,
                   "Missing", 100,       0.1,    0.1,
  ) %>%
    dplyr::mutate(grp = forcats::as_factor(grp)) %>%
    mockery::mock()

  mockery::stub(
    peds_plot_race,
    "peds_calc_race",
    tbl_mock
  )

  m_dt <- mockery::mock(as.Date("2021-04-01"), cycle = TRUE)
  mockery::stub(
    peds_plot_race,
    "date_inv",
    m_dt
  )

  plt <- peds_plot_race(tibble::tibble(), date = "2021-04-01")

  mockery::expect_called(m_dt, n = 1L)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "peds case rate by race",
      fig = plt,
      path = "peds-plot-race"
    )
  )
})

test_that("`void(peds_table_race())` matches snapshot", {
  tbl_mock <- mockery::mock(tibble::tribble(
                       ~ grp, ~ n, ~ percent, ~ rate,
    "Black/African American", 500,       0.5,    0.1,
                     "White", 300,       0.3,    0.1,
                     "Other", 100,       0.1,    0.1,
                   "Missing", 100,       0.1,    0.1,
  ))

  mockery::stub(
    peds_table_race,
    "peds_calc_race",
    tbl_mock
  )

  data <- tibble::tibble()

  tbl_void <- flextable::void(
    peds_table_race(data, date = "2021-04-01"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`peds_calc_race()` matches snapshot", {

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
    patient_race_calc = withr::with_seed(
      200L,
      sample(
        c("Black", "White", "Asian", "Native", "None", NA_character_),
        size = NROW(inv_start_dt),
        replace = TRUE,
        prob = c(0.6, 0.3, 0.03, 0.02, 0.01, 0.04)
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

  tbl_peds <- peds_calc_race(data, date = "2021-04-01")

  expect_snapshot(tbl_peds)
})
