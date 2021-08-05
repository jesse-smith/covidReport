test_that("`test_plot_total()` matches doppelganger", {
  mockery::stub(
    test_plot_total,
    "date_pcr",
    lubridate::as_date
  )

  data <- readRDS(test_path("../data/test_positivity_data.rds")) %>%
    dplyr::mutate(positive = .data[["inv_case_status"]] %in% c("C", "P")) %>%
    dplyr::nest_by(.data[["positive"]]) %>%
    dplyr::arrange(dplyr::desc(.data[["positive"]])) %>%
    set_attr("date", as.Date("2021-04-19"))

  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "test total",
      fig = test_plot_total(data, date = "2021-04-19"),
      path = "test-plot-total"
    )
  )
})

test_that("`prep_test_total()` output is correct", {
  data <- tibble::tibble(
    specimen_coll_dt = rep(
      seq(as.Date("2020-03-01"), as.Date("2021-04-20"), by = 1L),
      times = 200L
    ) %>%
      append(rep(NA_character_, 800L)) %>%
      format("%Y-%m-%dTH:M:SZ")
  ) %>%
    dplyr::mutate(positive = TRUE) %>%
    dplyr::slice_sample(prop = 1) %>%
    dplyr::nest_by(.data[["positive"]]) %>%
    vec_rep(2L) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(positive = c(TRUE, FALSE)) %>%
    dplyr::rowwise(.data[["positive"]])

  test_tot <- prep_test_total(data, date = as.Date("2021-04-19"), delay = 5L)

  expect_s3_class(test_tot, "n_tbl")
  expect_vector(test_tot[["test_date"]], lubridate::Date())
  expect_vector(test_tot[["n"]], integer())
  expect_vector(test_tot[["avg"]], double())
  expect_equal(attr(test_tot, "n_obs", exact = TRUE), 2L*84000L)
  expect_equal(attr(test_tot, "n_missing", exact = TRUE), 3600L)
  expect_equal(as.character(min(test_tot[["test_date"]])), "2020-03-14")
  expect_equal(as.character(max(test_tot[["test_date"]])), "2021-04-14")
})
