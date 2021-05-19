test_that("`void(inv_table_total())` matches snapshot", {
  data <- tibble::tibble(
    investigation_status_cd = sample(c(
      rep("C", 9e4L),
      rep("O", 9e3L),
      rep(NA_character_, 1e3L))
    )
  )

  src <- "Data Source: Shelby County Health Department"

  tbl_info <- tibble::tribble(
    ~ measure,       ~ n,
    "Opened Investigations", "100,000",
    "Closed Investigations",  "90,000",
    "Contacts Identified to Date",  "48,672",
    "Contacts Identified in Last 14 Days",     "987",
    src,       src
  )

  mockery::stub(
    inv_table_total,
    "inv_calc_total",
    tbl_info
  )

  tbl_void <- flextable::void(
    inv_table_total(data, date = "2021-04-19"),
    part = "all"
  )

  expect_snapshot(tbl_void)
})

test_that("`inv_calc_total()` matches reference", {
  data <- tibble::tibble(
    investigation_status_cd = sample(c(
      rep("C", 9e4L),
      rep("O", 9e3L),
      rep(NA_character_, 1e3L))
    )
  )

  redcap_data <- tibble::tibble(
    date = seq(as.Date("2021-02-02"), as.Date("2021-04-19"), by = 1L),
    n = seq_along(date)
  )

  mockery::stub(
    inv_calc_total,
    "coviData::path_inv",
    fs::path(
      "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
      "Sandbox data pull Final/2021-04-19 Final Data Pull.csv"
    )
  )

  mockery::stub(
    inv_calc_total,
    "redcap_contacts",
    redcap_data
  )

  tbl_ref <- tibble::tribble(
           ~ measure,     ~ n,
        "inv_opened", 100000L,
        "inv_closed",  90000L,
    "contacts_total",  48672L,
       "contacts_14",    987L
  )

  expect_equal(inv_calc_total(data, date = "2021-04-19"), tbl_ref)
})

test_that("`redcap_contacts()` returns expected output", {
  skip_if_offline()
  skip_on_covr()
  data <- redcap_contacts(date = as.Date("2021-04-19"))
  ref_dates <- seq(as.Date("2021-02-02"), as.Date("2021-04-19"), by = 1L)
  expect_s3_class(data, "tbl_df")
  expect_equal(data[["date"]], ref_dates)
  expect_vector(data[["n"]], integer())
})
