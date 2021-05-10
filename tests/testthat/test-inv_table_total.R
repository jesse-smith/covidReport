test_that("`inv_table_total()` info matches reference", {
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
    inv_table_total,
    "coviData::path_inv",
    fs::path(
      "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
      "Sandbox data pull Final/2021-04-19 Final Data Pull.csv"
    )
  )

  mockery::stub(
    inv_table_total,
    "redcap_contacts",
    redcap_data
  )

  src <- "Data Source: Shelby County Health Department"

  tbl_ref <- tibble::tribble(
                         ~ `Number of...`,       ~ N,
                  "Opened Investigations", "100,000",
                  "Closed Investigations",  "90,000",
            "Contacts Identified to Date",  "48,672",
    "Contacts Identified in Last 14 Days",     "987",
                                      src,       src
  )

  tbl_inv <- suppressWarnings(
    inv_table_total(data, date = "2021-04-19") %>%
      gt::as_raw_html() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble()
  )

  expect_equal(tbl_inv, tbl_ref)
})

test_that("`inv_table_total()` html matches snapshot", {
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
    inv_table_total,
    "coviData::path_inv",
    fs::path(
      "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
      "Sandbox data pull Final/2021-04-19 Final Data Pull.csv"
    )
  )

  mockery::stub(
    inv_table_total,
    "redcap_contacts",
    redcap_data
  )

  tbl_html <- suppressWarnings(
    inv_table_total(data, date = "2021-04-19") %>%
      gt::as_raw_html() %>%
      xml2::read_html() %>%
      rvest::html_node("table")
  )

  expect_snapshot(tbl_html)
})

test_that("`redcap_contacts()` returns expected output", {
  data <- redcap_contacts(date = as.Date("2021-04-19"))

  ref_dates <- seq(as.Date("2021-02-02"), as.Date("2021-04-19"), by = 1L)
  expect_s3_class(data, "tbl_df")
  expect_equal(data[["date"]], ref_dates)
  expect_vector(data[["n"]], integer())
})
