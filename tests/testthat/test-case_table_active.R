test_that("`case_table_active()` works with active cases", {

  path_fn <- function(
    date = NULL,
    dir = fs::path(
      "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA",
      "Sandbox data pull Final"
    ),
    ext = c("csv", "xlsx", "fst")
  ) {
    ext <- rlang::arg_match(ext)[[1L]]
    if (is.null(date)) date <- as.character(lubridate::today()-1L)
    date <- as.character(lubridate::as_date(date))

    fs::path(dir, paste0(date, " Final Data Pull.", ext))
  }

  mockery::stub(
    case_table_active,
    "coviData::path_inv",
    path_fn
  )

  data <- tibble::tibble(
    die_from_illness_ind = rep(
      c("Y", rep("N", 3L), rep("U", 3L), rep(NA, 3L)),
      1e4L
    ),
    inv_death_dt = seq(
      as.Date("2020-03-26"),
      as.Date("2021-03-26"),
      length.out = 99999L
    ) %>% purrr::prepend(lubridate::NA_Date_),
    illness_onset_dt = c(rep(as.Date("2020-03-10"), 99e3L), rep("2021-03-26", 1e3L)),
    specimen_coll_dt = illness_onset_dt,
    inv_start_dt = specimen_coll_dt
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date),
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_active <- suppressWarnings(
    case_table_active(data, date = "2021-04-01") %>%
      gt::as_raw_html() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble()
  )

  tbl_ref <- tibble::tribble(
                ~ Status,       ~ N,           ~ `%`,
                "Active",     "900",   "0.9&percnt;",
              "Deceased",   "9,999",  "10.0&percnt;",
    "Inactive/Recovered",  "89,101",  "89.1&percnt;",
                 "Total", "100,000", "100.0&percnt;"
  )

  expect_equal(tbl_active, tbl_ref)
})

test_that("`case_table_active()` works with no active cases", {

  path_fn <- function(
    date = NULL,
    dir = fs::path(
      "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA",
      "Sandbox data pull Final"
    ),
    ext = c("csv", "xlsx", "fst")
  ) {
    ext <- rlang::arg_match(ext)[[1L]]
    if (is.null(date)) date <- as.character(lubridate::today()-1L)
    date <- as.character(lubridate::as_date(date))

    fs::path(dir, paste0(date, " Final Data Pull.", ext))
  }

  mockery::stub(
    case_table_active,
    "coviData::path_inv",
    path_fn
  )

  data <- tibble::tibble(
    die_from_illness_ind = rep(
      c("Y", rep("N", 3L), rep("U", 3L), rep(NA, 3L)),
      1e4L
    ),
    inv_death_dt = seq(
      as.Date("2020-03-26"),
      as.Date("2021-03-26"),
      length.out = 99999L
    ) %>% purrr::prepend(lubridate::NA_Date_),
    illness_onset_dt = vctrs::vec_assign(
      rep(as.Date("2020-04-01"), times = 1e5L),
      i = seq(1, 1e5, by = 1e3),
      lubridate::NA_Date_
    ),
    specimen_coll_dt = vctrs::vec_assign(
      rep(as.Date("2020-04-10"), times = 1e5L),
      i = seq(7, 9e4, by = 1234),
      lubridate::NA_Date_
    ),
    inv_start_dt = dplyr::coalesce(
      specimen_coll_dt,
      rep(as.Date("2020-04-15"), times = 1e5L)
    )
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(lubridate::is.Date),
        ~ format(.x, "%Y-%m-%dT%H:%M:%SZ")
      )
    ) %>%
    dplyr::slice_sample(prop = 1)

  tbl_active <- suppressWarnings(
    case_table_active(data, date = "2021-04-01") %>%
      gt::as_raw_html() %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble()
  )

  tbl_ref <- tibble::tribble(
                ~ Status,       ~ N,           ~ `%`,
                "Active",       "0",   "0.0&percnt;",
              "Deceased",   "9,999",  "10.0&percnt;",
    "Inactive/Recovered",  "90,001",  "90.0&percnt;",
                 "Total", "100,000", "100.0&percnt;"
  )

  expect_equal(tbl_active, tbl_ref)
})
