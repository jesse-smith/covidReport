test_that("`zip_map_pct()` matches doppelganger", {

  zips <- withr::with_seed(
    200L,
    sample(coviData::shelby_zip, size = 5e5L, replace = TRUE)
  )

  data <- tibble::tibble(
    asiis_pat_id_ptr = 1:5e5,
    resident = c(rep(TRUE, 5*9e4), rep(FALSE, 5e4)),
    dose_count = c(rep(1, 5e4), rep(2, 5*9e4)),
    address_zip = {{ zips }}
  ) %>% dplyr::slice_sample(prop = 1)

  plt <- vac_map_pct(data, date = "2021-04-01")

  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "percent zip vaccinated",
      fig = plt,
      path = "vac-map-pct"
    )
  )
})
