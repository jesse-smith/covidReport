# Load and process data from 4/19/21 and extract investigation local ID
test_cumulative_data <- dplyr::transmute(
  coviData::process_positive_people(date = "2021-04-19"),
  inv_local_id = as.character(digest::digest2int(inv_local_id))
)

# Get report dates for data in `d`
test_report_data <- dplyr::mutate(
  dplyr::as_tibble(coviData::load_report_date()),
  inv_local_id = as.character(digest::digest2int(inv_local_id))
)

saveRDS(
  test_cumulative_data,
  file = testthat::test_path("../data/test_cumulative_data.rds")
)
saveRDS(
  test_report_data,
  file = testthat::test_path("../data/test_report_data.rds")
)