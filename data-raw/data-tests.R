# Load and process data from 4/19/21 and extract investigation local ID
test_cumulative_data <- coviData::read_inv(date = "2021-04-19") %>%
  coviData::process_inv() %>%
  coviData::pos() %>%
  dplyr::transmute(
    inv_local_id = as.character(digest::digest2int(inv_local_id))
  )

# Get report dates for data in `test_cumulative_data`
test_report_data <- dplyr::mutate(
  dplyr::as_tibble(coviData::load_report_date()),
  inv_local_id = as.character(digest::digest2int(inv_local_id))
)

# Tests
pos_test <- coviData::process_positive_tests(date = "2021-04-19")
neg_test <- coviData::process_negative_tests(date = "2021-04-19")

test_positivity_data <- dplyr::bind_rows(
  dplyr::select(pos_test, inv_case_status, specimen_coll_dt),
  dplyr::select(neg_test, inv_case_status, specimen_coll_dt)
) %>% dplyr::slice_sample(prop = 1)

saveRDS(
  test_cumulative_data,
  file = testthat::test_path("../data/test_cumulative_data.rds")
)
saveRDS(
  test_report_data,
  file = testthat::test_path("../data/test_report_data.rds")
)

saveRDS(
  test_positivity_data,
  file = testthat::test_path("../data/test_positivity_data.rds")
)
