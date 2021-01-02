# Load yesterday's cumulative case data
inv_data <- coviData::process_positive_people(Sys.Date() - 1)

# Combine with the current report date data
data <- dplyr::semi_join(
  coviData::load_report_date() %>% dplyr::as_tibble(),
  inv_data,
  by = "inv_local_id"
)

plot_epicurve(data, today = Sys.Date() - 1L, iterations = 100)
