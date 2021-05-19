# Data
pos_ppl <- dplyr::select(
  coviData::process_positive_people(),
  "inv_local_id",
  "inv_case_status",
  "specimen_coll_dt",
  "patient_dob",
  "die_from_illness_ind",
  "illness_onset_dt",
  "inv_start_dt",
  "inv_death_dt",
  "age_in_years",
  "investigation_status_cd"
)
gc()
pcr <- dplyr::select(
  janitor::clean_names(coviData::read_file_delim(coviData::path_pcr())),
  "inv_local_id",
  "specimen_coll_dt",
  "lab_result"
)
gc()

# Cumulative case slide
case_plt_cumulative <- case_plot_cumulative(pos_ppl)
gc()

# Daily case slide
case_plt_daily <- case_plot_daily(pos_ppl)
gc()

# Confirmed/Probable slide
case_tbl_confirmed_probable <- case_table_confirmed_probable(pos_ppl)
gc()

# Deaths slide
death_tbl_total <- death_table_total(pos_ppl)
gc()
death_tbl_age <- death_table_age(pos_ppl)
gc()

# Active slide
case_tbl_active <- case_table_active(pos_ppl)
gc()

# Test slide
test_tbl_total <- test_table_total(pcr)
gc()

# Positivity slide
test_plt_pos <- test_plot_positivity(pcr)
gc()

# Investigations slide
inv_tbl_total <- inv_table_total(pos_ppl)
gc()

remove(pos_ppl, pcr)
gc()

# Initialize Powerpoint
master <- "HD Blue and White"
date   <- coviData::path_inv() %>%
  fs::path_file() %>%
  fs::path_ext_remove() %>%
  stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
  lubridate::as_date() %>%
  format("%B %d, %Y")
pptx <- officer::read_pptx("dev/covid_report_template.pptx")

# Create title slide
title <- "COVID-19 Daily Status Report"
pptx <- pptx %>%
  officer::add_slide("Title Slide", master) %>%
  officer::ph_with(
    value = title,
    location = officer::ph_location_type("ctrTitle")
  ) %>%
  officer::ph_with(
    value = date,
    location = officer::ph_location_type("subTitle")
  )

# Create cumulative slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = case_plt_cumulative,
    location = officer::ph_location_type("pic")
  )

# Create daily slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = case_plt_daily,
    location = officer::ph_location_type("pic")
  )

# Create confirmed/probable slide
cp_title <- "COVID-19 Cases and Deaths by Status"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = cp_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = case_tbl_confirmed_probable,
    location = ph_location_table(
      case_tbl_confirmed_probable,
      pptx,
      layout = "Table",
      valign = 1
    )
  )

# Create deaths slide
deaths_title <- "COVID-19 Deaths"
pptx <- pptx %>%
  officer::add_slide("Two Table Vertical", master) %>%
  officer::ph_with(
    value = deaths_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = death_tbl_total,
    location = ph_location_table(
      death_tbl_total,
      pptx,
      layout = "Two Table Vertical",
      pos_h = FALSE,
      valign = 1
    )
  ) %>%
  officer::ph_with(
    value = death_tbl_age,
    location = ph_location_table(
      death_tbl_age,
      pptx,
      layout = "Two Table Vertical",
      pos_h = FALSE,
      pos_first = FALSE,
      valign = 1
    )
  )

# Create active slide
active_title <- "Active COVID-19 Cases"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = active_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = case_tbl_active,
    location = ph_location_table(
      case_tbl_active,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )

# Create test slide
test_title <- "COVID-19 PCR Tests"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = test_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = test_tbl_total,
    location = ph_location_table(
      test_tbl_total,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )

# Create positivity slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = test_plt_pos,
    location = officer::ph_location_type("pic")
  )

# Create investigations slide
inv_title <- "COVID-19 Case Investigations"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = inv_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = inv_tbl_total,
    location = ph_location_table(
      inv_tbl_total,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )

print(pptx, target = "dev/daily_status_report.pptx")
