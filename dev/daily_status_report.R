# Data
pos_ppl <- coviData::process_positive_people()
gc()
pcr <- coviData::read_file_delim(coviData::path_pcr())
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

# Initialize Powerpoint
master <- "HD Blue and White"
pptx <- officer::read_pptx("dev/covid_report_template.pptx")

# Create title slide
title <- "COVID-19 Daily Status Report"
pptx <- officer::add_slide(pptx, "Title Slide", master)
pptx <- officer::ph_with(
  pptx,
  value = title,
  location = officer::ph_location_type("ctrTitle")
)
pptx <- officer::ph_with(
  pptx,
  value = format(Sys.Date(), "%B %d, %Y"),
  location = officer::ph_location_type("subTitle")
)

# Create cumulative slide
pptx <- officer::add_slide(pptx, "Picture only", master)
pptx <- officer::ph_with(
  pptx,
  value = case_plt_cumulative,
  location = officer::ph_location_type("pic")
)

# Create daily slide
pptx <- officer::add_slide(pptx, "Picture only", master)
pptx <- officer::ph_with(
  pptx,
  value = case_plt_daily,
  location = officer::ph_location_type("pic")
)

# Create confirmed/probable slide
pptx <- officer::add_slide(pptx, "Title and Content", master)
pptx <- officer::ph_with(
  pptx,
  value = "Confirmed and Probable Cases/Deaths",
  location = officer::ph_location_type("title")
)
pptx <- officer::ph_with(
  pptx,
  value = case_tbl_confirmed_probable,
  location = officer::ph_location_type("body")
)

print(pptx, target = "dev/officer_report.pptx")
