vac_back_generate <- function(
  data = read_vac(date = date),
  date = NULL,
  days_back = 2
){

  date <- date_vac(date)

  date2 <- date - days_back

  data2 <- subset(data, lubridate::mdy(insert_date) <= date2)

  d_text <-  stringr::str_replace_all(date2, "[^[:alnum:]]", "")

  data.table::fwrite(data2,
                     paste0("V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/COVID-19 Vaccine Reporting/data/COVID-19 Vaccine data pull/COVID_VACC_MSR_", d_text, ".csv"),
                     sep=",", na="", compress = "none",scipen=999, eol="\n")


  rpt_vac_pptx(date = date2)



  v_data <- vac_prep(read_vac(date = date2), distinct = TRUE)



  # Create vaccination goal plot
  v_goal_file_stem <- paste0("vaccination_goal_", date2)
  v_goal_path <- coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccination_goal/",
    v_goal_file_stem,
    ext = "png"
  )
  p_goal <- vac_plot_goal(data = v_data, date = date2)
  if (rlang::is_interactive()) show(p_goal)
  save_plot(p_goal, path = v_goal_path, width = 12, height = 9)














  # Create vaccination age group plot
  v_age_pop_file_stem <- paste0("vaccination_age_pop_", date2)
  v_age_pop_path <- coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccination_age_pop/",
    v_age_pop_file_stem,
    ext = "png"
  )
  p_age_pop <- vac_plot_age(data = v_data, date = date2)
  if (rlang::is_interactive()) show(p_age_pop)
  covidReport::save_plot(p_age_pop, path = v_age_pop_path)



  # creating vaccination percent zip map
  v_map_pct_file_stem <- paste0("vaccination_zip_pct_", date2)
  v_map_pct_path <- coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccination_map_pct/",
    v_map_pct_file_stem,
    ext = "png"
  )
  p_map_pct <- covidReport::vac_map_pct(data = v_data, date = date2)
  if (rlang::is_interactive()) show(p_map_pct)
  coviData::save_plot(p_map_pct, path = v_map_pct_path, ratio = c(12, 9), size = 1.125)


  #creating full vaccination percent zip map
  v_map_pct_file_stem <- paste0("full_vaccination_zip_pct_", date2)
  v_map_pct_path <- coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccination_map_pct/",
    v_map_pct_file_stem,
    ext = "png"
  )
  p_map_pct_f <- covidReport:::vac_fully_map_pct(data = v_data, date = date2)
  if (rlang::is_interactive()) show(p_map_pct_f)
  coviData::save_plot(p_map_pct_f, path = v_map_pct_path, ratio = c(12, 9), size = 1.125)


  #creating bivalent vaccination percent zip map
  v_map_pct_file_stem <- paste0("bivalent_vaccination_zip_pct_", date2)
  v_map_pct_path <- coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccination_map_pct/",
    v_map_pct_file_stem,
    ext = "png"
  )
  p_map_pct_b <- covidReport:::vac_bivalent_map_pct(data = v_data, date = date2)
  if (rlang::is_interactive()) show(p_map_pct_b)
  coviData::save_plot(p_map_pct_b, path = v_map_pct_path, ratio = c(12, 9), size = 1.125)


  #creating vaccination percent zip map with grant zips
  v_map_pct_file_stem <- paste0("grant_vaccination_zip_pct_", date2)
  v_map_pct_path <- coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccination_map_pct/",
    v_map_pct_file_stem,
    ext = "png"
  )
  p_map_pct_grant <- covidReport:::grant_zip_vac_map_pct(data = v_data, date = date2)
  if (rlang::is_interactive()) show(p_map_pct_grant)
  coviData::save_plot(p_map_pct_grant, path = v_map_pct_path, ratio = c(12, 9), size = 1.125)


  #creating full vaccination percent zip map with grant zips
  v_map_pct_file_stem <- paste0("grant_full_vaccination_zip_pct_", date2)
  v_map_pct_path <- coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccination_map_pct/",
    v_map_pct_file_stem,
    ext = "png"
  )
  p_map_pct_grant_f <- covidReport:::grant_zip_vac_fully_map_pct(data = v_data, date = date2)
  if (rlang::is_interactive()) show(p_map_pct_grant_f)
  coviData::save_plot(p_map_pct_grant_f, path = v_map_pct_path, ratio = c(12, 9), size = 1.125)




  #creating bivalent vaccination percent zip map with grant zips
  v_map_pct_file_stem <- paste0("grant_bivalent_vaccination_zip_pct_", date2)
  v_map_pct_path <- coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccination_map_pct/",
    v_map_pct_file_stem,
    ext = "png"
  )
  p_map_pct_grant_b <- covidReport:::grant_zip_vac_bivalent_map_pct(data = v_data, date = date2)
  if (rlang::is_interactive()) show(p_map_pct_grant_b)
  coviData::save_plot(p_map_pct_grant_b, path = v_map_pct_path, ratio = c(12, 9), size = 1.125)



  }
