#' Create Powerpoint for Daily Status Report
#'
#' @param date The date for which to run the report; defaults to most recent
#'
#' @param dir The directory to save the report; set to `NULL` to return without
#'   saving
#'
#' @param inv Investigation data from
#'   \code{\link[coviData:process-nbs]{process_inv()}}
#'
#' @param pcr PCR test data from
#'   \code{\link[coviData:process-nbs]{process_pcr()}}
#'
#' @return An `rpptx` object
#'
#' @export
rpt_daily_pptx <- function(
  date = NULL,
  dir = coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Status Report",
    "automated"
  ),
  inv = process_inv(read_inv(date)),
  pcr = process_pcr(read_pcr(date), inv = inv)
) {

  # Ensure valid date
  date <- coviData::date_inv(date)

  # Load powerpoint template
  pptx <- officer::read_pptx(system.file(
    "extdata", "covid_report_template.pptx",
    package = "covidReport",
    mustWork = TRUE
  ))
  gc(verbose = FALSE)

  # Data
  pos_ppl <- dplyr::select(
    pos(inv),
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
  pcr_cols <- c("inv_local_id", "specimen_coll_dt", "lab_result")
  pcr_subset <- dplyr::mutate(
    pcr,
    data = list_of(dplyr::select(.data[["data"]], {{ pcr_cols }}))
  )

  remove(pcr, inv)
  gc(verbose = FALSE)

  # Cumulative case slide
  case_plt_cumulative <- case_plot_cumulative(pos_ppl, date = date)
  gc(verbose = FALSE)

  # Daily case slide
  case_plt_daily_all <- case_plot_daily_ped_all(pos_ppl, date = date)
  gc(verbose = FALSE)

  # 30-Day case slide
  case_plt_recent <- case_plot_daily_recent(pos_ppl, date = date)
  gc(verbose = FALSE)

  # # Daily ped slide
  # case_plt_daily_ped <- case_plot_daily_ped_only (pos_ppl, date = date)
  # gc(verbose = FALSE)

  # Confirmed/Probable slide
  case_tbl_cp <- case_table_confirmed_probable(pos_ppl, date = date)
  gc(verbose = FALSE)

  # Deaths slide
  death_tbl_total <- death_table_total(pos_ppl, date = date)
  gc(verbose = FALSE)
  death_tbl_age <- death_table_age_summary(pos_ppl, date = date)
  gc(verbose = FALSE)

  # Active slide
  case_tbl_active <- case_table_active(pos_ppl, date = date)
  gc(verbose = FALSE)

  # Test table slide
  test_tbl_total <- test_table_total(pcr_subset, date = date)
  gc(verbose = FALSE)

  # Test plot slide
  test_plt_total <- test_plot_total(pcr_subset, date = date)
  gc(verbose = FALSE)

  # Positivity slide
  test_plt_pos <- test_plot_positivity(pcr_subset, date = date)
  gc(verbose = FALSE)

  # Investigations slide
  # inv_tbl_total <- inv_table_total(pos_ppl, date = date)
  # gc(verbose = FALSE)

  remove(pos_ppl, pcr_subset)
  gc(verbose = FALSE)

  # Report variables
  master <- "HD Blue and White"
  date_ppt <- format(date, "%B %d, %Y")

  # Create title slide
  title <- "COVID-19 Daily Status Report"
  pptx <- pptx %>%
    officer::add_slide("Title Slide", master) %>%
    officer::ph_with(
      value = title,
      location = officer::ph_location_type("ctrTitle")
    ) %>%
    officer::ph_with(
      value = date_ppt,
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
      value = case_plt_daily_all,
      location = officer::ph_location_type("pic")
    )

  # Create 30-day slide
  pptx <- pptx %>%
    officer::add_slide("Picture only", master) %>%
    officer::ph_with(
      value = case_plt_recent,
      location = officer::ph_location_type("pic")
    )

  # # Create daily ped slide
  # pptx <- pptx %>%
  #   officer::add_slide("Picture only", master) %>%
  #   officer::ph_with(
  #     value = case_plt_daily_ped,
  #     location = officer::ph_location_type("pic")
  #   )

  # Create confirmed/probable slide
  cp_title <- "COVID-19 Cases and Deaths by Status"
  pptx <- pptx %>%
    officer::add_slide("Table", master) %>%
    officer::ph_with(
      value = cp_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = case_tbl_cp,
      location = ph_location_table(
        case_tbl_cp,
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
      value = date_ppt,
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

  # # Create active slide
  # active_title <- "Active COVID-19 Cases"
  # pptx <- pptx %>%
  #   officer::add_slide("Table", master) %>%
  #   officer::ph_with(
  #     value = active_title,
  #     location = officer::ph_location_type("title")
  #   ) %>%
  #   officer::ph_with(
  #     value = date_ppt,
  #     location = officer::ph_location_type("subTitle")
  #   ) %>%
  #   officer::ph_with(
  #     value = case_tbl_active,
  #     location = ph_location_table(
  #       case_tbl_active,
  #       pptx,
  #       layout = "Table",
  #       pos_h = FALSE,
  #       valign = 1
  #     )
  #   )

  # Create test table slide
  test_tbl_title <- "COVID-19 PCR Tests"
  pptx <- pptx %>%
    officer::add_slide("Table", master) %>%
    officer::ph_with(
      value = test_tbl_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
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

  # Create test plot slide
  pptx <- pptx %>%
    officer::add_slide("Picture only", master) %>%
    officer::ph_with(
      value = test_plt_total,
      location = officer::ph_location_type("pic")
    )

  # Create positivity slide
  pptx <- pptx %>%
    officer::add_slide("Picture only", master) %>%
    officer::ph_with(
      value = test_plt_pos,
      location = officer::ph_location_type("pic")
    )

  # Create investigations slide
  # inv_title <- "COVID-19 Case Investigations"
  # pptx <- pptx %>%
  #   officer::add_slide("Table", master) %>%
  #   officer::ph_with(
  #     value = inv_title,
  #     location = officer::ph_location_type("title")
  #   ) %>%
  #   officer::ph_with(
  #     value = date_ppt,
  #     location = officer::ph_location_type("subTitle")
  #   ) %>%
  #   officer::ph_with(
  #     value = inv_tbl_total,
  #     location = ph_location_table(
  #       inv_tbl_total,
  #       pptx,
  #       layout = "Table",
  #       pos_h = FALSE,
  #       valign = 1
  #     )
  #   )

  if (!is.null(dir)) {
    path <- coviData::path_create(
      dir,
      paste0("daily_status_report_", date, ".pptx")
    )
    print(pptx, target = path)
    attr(pptx, "path") <- path
  }

  pptx
}

#' Send Daily COVID-19 Status Summary via Outlook Email
#'
#' @param date The date for which to run the report; defaults to most recent
#'
#' @param to A `character` vector of recipient email addresses
#'
#' @param dir_pptx Directory containing daily status report powerpoint files
#'
#' @param demog Should the demographics report be attached? The default attaches
#'   on Tuesdays only.
#'
#' @param inv Investigation data from
#'   \code{\link[coviData:process-nbs]{process_inv()}}
#'
#' @param pcr PCR test data from
#'   \code{\link[coviData:process-nbs]{process_pcr()}}
#'
#' @export
rpt_daily_mail <- function(
  date = NULL,
  to = c(
    "Liang.Li@shelbycountytn.gov",
    "Allison.Plaxco@shelbycountytn.gov"
  ),
  dir_pptx = coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Status Report",
    "automated"
  ),
  demog = rlang::is_true(weekdays(date_inv(date)) == "Tuesday"),
  inv = process_inv(read_inv(date)),
  pcr = process_pcr(read_pcr(date), inv = inv)
) {

  date <- date_inv(date)

  str_date <- format(date, "%m/%d/%y")

  if (weekdays(date) %in% c("Saturday", "Sunday")) {
    subject <- paste("COVID-19 Numbers for", str_date)
    intro <- paste("Below are the COVID-19 numbers for", str_date)
  } else {
    subject <- paste("COVID-19 Status Report for", str_date)
    intro <- paste("Attached is the COVID-19 status report for", str_date)
  }

  # Data
  inv_cols <- c(
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
  pcr_cols <- c("inv_local_id", "specimen_coll_dt", "lab_result")
  inv_subset <- dplyr::mutate(
    inv,
    data = list_of(dplyr::select(.data[["data"]], {{ inv_cols }}))
  )
  pcr_subset <- dplyr::mutate(
    pcr,
    data = list_of(dplyr::select(.data[["data"]], {{ pcr_cols }}))
  )

  n_ped_total <- NROW(filter_peds(pos(inv)))
  n_ped_active <- NROW(filter_active(filter_peds(pos(inv))))
  n_ped_30 <- NROW(filter_active(filter_peds(pos(inv)), days = 30L))
  inv_yest = process_inv(read_inv(date-1))
  inv_week = process_inv(read_inv(date-14))

#  n_ped_new <- n_ped_total - NROW(filter_peds(pos(inv_yest)))

  n_ped_new <- n_ped_total - NROW(filter_peds(pos(inv_week)))

  remove(pcr, inv, inv_yest)
  gc()

  # People totals
  pos_inv  <- pos(inv_subset)
  n_ppl_pos <- NROW(pos_inv)
  n_ppl_neg <- NROW(neg(inv_subset))
  remove(inv_subset)
  gc()

  ppl_tbl_total <- tibble::tibble(
    result = c("+ People", "- People", "Total People"),
    n = c(n_ppl_pos, n_ppl_neg, n_ppl_pos + n_ppl_neg)
  ) %>%
    gt::gt() %>%
    fmt_covid_table(total = TRUE) %>%
    gt::opt_align_table_header("right") %>%
    gt::cols_align("right") %>%
    gt::fmt_number("n", decimals = 0L) %>%
    gt::as_raw_html()
  gc()

  # Test totals
  test_total_df <- test_calc_total(pcr_subset, date = date)
  rm(pcr_subset)
  gc()

  test_tbl_total <- test_total_df %>%
    dplyr::mutate(result = c("+ Test", "- Test", "Total Tests")) %>%
    dplyr::select(-"percent") %>%
    gt::gt() %>%
    fmt_covid_table(total = TRUE) %>%
    gt::opt_align_table_header("right") %>%
    gt::cols_align("right") %>%
    gt::fmt_number("n", decimals = 0L) %>%
    gt::as_raw_html()
  gc()

  # Confirmed/Probable
  cp_tbl <- case_calc_confirmed_probable(pos_inv, date = date) %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::opt_align_table_header("right") %>%
    gt::cols_align("right") %>%
    gt::fmt_number(c("total", "C", "P"), decimals = 0L) %>%
    gt::as_raw_html()
  gc()

  # Active
  active_tbl <- case_calc_active(pos_inv, date = date) %>%
    dplyr::select(-"percent") %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::opt_align_table_header("right") %>%
    gt::cols_align("right") %>%
    gt::fmt_number("n", decimals = 0L) %>%
    gt::as_raw_html()
  gc()

  # Total deaths
  n_deaths <- NROW(filter_deaths(pos_inv))
  gc()

  remove(pos_inv)
  gc()

  # Vaccination tables
  vac_date <- lubridate::Date()
  i <- 0L
  while(vec_is_empty(vac_date)) {
    vac_date <- date_vac(date - i)
    i <- i + 1L
  }
  if (vec_is_empty(date_vac(date))) {
    vac_msg <- paste0(
      "<br>",
      "Vaccination data has not yet been updated today; ",
      "the most recent data (", format(vac_date, "%m/%d/%Y"), ") ",
      "is shown below. If vaccination data is posted, ",
      "please update these numbers before sending."
    )
  } else {
    vac_msg <- ""
  }

  #Read vac tables
  vac_recent <- gt::as_raw_html(vac_table_recent_email())
  gc()
  vac_ppl <- gt::as_raw_html(vac_table_totals_email())
  gc()


  # Vaccination numbers
  n_ppl_vac <- nrow(vac_prep(distinct = TRUE))
  n_pct_vac <- round(100*n_ppl_vac/937166, digits = 1)
  n_pct_vac_goal <- round(100*n_ppl_vac/700000, digits = 1)
  n_avg_vac <- vac_recent %>%
    as_tbl() %>%
    dplyr::mutate(
      dplyr::across(.fns = ~ as.integer(stringr::str_remove_all(.x, "[^0-9]")))
    ) %>%
    dplyr::pull(2L) %>%
    divide_by(7) %>%
    round()
  n_avg_case <- sort(date - 0:7) %>%
    purrr::map_int(~ NROW(coviData::read_inv_id(.x))) %>%
    diff() %>%
    mean() %>%
    round()


  # Email body numbers
  str_test_total <- test_total_df %>%
    dplyr::filter(tolower(.data[["result"]]) == "total") %>%
    dplyr::pull("n") %>%
    format(big.mark = ",")
  str_ppl_pos <- format(n_ppl_pos, big.mark = ",")
  str_ppl_new <- n_ppl_pos %>%
    subtract(NROW(coviData::read_inv_id(date = date - 1L))) %>%
    format(big.mark = ",")
  gc()
  str_deaths <- format(n_deaths, big.mark = ",")

  str_pct_vac <- paste0(n_pct_vac, "%")
  str_pct_vac_goal <- paste0(n_pct_vac_goal, "%")
  str_ppl_vac <- format(n_ppl_vac, big.mark = ",")
  str_avg_vac <- format(n_avg_vac, big.mark = ",")
  str_avg_case <- format(n_avg_case, big.mark = ",")
  str_ped_30 <- format(n_ped_30, big.mark = ",")
  str_ped_active <- format(n_ped_active, big.mark = ",")
  str_ped_new <- format(n_ped_new, big.mark = ",")
  str_ped_total <- format(n_ped_total, big.mark = ",")



  body <- paste0(
    intro,
    vac_msg,
    "<br><br>",
    "Total Tests: ", str_test_total, "<br>",
    "Total Cases: ", str_ppl_pos, "<br>",
    "New Cases: ", str_ppl_new, "<br>",
    "Total Deaths: ", str_deaths,
    "<br><br>",
    #"Cumulative Pediatric Cases: ", str_ped_total, "<br>",
    "Pediatric Cases in the Last 30 Days: ", str_ped_30, "<br>",
    "14-Day Pediatric Cases: ", str_ped_active, "<br>",
   # "New Pediatric Cases: ", str_ped_new,
    "<br><br>",
    "% Vaccinated of Goal: ", str_pct_vac_goal, "<br>",
    "% Vaccinated of Population: ", str_pct_vac, "<br>",
    "Total People Vaccinated: ", str_ppl_vac, "<br>",
    "Vaccinations per day (7-day average): ", str_avg_vac, "<br>",
    "Reported cases per day (7-day average): ", str_avg_case,
    "<br><br>",
    vac_recent, "<br>",
    vac_ppl,
    "<br><br>",
    "Thanks!",
    "<br><br>",
    "<h3>Supplementary Numbers: Delete Before Sending</h3>", "<br>",
    test_tbl_total, "<br>",
    ppl_tbl_total, "<br>",
    cp_tbl, "<br>",
    active_tbl,
    "<br><br>",
    "<i>Note: This email was generated automatically</i>"
  )

  if (weekdays(lubridate::today()) == "Tuesday") {
    body <- paste0(
      intro,
      vac_msg,
      "<br><br>",
      "Total Tests: ", str_test_total, "<br>",
      "Total Cases: ", str_ppl_pos, "<br>",
      "New Cases: ", str_ppl_new, "<br>",
      "Total Deaths: ", str_deaths,
      "<br><br>",
      "Cumulative Pediatric Cases: ", str_ped_total, "<br>",
      "Pediatric Cases in the Last 30 Days: ", str_ped_30, "<br>",
      "14-Day Pediatric Cases: ", str_ped_active, "<br>",
      "New Pediatric Cases (Reported in last 14 days): ", str_ped_new,
      "<br><br>",
      "% Vaccinated of Goal: ", str_pct_vac_goal, "<br>",
      "% Vaccinated of Population: ", str_pct_vac, "<br>",
      "Total People Vaccinated: ", str_ppl_vac, "<br>",
      "Vaccinations per day (7-day average): ", str_avg_vac, "<br>",
      "Reported cases per day (7-day average): ", str_avg_case,
      "<br><br>",
      vac_recent, "<br>",
      vac_ppl,
      "<br><br>",
      "Thanks!",
      "<br><br>",
      "<h3>Supplementary Numbers: Delete Before Sending</h3>", "<br>",
      test_tbl_total, "<br>",
      ppl_tbl_total, "<br>",
      cp_tbl, "<br>",
      active_tbl,
      "<br><br>",
      "<i>Note: This email was generated automatically</i>"
    )
  }

  # Get powerpoints if available
  ppt_path <- fs::dir_ls(
    dir_pptx,
    type = "file",
    regexp = paste0("daily_status_report_", date, ".pptx", collapse = "")
  )

  if (rlang::is_true(demog)) {
    demog_path <- fs::dir_ls(
      dir_pptx,
      type = "file",
      regexp = paste0("demographic_status_report_",date,".pptx", collapse = "")
    )
    ppt_path <- c(ppt_path, demog_path)
  }

  coviData::notify(
    to = to,
    subject = subject,
    body = body,
    html = TRUE,
    attach = ppt_path
  )
}
