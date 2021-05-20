#' Create Powerpoint for Daily Status Report
#'
#' @param date The date for which to run the report; defaults to most recent
#'
#' @param dir The directory to save the report; set to `NULL` to return without
#'   saving
#'
#' @return An `rpptx` object
#'
#' @export
rpt_daily_pptx <- function(
  date = NULL,
  dir = coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Status Report",
    "automated"
  )
) {

  # Ensure valid date
  if (vec_is_empty(date)) {
    date <- coviData::path_inv() %>%
      fs::path_file() %>%
      fs::path_ext_remove() %>%
      stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
      lubridate::as_date()
  }
  date <- lubridate::as_date(date)

  # Load powerpoint template
  pptx <- officer::read_pptx(system.file(
    "extdata", "covid_report_template.pptx",
    package = "covidReport",
    mustWork = TRUE
  ))
  gc()

  # Data
  pos_ppl <- dplyr::select(
    coviData::process_positive_people(date = date),
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
    janitor::clean_names(coviData::read_file_delim(coviData::path_pcr(date))),
    "inv_local_id",
    "specimen_coll_dt",
    "lab_result"
  )
  gc()

  # Cumulative case slide
  case_plt_cumulative <- case_plot_cumulative(pos_ppl, date = date)
  gc()

  # Daily case slide
  case_plt_daily <- case_plot_daily(pos_ppl, date = date)
  gc()

  # Confirmed/Probable slide
  case_tbl_cp <- case_table_confirmed_probable(pos_ppl, date = date)
  gc()

  # Deaths slide
  death_tbl_total <- death_table_total(pos_ppl, date = date)
  gc()
  death_tbl_age <- death_table_age(pos_ppl, date = date)
  gc()

  # Active slide
  case_tbl_active <- case_table_active(pos_ppl, date = date)
  gc()

  # Test slide
  test_tbl_total <- test_table_total(pcr, date = date)
  gc()

  # Positivity slide
  test_plt_pos <- test_plot_positivity(pcr, date = date)
  gc()

  # Investigations slide
  inv_tbl_total <- inv_table_total(pos_ppl, date = date)
  gc()

  remove(pos_ppl, pcr)
  gc()

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

  # Create active slide
  active_title <- "Active COVID-19 Cases"
  pptx <- pptx %>%
    officer::add_slide("Table", master) %>%
    officer::ph_with(
      value = active_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
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
      value = date_ppt,
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
#' @export
rpt_daily_mail <- function(
  date = NULL,
  to = c(
    "Jesse.Smith@shelbycountytn.gov",
    "Chaitra.Subramanya@shelbycountytn.gov",
    "Allison.Plaxco@shelbycountytn.gov"
  ),
  dir_pptx = coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Status Report",
    "automated"
  )
) {

  date <- coviData::path_inv(date) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
    lubridate::as_date()

  str_date <- format(date, "%m/%d/%y")

  if (weekdays(date) %in% c("Saturday", "Sunday")) {
    subject <- paste("COVID-19 Numbers for", str_date)
    intro <- paste("Below are the COVID-19 numbers for", str_date)
  } else {
    subject <- paste("COVID-19 Status Report for", str_date)
    intro <- paste("Attached is the COVID-19 status report for", str_date)
  }

  # Test totals
  test_total_df <- test_calc_total(date = date)
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

  # People totals
  inv <- coviData::read_file_delim(coviData::path_inv(date))
  gc()
  ppl_pos <- coviData::process_positive_people(inv, date = date)
  gc()
  n_ppl_pos <- NROW(ppl_pos)
  n_ppl_neg <- NROW(coviData::process_negative_people(inv, date = date))
  gc()
  remove(inv)
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

  # Confirmed/Probable
  cp_tbl <- case_calc_confirmed_probable(ppl_pos, date = date) %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::opt_align_table_header("right") %>%
    gt::cols_align("right") %>%
    gt::fmt_number(c("total", "C", "P"), decimals = 0L) %>%
    gt::as_raw_html()
  gc()

  # Active
  active_tbl <- case_calc_active(ppl_pos, date = date) %>%
    dplyr::select(-"percent") %>%
    gt::gt() %>%
    fmt_covid_table() %>%
    gt::opt_align_table_header("right") %>%
    gt::cols_align("right") %>%
    gt::fmt_number("n", decimals = 0L) %>%
    gt::as_raw_html()
  gc()

  # Total deaths
  n_deaths <- NROW(filter_deaths(ppl_pos))
  gc()
  remove(ppl_pos)
  gc()

  # Email body numbers
  str_test_total <- test_total_df %>%
    dplyr::filter(tolower(.data[["result"]]) == "total") %>%
    dplyr::pull("n") %>%
    format(big.mark = ",")
  str_ppl_pos <- format(n_ppl_pos, big.mark = ",")
  str_ppl_new <- subtract(
    n_ppl_pos,
    NROW(coviData::process_positive_people(date = date - 1L))
  ) %>% format(big.mark = ",")
  gc()
  str_deaths <- format(n_deaths, big.mark = ",")

  # Vaccination tables
  vac_data <- coviData::vac_prep(coviData::vac_load(date = date))
  gc()
  vac_recent <- vac_table_recent(vac_data, date = date) %>%
    gt::as_raw_html()
  gc()
  vac_ppl <- vac_table_totals(vac_data, date = date) %>%
    gt::as_raw_html()
  gc()
  remove(vac_data)
  gc()

  body <- paste0(
    intro,
    "<br><br>",
    "Total Tests: ", str_test_total, "<br>",
    "Total Cases: ", str_ppl_pos, "<br>",
    "New Cases: ", str_ppl_new, "<br>",
    "Total Deaths: ", str_deaths,
    "<br><br>",
    "Call Center Numbers as of ", str_date, "<br>",
    "Total Answered: **", "<br>",
    "Total Calls to Date: *****",
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

  # Get powerpoint if available
  ppt_path <- fs::dir_ls(
    dir_pptx,
    type = "file",
    regexp = paste0("daily_status_report_", date, ".pptx", collapse = "")
  )

  coviData::notify(
    to = to,
    subject = subject,
    body = body,
    html = TRUE,
    attach = ppt_path
  )
}
