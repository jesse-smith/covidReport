#' Create Powerpoint for Weekly Demographic Status Report
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
rpt_demog_pptx <- function(
  date = NULL,
  dir = coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Status Report",
    "automated"
  ),
  inv = pos(process_inv(read_inv(date)))
) {

  # Ensure valid date
  date <- coviData::date_inv(date)

  # Load powerpoint template
  pptx <- officer::read_pptx(system.file(
    "extdata", "covid_report_template.pptx",
    package = "covidReport",
    mustWork = TRUE
  ))
  gc()

  # Data
  pos_ppl <- inv %>%
    dplyr::mutate(.id_tmp_ = dplyr::row_number()) %>%
    dplyr::filter(
      .data[[".id_tmp_"]] %in% unique(c(
        filter_active(., date = {{ date }})[[".id_tmp_"]],
        filter_peds(.)[[".id_tmp_"]],
        filter_deaths(.)[[".id_tmp_"]]
      ))
    ) %>%
    dplyr::select(
      "specimen_coll_dt",
      "patient_dob",
      "die_from_illness_ind",
      "illness_onset_dt",
      "inv_start_dt",
      "inv_death_dt",
      "patient_deceased_dt",
      "age_in_years",
      "patient_ethnicity",
      "patient_race_calc",
      "patient_current_sex"
    )
  remove(inv)
  gc()

  # Active Cases by Age
  active_tbl_age <- active_table_age(pos_ppl, date = date)
  active_plt_age <- active_plot_age(pos_ppl, date = date)
  gc()

  # Active Cases by Sex
  active_tbl_sex <- active_table_sex(pos_ppl, date = date)
  active_plt_sex <- active_plot_sex(pos_ppl, date = date)
  gc()
  # Active Cases by Race
  active_tbl_race <- active_table_race(pos_ppl, date = date)
  active_plt_race <- active_plot_race(pos_ppl, date = date)
  gc()

  # Active Cases by Ethnicity
  active_tbl_ethnicity <- active_table_ethnicity(pos_ppl, date = date)
  active_plt_ethnicity <- active_plot_ethnicity(pos_ppl, date = date)
  gc()

  # Pediatric Cases by Sex
  peds_tbl_sex <- peds_table_sex(pos_ppl, date = date)
  peds_plt_sex <- peds_plot_sex(pos_ppl, date = date)
  gc()

  # Pediatric Cases by Race
  peds_tbl_race <- peds_table_race(pos_ppl, date = date)
  peds_plt_race <- peds_plot_race(pos_ppl, date = date)
  gc()

  # Pediatric Cases by Ethnicity
  peds_tbl_ethnicity <- peds_table_ethnicity(pos_ppl, date = date)
  peds_plt_ethnicity <- peds_plot_ethnicity(pos_ppl, date = date)
  gc()

  # Deaths by Age
  death_tbl_age <- death_table_age(pos_ppl, date = date)
  death_plt_age <- death_plot_age(pos_ppl, date = date)
  gc()

  # Deaths by Sex
  death_tbl_sex <- death_table_sex(pos_ppl, date = date)
  death_plt_sex <- death_plot_sex(pos_ppl, date = date)
  gc()

  # Deaths by Race
  death_tbl_race <- death_table_race(pos_ppl, date = date)
  death_plt_race <- death_plot_race(pos_ppl, date = date)
  gc()

  # Deaths by Ethnicity
  death_tbl_ethnicity <- death_table_ethnicity(pos_ppl, date = date)
  death_plt_ethnicity <- death_plot_ethnicity(pos_ppl, date = date)
  gc()

  remove(pos_ppl)

  # Report variables
  master <- "HD Blue and White"
  date_ppt <- format(date, "%B %d, %Y")

  # Create title slide
  title <- "COVID-19 Demographic Status Report"
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

  # Create active age slide
  active_age_title <- "Active COVID-19 Cases by Age"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = active_age_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = active_tbl_age,
      location = ph_location_table(
        active_tbl_age,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    ) %>%
    officer::ph_with(
      value = active_plt_age,
      location = officer::ph_location_type("pic")
    )

  # Create active sex slide
  active_sex_title <- "Active COVID-19 Cases by Sex"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = active_sex_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = active_tbl_sex,
      location = ph_location_table(
        active_tbl_sex,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    ) %>%
    officer::ph_with(
      value = active_plt_sex,
      location = officer::ph_location_type("pic")
    )

  # Create active race slide
  active_race_title <- "Active COVID-19 Cases by Race"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = active_race_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = active_plt_race,
      location = officer::ph_location_type("pic")
    ) %>%
    officer::ph_with(
      value = active_tbl_race,
      location = ph_location_table(
        active_tbl_race,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    )

  # Create active ethnicity slide
  active_eth_title <- "Active COVID-19 Cases by Ethnicity"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = active_eth_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = active_tbl_ethnicity,
      location = ph_location_table(
        active_tbl_ethnicity,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    ) %>%
    officer::ph_with(
      value = active_plt_ethnicity,
      location = officer::ph_location_type("pic")
    )

  # Create peds sex slide
  peds_sex_title <- "Pediatric COVID-19 Cases by Sex"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = peds_sex_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = peds_tbl_sex,
      location = ph_location_table(
        peds_tbl_sex,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    ) %>%
    officer::ph_with(
      value = peds_plt_sex,
      location = officer::ph_location_type("pic")
    )

  # Create peds race slide
  peds_race_title <- "Pediatric COVID-19 Cases by Race"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = peds_race_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = peds_plt_race,
      location = officer::ph_location_type("pic")
    ) %>%
    officer::ph_with(
      value = peds_tbl_race,
      location = ph_location_table(
        peds_tbl_race,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    )

  # Create peds ethnicity slide
  peds_eth_title <- "Pediatric COVID-19 Cases by Ethnicity"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = peds_eth_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = peds_tbl_ethnicity,
      location = ph_location_table(
        peds_tbl_ethnicity,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    ) %>%
    officer::ph_with(
      value = peds_plt_ethnicity,
      location = officer::ph_location_type("pic")
    )

  # Create death age slide
  death_age_title <- "COVID-19 Deaths by Age"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = death_age_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = death_tbl_age,
      location = ph_location_table(
        death_tbl_age,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    ) %>%
    officer::ph_with(
      value = death_plt_age,
      location = officer::ph_location_type("pic")
    )

  # Create death sex slide
  death_sex_title <- "COVID-19 Deaths by Sex"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = death_sex_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = death_tbl_sex,
      location = ph_location_table(
        death_tbl_sex,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    ) %>%
    officer::ph_with(
      value = death_plt_sex,
      location = officer::ph_location_type("pic")
    )

  # Create death race slide
  death_race_title <- "COVID-19 Deaths by Race"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = death_race_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = death_plt_race,
      location = officer::ph_location_type("pic")
    ) %>%
    officer::ph_with(
      value = death_tbl_race,
      location = ph_location_table(
        death_tbl_race,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    )

  # Create death ethnicity slide
  death_eth_title <- "COVID-19 Deaths by Ethnicity"
  pptx <- pptx %>%
    officer::add_slide("Two Content", master) %>%
    officer::ph_with(
      value = death_eth_title,
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = date_ppt,
      location = officer::ph_location_type("subTitle")
    ) %>%
    officer::ph_with(
      value = death_tbl_ethnicity,
      location = ph_location_table(
        death_tbl_ethnicity,
        pptx,
        layout = "Two Content",
        valign = 1
      )
    ) %>%
    officer::ph_with(
      value = death_plt_ethnicity,
      location = officer::ph_location_type("pic")
    )

  if (!is.null(dir)) {
    path <- coviData::path_create(
      dir,
      paste0("demographic_status_report_", date, ".pptx")
    )
    print(pptx, target = path)
    attr(pptx, "path") <- path
  }

  pptx
}
