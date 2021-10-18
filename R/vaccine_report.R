rpt_daily_pptx <- function(
  date = NULL,
  dir = coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Status Report",
    "automated"
  )
) {


# Vaccination tables
  date <- date_vac(date)
vac_date <- date

#all_dose <- coviData:::vac_prep_all()

#table by vaccinations

vaccinations <- vac_table_recent_new()

vaccinated_people <- vac_table_totals_new()

vac_map <- vac_plot_goal()

vac_plot <- vac_plot_daily()

vac_age <- vac_plot_age()

vac_map_pct <- vac_map_pct()


#Start processing data for tables
vac_data <- read_vac(vac_date)

vac_data$address_zip2 <- vac_parse_zip(vac_data$address_zip)

vac_data$shelby_resident <- ifelse(is.na(vac_data$address_zip2)
                                   | vac_data$address_zip2 != "Other", "Yes", "No")

vac_data2 <- subset(vac_data, shelby_resident != "No")

vac_data2 <- subset(vac_data2, dose_count<= 2)

vac_data2$recip_fully_vacc <- ifelse(vac_data2$dose_count == 2
                                     & (vac_data2$cvx_code == 207 |vac_data2$cvx_code == 208), 1,0)


vac_data2$recip_fully_vacc <- ifelse(vac_data2$dose_count == 1
                                     & (vac_data2$cvx_code == 210 |vac_data2$cvx_code == 212), 1,
                                     vac_data2$recip_fully_vacc)

vac_data2 <- subset(vac_data2, (cvx_code != 210 & cvx_code != 212) | dose_count < 2)

#sum(vac_data2$recip_fully_vacc == 0)

vac_people <- data.table::setorder(vac_data2, -dose_count)


vac_people <- dplyr::distinct(vac_people, asiis_pat_id_ptr, .keep_all = TRUE)
#sum(vac_people$dose_count == 2)




library("dplyr")

.data = vac_prep(read_vac(vac_date))
by_pop = TRUE
incl_under_12 = FALSE

by_pop <- coviData::assert_bool(by_pop)
incl_under_12 <- coviData::assert_bool(incl_under_12)

gg_data <- .data %>%
  vac_count_grp() %>%
  vac_join_age_pop(incl_under_12 = incl_under_12) %>%
  vac_age_fct()

atleast1_age <- gg_data %>% subset(full == FALSE)
fully_age <- gg_data %>% subset(full == TRUE)

fully_age$fully_vac <- fully_age$n_vac
fully_age <- fully_age %>% dplyr::select(-full, -n_vac, -n_pop)

atleast1_age$atleast1 <- atleast1_age$n_vac
atleast1_age <- atleast1_age %>% dplyr::select(-full, -n_vac, -n_pop)

age_table <- dplyr::full_join(atleast1_age, fully_age)%>%
  janitor::adorn_totals()%>%
  flextable::flextable() %>%
  flextable::set_header_labels(
    age_grp = "Age Group",
    atleast1 = "Atleast 1 Dose",
    fully_vac = "Fully Vaccinated"
  ) %>%
  fmt_covid_table(total = TRUE) %>%
  flextable::autofit()







#START SEX PLOT
vac_people$pat_gender <- ifelse(vac_people$pat_gender == "F", "Female", vac_people$pat_gender)

vac_people$pat_gender <- ifelse(vac_people$pat_gender == "M", "Male", vac_people$pat_gender)

vac_people$pat_gender <- ifelse(is.na(vac_people$pat_gender) | vac_people$pat_gender == "O"
                                | vac_people$pat_gender == "U", "Other/Unknown",
                                vac_people$pat_gender)

full_sex <- vac_people %>% group_by(pat_gender, recip_fully_vacc)%>%
  summarize(n = n())%>% dplyr::rename(sex = pat_gender, full = recip_fully_vacc, n_vac = n)%>%
  subset(full==1)

atleast1_sex <- vac_people %>% group_by(pat_gender)%>%
  summarize(n = n())%>% dplyr::rename(sex = pat_gender, n_vac = n)
atleast1_sex$full <- 0

sex <- dplyr::full_join(full_sex, atleast1_sex)

sex$n_pop <- ifelse(sex$sex == "Female", 491704, 445462)
sex$n_pop <- ifelse(sex$sex == "Other/Unknown", NA, sex$n_pop)

#recip_fully_vac needs to be true or false
sex$full <- ifelse(sex$full == 1, TRUE, FALSE )

#subset to exclude other/unknown
gg_data <- subset(sex, sex != "Other/Unknown")

gg_data <- data.table::setorder(gg_data, full)

sex_plot <- gg_data%>%
  vac_sex_ggplot(by_pop = by_pop) %>%
  set_covid_theme() %>%
  vac_age_axis_limits(by_pop = by_pop) %>%
  add_vac_axis_labels(by_pop = by_pop, demog = "Sex") %>%
  add_vac_age_col(by_pop = by_pop) %>%
  add_vac_age_col_labels() %>%
  add_vac_age_scale(by_pop = by_pop) %>%
  remove_x_grid() %>%
  add_vac_title_caption(by_pop = by_pop, date = date, demog = "Sex")

#make sex table
full_sex$fully_vac <- full_sex$n_vac
full_sex <- full_sex %>% dplyr::select(-full, -n_vac)

atleast1_sex$atleast1 <- atleast1_sex$n_vac
atleast1_sex <- atleast1_sex %>% dplyr::select(-full, -n_vac)

sex_table <- dplyr::full_join(atleast1_sex, full_sex)%>%
  janitor::adorn_totals()%>%
  flextable::flextable() %>%
  flextable::set_header_labels(
    sex = "Sex",
    atleast1 = "Atleast 1 Dose",
    fully_vac = "Fully Vaccinated"
  ) %>%
  fmt_covid_table(total = TRUE) %>%
  flextable::autofit()




#START RACE PLOT
vac_people$race <- ifelse(vac_people$race == "ASIAN", "Asian", vac_people$race)

vac_people$race <- ifelse(vac_people$race == "WHITE", "White", vac_people$race)

vac_people$race <- ifelse(vac_people$race == "BLACK OR AFRICAN AMERICAN", "Black/African American", vac_people$race)

vac_people$race <- ifelse(is.na(vac_people$race) | vac_people$race == "OTHER/MULTIRACIAL"
                          | vac_people$race == "UNKNOWN", "Other/Unknown",
                          vac_people$race)


full_race <- vac_people %>% group_by(race, recip_fully_vacc)%>%
  summarize(n = n())%>% dplyr::rename(full = recip_fully_vacc, n_vac = n)%>%
  subset(full==1)

atleast1_race <- vac_people %>% group_by(race)%>%
  summarize(n = n())%>% dplyr::rename(n_vac = n)
atleast1_race$full <- 0

race <- dplyr::full_join(full_race, atleast1_race)

race$n_pop <- ifelse(race$race == "Asian", 26200, 0)
race$n_pop <- ifelse(race$race == "Black/African American", 504715, race$n_pop)
race$n_pop <- ifelse(race$race == "White", 365798, race$n_pop)
race$n_pop <- ifelse(race$race == "Other/Unknown", NA, race$n_pop)




#recip_fully_vac needs to be true or false
race$full <- ifelse(race$full == 1, TRUE, FALSE )

#subset to exclude other/unknown
gg_data <- subset(race, race != "Other/Unknown")

gg_data <- data.table::setorder(gg_data, full)

race_plot <- gg_data%>%
  vac_race_ggplot(by_pop = by_pop) %>%
  set_covid_theme() %>%
  vac_age_axis_limits(by_pop = by_pop) %>%
  add_vac_axis_labels(by_pop = by_pop, demog = "Race") %>%
  add_vac_age_col(by_pop = by_pop) %>%
  add_vac_age_col_labels() %>%
  add_vac_age_scale(by_pop = by_pop) %>%
  remove_x_grid() %>%
  add_vac_title_caption(by_pop = by_pop, date = date, demog = "Race")

#make race table
full_race$fully_vac <- full_race$n_vac
full_race <- full_race %>% dplyr::select(-full, -n_vac)

atleast1_race$atleast1 <- atleast1_race$n_vac
atleast1_race <- atleast1_race %>% dplyr::select(-full, -n_vac)

race_table <- dplyr::full_join(atleast1_race, full_race)%>%
  janitor::adorn_totals()%>%

    flextable::flextable() %>%
    flextable::set_header_labels(
      race = "Race",
      atleast1 = "Atleast 1 Dose",
      fully_vac = "Fully Vaccinated"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::autofit()




#START ETHNICITY

vac_people$ethnicity <- ifelse(vac_people$ethnicity == "Hispanic Or Latino", "Hispanic or Latino", vac_people$ethnicity)

vac_people$ethnicity <- ifelse(vac_people$ethnicity == "Not Hispanic Or Latino", "Not Hispanic or Latino", vac_people$ethnicity)

full_eth <- vac_people %>% group_by(ethnicity, recip_fully_vacc)%>%
  summarize(n = n())%>% dplyr::rename(full = recip_fully_vacc, n_vac = n)%>%
  subset(full==1)

atleast1_eth <- vac_people %>% group_by(ethnicity)%>%
  summarize(n = n())%>% dplyr::rename(n_vac = n)
atleast1_eth$full <- 0

ethnicity <- dplyr::full_join(full_eth, atleast1_eth)

ethnicity$n_pop <- ifelse(ethnicity$ethnicity == "Hispanic or Latino", 62125, 875041)
ethnicity$n_pop <- ifelse(ethnicity$ethnicity == "Unknown", NA, ethnicity$n_pop)

#recip_fully_vac needs to be true or false
ethnicity$full <- ifelse(ethnicity$full == 1, TRUE, FALSE )

#subset to exclude other/unknown
gg_data <- subset(ethnicity, ethnicity != "Unknown")

gg_data <- data.table::setorder(gg_data, full)

ethnicity_plot <- gg_data%>%
  vac_ethnicity_ggplot(by_pop = by_pop) %>%
  set_covid_theme() %>%
  vac_age_axis_limits(by_pop = by_pop) %>%
  add_vac_axis_labels(by_pop = by_pop, demog = "Ethnicity") %>%
  add_vac_age_col(by_pop = by_pop) %>%
  add_vac_age_col_labels() %>%
  add_vac_age_scale(by_pop = by_pop) %>%
  remove_x_grid() %>%
  add_vac_title_caption(by_pop = by_pop, date = date, demog = "Ethnicity")

#make ethnicity table
full_eth$fully_vac <- full_eth$n_vac
full_eth <- full_eth %>% dplyr::select(-full, -n_vac)

atleast1_eth$atleast1 <- atleast1_eth$n_vac
atleast1_eth <- atleast1_eth %>% dplyr::select(-full, -n_vac)

ethnicity_table <- dplyr::full_join(atleast1_eth, full_eth)%>%
  janitor::adorn_totals()%>%
  flextable::flextable() %>%
  flextable::set_header_labels(
    ethnicity = "Ethnicity",
    atleast1 = "Atleast 1 Dose",
    fully_vac = "Fully Vaccinated"
  ) %>%
  fmt_covid_table(total = TRUE) %>%
  flextable::autofit()







#start making powerpoint

# Load powerpoint template
pptx <- officer::read_pptx(system.file(
  "extdata", "covid_report_template.pptx",
  package = "covidReport",
  mustWork = TRUE
))
gc()



# Report variables
master <- "HD Blue and White"
date_ppt <- format(lubridate::as_date(vac_date), "%B %d, %Y")



# Create title slide
title <- "COVID-19 Vaccination Report"
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




# Create vaccinations slide
cp_title <- "COVID-19 Vaccinations"
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
    value = vaccinations,
    location = ph_location_table(
      vaccinations,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )


# Create vaccinated people slide
cv_title <- "Vaccinated People"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = cv_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date_ppt,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = vaccinated_people,
    location = ph_location_table(
      vaccinated_people,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )



# Create vac map slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) #%>%
  # officer::ph_with(
  #   value = ,
  #   location = officer::ph_location_type("pic")
  # )




# Create vaccinations by age table slide
age_title <- "COVID-19 Vaccinations by Age Group"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = age_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date_ppt,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = age_table,
    location = ph_location_table(
      age_table,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )

# Create vac by age plot slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = vac_age,
    location = officer::ph_location_type("pic")
  )



# Create vaccinations by sex table slide
sex_title <- "COVID-19 Vaccinations by Sex"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = sex_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date_ppt,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = sex_table,
    location = ph_location_table(
      sex_table,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )

# Create vac by sex plot slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = sex_plot,
    location = officer::ph_location_type("pic")
  )





# Create vaccinations by race table slide
race_title <- "COVID-19 Vaccinations by Race"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = race_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date_ppt,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = race_table,
    location = ph_location_table(
      race_table,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )

# Create vac by race plot slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = race_plot,
    location = officer::ph_location_type("pic")
  )



# Create vaccinations by Ethnicity table slide
ethnicity_title <- "COVID-19 Vaccinations by Ethnicity"
pptx <- pptx %>%
  officer::add_slide("Table", master) %>%
  officer::ph_with(
    value = ethnicity_title,
    location = officer::ph_location_type("title")
  ) %>%
  officer::ph_with(
    value = date_ppt,
    location = officer::ph_location_type("subTitle")
  ) %>%
  officer::ph_with(
    value = ethnicity_table,
    location = ph_location_table(
      ethnicity_table,
      pptx,
      layout = "Table",
      pos_h = FALSE,
      valign = 1
    )
  )

# Create vac by ethnicity plot slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = ethnicity_plot,
    location = officer::ph_location_type("pic")
  )


#
# # Create vac age plot slide
# vac_age_title <- "COVID-19 Vaccination by Age"
# pptx <- pptx %>%
#   officer::add_slide("Two Content", master) %>%
#   officer::ph_with(
#     value = vac_age_title,
#     location = officer::ph_location_type("title")
#   ) %>%
#   officer::ph_with(
#     value = date_ppt,
#     location = officer::ph_location_type("subTitle")
#   ) %>%
#   officer::ph_with(
#     value = age_table,
#     location = ph_location_table(
#       age_table,
#       pptx,
#       layout = "Two Content",
#       valign = 1
#     )
#   ) %>%
#   officer::ph_with(
#     value = vac_age,
#     location = officer::ph_location_type("pic")
#   )
#
#
#
#
#
# # Create vac sex plot slide
# vac_sex_title <- "COVID-19 Vaccination by Sex"
# pptx <- pptx %>%
#   officer::add_slide("Two Content", master) %>%
#   officer::ph_with(
#     value = vac_sex_title,
#     location = officer::ph_location_type("title")
#   ) %>%
#   officer::ph_with(
#     value = date_ppt,
#     location = officer::ph_location_type("subTitle")
#   ) %>%
#   officer::ph_with(
#     value = sex_table,
#     location = ph_location_table(
#       sex_table,
#       pptx,
#       layout = "Two Content",
#       valign = 1
#     )
#   ) %>%
#   officer::ph_with(
#     value = sex_plot,
#     location = officer::ph_location_type("pic")
#   )
#
#
#
#
#
# # Create vac race plot slide
# vac_race_title <- "COVID-19 Vaccination by Race"
# pptx <- pptx %>%
#   officer::add_slide("Two Content", master) %>%
#   officer::ph_with(
#     value = vac_race_title,
#     location = officer::ph_location_type("title")
#   ) %>%
#   officer::ph_with(
#     value = date_ppt,
#     location = officer::ph_location_type("subTitle")
#   ) %>%
#   officer::ph_with(
#     value = race_table,
#     location = ph_location_table(
#       race_table,
#       pptx,
#       layout = "Two Content",
#       valign = 1
#     )
#   ) %>%
#   officer::ph_with(
#     value = race_plot,
#     location = officer::ph_location_type("pic")
#   )
#
#
#
#
#
#
#
# # Create vac ethnicity plot slide
# vac_ethnicity_title <- "COVID-19 Vaccination by Race"
# pptx <- pptx %>%
#   officer::add_slide("Two Content", master) %>%
#   officer::ph_with(
#     value = vac_ethnicity_title,
#     location = officer::ph_location_type("title")
#   ) %>%
#   officer::ph_with(
#     value = date_ppt,
#     location = officer::ph_location_type("subTitle")
#   ) %>%
#   officer::ph_with(
#     value = ethnicity_table,
#     location = ph_location_table(
#       ethnicity_table,
#       pptx,
#       layout = "Two Content",
#       valign = 1
#     )
#   ) %>%
#   officer::ph_with(
#     value = ethnicity_plot,
#     location = officer::ph_location_type("pic")
#   )
#
#
#




# Create vac daily plot slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = vac_plot,
    location = officer::ph_location_type("pic")
  )



# Create zip slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) #%>%
  # officer::ph_with(
  #   value = ,
  #   location = officer::ph_location_type("pic")
  # )


if (!is.null(dir)) {
  path <- coviData::path_create(
    dir,
    paste0("vaccine_report_", vac_date, ".pptx")
  )
  print(pptx, target = path)
  attr(pptx, "path") <- path
}

pptx


}





vac_ethnicity_ggplot <- function(data, by_pop) {

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[["ethnicity"]],
      y = !!vac_age_choose_y(by_pop = by_pop),
      color = .data[["full"]],
      fill = .data[["full"]]
    )
  )
}


vac_race_ggplot <- function(data, by_pop) {

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[["race"]],
      y = !!vac_age_choose_y(by_pop = by_pop),
      color = .data[["full"]],
      fill = .data[["full"]]
    )
  )
}




vac_sex_ggplot <- function(data, by_pop) {

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[["sex"]],
      y = !!vac_age_choose_y(by_pop = by_pop),
      color = .data[["full"]],
      fill = .data[["full"]]
    )
  )
}



add_vac_axis_labels <- function(gg_obj, by_pop, demog) {
  by_pop <- coviData::assert_bool(by_pop)

  ylab <- dplyr::if_else(by_pop, "% Population", "% Vaccinations")
  add_axis_labels(gg_obj, xlab = demog, ylab = ylab)
}



add_vac_title_caption <- function(gg_obj, by_pop, date, demog) {

  title <- dplyr::case_when(
    by_pop ~ paste0("Population Vaccinated by ", demog),
    !by_pop ~ paste0("People Vaccinated by ",demog)
  )
  subtitle <- format(as.Date(date), "%B %d, %Y")

  caption <- "Data Source: Tennessee Immunization Information System (TennIIS)"
  add_title_caption(
    gg_obj,
    title = title,
    subtitle = subtitle,
    caption = caption
  )
}



#' Create a NEW Table of Vaccination Totals
#'
#' @param data TennIIS vaccination data, as output by
#'   \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param date The download date of the data to use; defaults to most recent
#'   file
#'
#' @return A `gt_tbl`
#'
#' @export
vac_table_totals_new <- function(
  data_all = coviData:::vac_prep_all(coviData::read_vac(date = date)),
  data_12 = coviData:::vac_prep(coviData::read_vac(date = date)),
  date = NULL
) {

  pop <- 937166

  today <- date_vac(date)



  title <- paste0(
    "People Vaccinated (", format(today, "%m/%d/%y"), ")"
  )
  dose_12 <- vac_count(data_12) %>%
    dplyr::mutate(
      status = dplyr::if_else(
        .data[["recip_fully_vacc"]] %in% TRUE,
        "Completed",
        "Initiated"
      ),
      .before = 1L
    )  %>%
    dplyr::group_by(.data[["status"]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(.data[["status"]])) %>%
    dplyr::mutate(pct_pop = .data[["n"]] / {{ pop }})

  add_doses <- vac_count(data_all, filter_2nd_dose = FALSE) %>%
    dplyr::mutate(
      status = dplyr::if_else(
        .data[["dose_count"]] %in% 3,
        "Additional Dose",
        "Drop"
      ),
      .before = 1L
    )  %>%
    dplyr::group_by(.data[["status"]]) %>%
    dplyr::summarize(n = sum(.data[["n"]], na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(.data[["status"]])) %>%
    dplyr::mutate(pct_pop = .data[["n"]] / {{ pop }}) %>%
    subset(status != "Drop")

  additional_doses <- sum(add_doses$n)
  additional_doses_pct <- sum(add_doses$pct_pop)

  joined_doses <- dplyr::full_join(dose_12, add_doses)

  joined_doses$n <- ifelse(joined_doses$status == "Completed", joined_doses$n-additional_doses, joined_doses$n)

  joined_doses$pct_pop <- ifelse(joined_doses$status == "Completed", joined_doses$pct_pop-additional_doses_pct, joined_doses$pct_pop)


  joined_doses$pct_pop <- round(joined_doses$pct_pop*100, 1)

  joined_doses%>%
    janitor::adorn_totals()%>%

    flextable::flextable() %>%
    flextable::set_header_labels(
      type = "",
      status = "Status",
      N = "N",
      pct_pop = "% Population"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::autofit()

}










#' Create NEW Table of Recent and Total Vaccine Doses
#'
#' @param data TennIIS vaccination data, as output from
#'   \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param date The download date of the data to use; defaults to the most recent
#'   file
#'
#' @return A `gt_tbl`
#'
#' @export
vac_table_recent_new <- function(
  data = coviData:::vac_prep_all(coviData::read_vac(date = date)),
  date = NULL
) {

  today <- vac_date(date)

  n_total <- data %>%
    vac_count(by = "dose", filter_2nd_dose = FALSE) %>%
    dplyr::pull("n") %>%
    sum(na.rm = TRUE)

  n_last_week <- data %>%
    dplyr::mutate(
      vacc_date = coviData::std_dates(
        .data[["vacc_date"]],
        orders = "mdy",
        force = "dt",
        train = FALSE
      )
    ) %>%
    dplyr::filter(.data[["vacc_date"]] > today - 7L) %>%
    vac_count(by = "dose", filter_2nd_dose = FALSE) %>%
    dplyr::pull("n") %>%
    sum(na.rm = TRUE)

  title <- paste0("COVID-19 Vaccinations (", format(today, "%m/%d/%y"), ")")

  tibble::tibble(
    n_total,
    n_last_week
  ) %>%

    flextable::flextable() %>%
    flextable::set_header_labels(
      n_total = "Total Doses",
      n_last_week = paste0("Doses Reported", "\n", "Within Last 7 Days")
    ) %>%
    fmt_covid_table() %>%
    flextable::autofit()

}






