rpt_vac_pptx <- function(
  date = NULL,
  dir = coviData::path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Status Report",
    "automated"
  )
) {


  date <- date_vac(date)
  vac_date <- date


  data = vac_prep(read_vac(date))
  people = vac_prep(read_vac(date), distinct = TRUE)

  people$dose_status <- people$status

  population <- covidReport::pop_2019

  people$pat_gender <- dplyr::case_when(
    is.na(people$pat_gender) ~ "Unknown",
    people$pat_gender == "U" ~ "Unknown",
    people$pat_gender == "O" ~ "Other",
    people$pat_gender == "M" ~ "Male",
    people$pat_gender == "F" ~ "Female"
  )

  people$race <- dplyr::case_when(
    people$race == "BLACK OR AFRICAN AMERICAN"  ~ "Black/African American",
    people$race == "WHITE" ~ "White",
    people$race == "ASIAN" ~ "Asian/Pacific Islander",
    people$race == "OTHER/MULTIRACIAL" ~ "Other/Multiracial",
    TRUE ~ "Unknown"
  )

  people$ethnicity <- dplyr::case_when(
    people$ethnicity == "HISPANIC OR LATINO"  ~ "Hispanic/Latino",
    people$ethnicity == "NOT HISPANIC OR LATINO" ~ "Not Hispanic/Latino",
    TRUE ~ "Other/Unknown"
  )

  sex <- population %>%
    dplyr::group_by(sex)%>%
    dplyr::summarise(n = sum(population))%>%
    dplyr::rename(pat_gender = sex, pop = n)


  race <- population %>%
    dplyr::group_by(race)%>%
    dplyr::summarise(n = sum(population))%>%
    dplyr::rename(pop = n)

  ethnicity <- population %>%
    dplyr::group_by(ethnicity)%>%
    dplyr::summarise(n = sum(population))%>%
    dplyr::rename(pop = n)

  population$age_group <- as.double(population$age) %>%
    vac_age_grp()

  age_group <- population %>%
    dplyr::group_by(age_group)%>%
    dplyr::summarise(n = sum(population))%>%
    dplyr::rename(pop = n)

  library("dplyr")

  gg_sex <- people %>%
    dplyr::group_by(dose_status, pat_gender)%>%
    dplyr::summarise(n = n())%>%
    dplyr::left_join(sex)%>%
    dplyr::mutate(pct_pop = n/pop)%>%
    subset(!is.na(pct_pop))%>%
    dplyr::arrange(pat_gender, desc(dose_status))%>%
    dplyr::group_by(pat_gender) %>%
    dplyr::mutate(cum_total = cumsum(pct_pop))%>%
    dplyr::mutate(label_y = ifelse(
      pct_pop < 0.01, NA, cum_total
    ))%>%
    dplyr::mutate(label_tot = ifelse(
      dose_status == "Bivalent Booster", cum_total, NA
    ))

  # gg_sex2 <- people %>%
  #   dplyr::group_by(status, pat_gender)%>%
  #   dplyr::summarise(n = n())%>%
  #   dplyr::left_join(sex)%>%
  #   dplyr::mutate(pct_pop = n/pop)%>%
  #   subset(!is.na(pct_pop))%>%
  #   dplyr::arrange(pat_gender, desc(status))%>%
  #   dplyr::group_by(pat_gender) %>%
  #   dplyr::mutate(cum_total = cumsum(pct_pop))%>%
  #   dplyr::mutate(label_y = ifelse(
  #     pct_pop < 0.01, NA, cum_total
  #   ))%>%
  #   dplyr::mutate(label_tot = ifelse(
  #     status == "Not up to date", cum_total, NA
  #   ))

  gg_race <- people %>%
    dplyr::group_by(dose_status, race)%>%
    dplyr::summarise(n = n())%>%
    dplyr::left_join(race)%>%
    dplyr::mutate(pct_pop = n/pop)%>%
    subset(!is.na(pct_pop))%>%
    dplyr::arrange(race, desc(dose_status))%>%
    dplyr::group_by(race) %>%
    dplyr::mutate(cum_total = cumsum(pct_pop))%>%
    dplyr::mutate(label_y = ifelse(
      pct_pop < 0.01, NA, cum_total
    ))%>%
    dplyr::mutate(label_tot = ifelse(
      dose_status == "Bivalent Booster", cum_total, NA
    ))


  # gg_race2 <- people %>%
  #   dplyr::group_by(status, race)%>%
  #   dplyr::summarise(n = n())%>%
  #   dplyr::left_join(race)%>%
  #   dplyr::mutate(pct_pop = n/pop)%>%
  #   subset(!is.na(pct_pop))%>%
  #   dplyr::arrange(race, desc(status))%>%
  #   dplyr::group_by(race) %>%
  #   dplyr::mutate(cum_total = cumsum(pct_pop))%>%
  #   dplyr::mutate(label_y = ifelse(
  #     pct_pop < 0.01, NA, cum_total
  #   ))%>%
  #   dplyr::mutate(label_tot = ifelse(
  #     status == "Not up to date", cum_total, NA
  #   ))

  gg_ethnicity <- people %>%
    dplyr::group_by(dose_status, ethnicity)%>%
    dplyr::summarise(n = n())%>%
    dplyr::left_join(ethnicity)%>%
    dplyr::mutate(pct_pop = n/pop)%>%
    subset(!is.na(pct_pop))%>%
    dplyr::arrange(ethnicity, desc(dose_status))%>%
    dplyr::group_by(ethnicity) %>%
    dplyr::mutate(cum_total = cumsum(pct_pop))%>%
    dplyr::mutate(label_y = ifelse(
      pct_pop < 0.01, NA, cum_total
    ))%>%
    dplyr::mutate(label_tot = ifelse(
      dose_status == "Bivalent Booster", cum_total, NA
    ))

  # gg_ethnicity2 <- people %>%
  #   dplyr::group_by(status, ethnicity)%>%
  #   dplyr::summarise(n = n())%>%
  #   dplyr::left_join(ethnicity)%>%
  #   dplyr::mutate(pct_pop = n/pop)%>%
  #   subset(!is.na(pct_pop))%>%
  #   dplyr::arrange(ethnicity, desc(status))%>%
  #   dplyr::group_by(ethnicity) %>%
  #   dplyr::mutate(cum_total = cumsum(pct_pop))%>%
  #   dplyr::mutate(label_y = ifelse(
  #     pct_pop < 0.01, NA, cum_total
  #   ))%>%
  #   dplyr::mutate(label_tot = ifelse(
  #     status == "Not up to date", cum_total, NA
  #   ))



  library("ggplot2")

  people$age_group <- as.double(people$age_at_admin) %>%
    vac_age_grp()

  gg_age <- people %>%
    dplyr::group_by(dose_status, age_group)%>%
    dplyr::summarise(n = n())%>%
    dplyr::left_join(age_group)%>%
    dplyr::mutate(pct_pop = n/pop)%>%
    subset(!is.na(pct_pop))%>%
    dplyr::arrange(age_group, desc(dose_status))%>%
    dplyr::group_by(age_group) %>%
    dplyr::mutate(label_y = cumsum(pct_pop))






  #start age table
  people$age_group <- ifelse(people$age_group == "0-4", "00-04", people$age_group)

  people$age_group <- ifelse(is.na(people$age_group), "Unknown", people$age_group)


  additional_dose_multiple <- people %>%
    dplyr::group_by(dose_status, age_group)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Bivalent Booster") %>%
    dplyr::rename("Bivalent Booster" = n)

  completed <- people %>%
    dplyr::group_by(dose_status, age_group)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Completed/Monovalent Booster") %>%
    dplyr::rename("Completed/Monovalent Booster" = n)

  initiated <- people %>%
    dplyr::group_by(dose_status, age_group)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Initiated") %>%
    dplyr::rename("Initiated" = n)

  age_table <- left_join(initiated, completed, by = "age_group")%>%
    left_join(additional_dose_multiple, by = "age_group") %>%
    dplyr::select(age_group, "Initiated", "Completed/Monovalent Booster", "Bivalent Booster")%>%
    janitor::adorn_totals(where = c("row", "col"))%>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      age_group = "Age Group"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::autofit()

  vac_age <- vac_plot_age(date = date)
  #vac_age_utd <- vac_up_to_date(date = date)

  #Start sex plot and table

  unknown_gender <- sum(people$pat_gender == "Other" | people$pat_gender == "Unknown")

  sex_plot <- gg_sex%>%
    ggplot2::ggplot(ggplot2::aes(fill=dose_status, y=pct_pop, x=pat_gender)) +
    ggplot2::geom_bar(stack="dodge", stat="identity") +
    ggplot2::scale_fill_manual(values=c("deepskyblue4","steelblue3", "midnightblue"))+
    ggplot2::scale_color_manual(values=c("deepskyblue4","steelblue3", "midnightblue"))+
    ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))+
    ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE))+
    ggplot2::labs(fill = "Status")+
    ggplot2::labs(color = "Status")+
    ggplot2::geom_text(ggplot2::aes(y = label_y, label = paste0(round(pct_pop*100, digits = 1), "%")), vjust = 1.2, color = "white")+
    ggplot2::geom_text(ggplot2::aes(y = label_tot, label = paste0("Total: ", round(label_tot*100, digits = 1), "%")), vjust = -1, color = "black")+
    ggplot2::labs(fill = "Status")+
    ggplot2::ggtitle("Population Vaccinated by Sex")+
    ggplot2::labs(x="Sex", y= "% Population Vaccinated by Sex")+
    ggplot2::labs(subtitle = format(as.Date(date), "%B %d, %Y"),
                  caption = paste0(
                    "Note: Excludes vaccinated individuals with other/unknown sex (n = ", format(unknown_gender, big.mark = ","), ")."
                  ))+
    ggthemes::theme_fivethirtyeight()+
    theme(axis.title = element_text(face="bold")) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.caption = element_text(hjust = 0.5))+
    theme(plot.subtitle = element_text(hjust = 0.5, size = 14))+
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

  # sex_plot2 <- gg_sex2%>%
  #   ggplot2::ggplot(ggplot2::aes(fill=status, y=pct_pop, x=pat_gender)) +
  #   ggplot2::geom_bar(stack="dodge", stat="identity") +
  #   ggplot2::scale_fill_manual(values=c("midnightblue","deepskyblue3"))+
  #   ggplot2::scale_color_manual(values=c("midnightblue","deepskyblue3"))+
  #   ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))+
  #   ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE))+
  #   ggplot2::labs(fill = "Status")+
  #   ggplot2::labs(color = "Status")+
  #   ggplot2::geom_text(ggplot2::aes(y = label_y, label = paste0(round(pct_pop*100, digits = 1), "%")), vjust = 1.2, color = "white")+
  #   ggplot2::geom_text(ggplot2::aes(y = label_tot, label = paste0("Total: ", round(label_tot*100, digits = 1), "%")), vjust = -1, color = "black")+
  #   ggplot2::labs(fill = "Status")+
  #   ggplot2::ggtitle("Population Up to Date with COVID-19 Vaccination by Sex")+
  #   ggplot2::labs(x="Sex", y= "% Population Vaccinated by Sex")+
  #   ggplot2::labs(subtitle = format(as.Date(date), "%B %d, %Y"),
  #                 caption = paste0(
  #                   "Note: Excludes vaccinated individuals with other/unknown sex (n = ", format(unknown_gender, big.mark = ","), ")."
  #                 ))+
  #   ggthemes::theme_fivethirtyeight()+
  #   theme(axis.title = element_text(face="bold")) +
  #   theme(plot.title = element_text(hjust = 0.5))+
  #   theme(plot.caption = element_text(hjust = 0.5))+
  #   theme(plot.subtitle = element_text(hjust = 0.5, size = 14))+
  #   ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  #   theme(
  #     panel.grid.major.x = element_blank(),
  #     panel.grid.minor.x = element_blank()
  #   )



  additional_dose_multiple <- people %>%
    dplyr::group_by(dose_status, pat_gender)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Bivalent Booster") %>%
    dplyr::rename("Bivalent Booster" = n)


  completed <- people %>%
    dplyr::group_by(dose_status, pat_gender)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Completed/Monovalent Booster") %>%
    dplyr::rename("Completed/Monovalent Booster" = n)

  initiated <- people %>%
    dplyr::group_by(dose_status, pat_gender)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Initiated") %>%
    dplyr::rename("Initiated" = n)

  sex_table <- left_join(initiated, completed, by = "pat_gender")%>%
    left_join(additional_dose_multiple, by = "pat_gender") %>%
    dplyr::select(pat_gender, "Initiated", "Completed/Monovalent Booster", "Bivalent Booster")%>%
    janitor::adorn_totals(where = c("row", "col"))%>%

    flextable::flextable() %>%
    flextable::set_header_labels(
      pat_gender = "Sex"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::autofit()

  #end sex plot and table




  #Start race plot and table

  unknown_race <- sum(people$race == "Other/Multiracial" | people$race == "Unknown")

  race_plot <- gg_race%>%
    ggplot2::ggplot(ggplot2::aes(fill=dose_status, y=pct_pop, x=race)) +
    ggplot2::geom_bar(stack="dodge", stat="identity") +
    ggplot2::scale_fill_manual(values=c("deepskyblue4","steelblue3", "midnightblue"))+
    ggplot2::scale_color_manual(values=c("deepskyblue4","steelblue3", "midnightblue"))+
    ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))+
    ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE))+
    ggplot2::labs(fill = "Status")+
    ggplot2::labs(color = "Status")+
    ggplot2::geom_text(ggplot2::aes(y = label_y, label = paste0(round(pct_pop*100, digits = 1), "%")), vjust = 1.2, color = "white")+
    ggplot2::geom_text(ggplot2::aes(y = label_tot, label = paste0("Total: ", round(label_tot*100, digits = 1), "%")), vjust = -1, color = "black")+
    ggplot2::labs(fill = "Status")+
    ggplot2::ggtitle("Population Vaccinated by Race")+
    ggplot2::labs(x="Race", y= "% Population Vaccinated by Race")+
    ggplot2::labs(subtitle = format(as.Date(date), "%B %d, %Y"),
                  caption = paste0(
                    "Note: Excludes vaccinated individuals with other/unknown race (n = ", format(unknown_race, big.mark = ","), ")."
                  ))+
    ggthemes::theme_fivethirtyeight()+
    theme(axis.title = element_text(face="bold")) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.caption = element_text(hjust = 0.5))+
    theme(plot.subtitle = element_text(hjust = 0.5, size = 14))+
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

#
#
#   race_plot2 <- gg_race2%>%
#     ggplot2::ggplot(ggplot2::aes(fill=status, y=pct_pop, x=race)) +
#     ggplot2::geom_bar(stack="dodge", stat="identity") +
#     ggplot2::scale_fill_manual(values=c("midnightblue","deepskyblue3"))+
#     ggplot2::scale_color_manual(values=c("midnightblue","deepskyblue3"))+
#     ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))+
#     ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE))+
#     ggplot2::labs(fill = "Status")+
#     ggplot2::labs(color = "Status")+
#     ggplot2::geom_text(ggplot2::aes(y = label_y, label = paste0(round(pct_pop*100, digits = 1), "%")), vjust = 1.2, color = "white")+
#     ggplot2::geom_text(ggplot2::aes(y = label_tot, label = paste0("Total: ", round(label_tot*100, digits = 1), "%")), vjust = -1, color = "black")+
#     ggplot2::labs(fill = "Status")+
#     ggplot2::ggtitle("Population Up to Date with COVID-19 Vaccination by Race")+
#     ggplot2::labs(x="Race", y= "% Population Vaccinated by Race")+
#     ggplot2::labs(subtitle = format(as.Date(date), "%B %d, %Y"),
#                   caption = paste0(
#                     "Note: Excludes vaccinated individuals with other/unknown race (n = ", format(unknown_race, big.mark = ","), ")."
#                   ))+
#     ggthemes::theme_fivethirtyeight()+
#     theme(axis.title = element_text(face="bold")) +
#     theme(plot.title = element_text(hjust = 0.5))+
#     theme(plot.caption = element_text(hjust = 0.5))+
#     theme(plot.subtitle = element_text(hjust = 0.5, size = 14))+
#     ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank()
#     )



  additional_dose_multiple <- people %>%
    dplyr::group_by(dose_status, race)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Bivalent Booster") %>%
    dplyr::rename("Bivalent Booster" = n)

  completed <- people %>%
    dplyr::group_by(dose_status, race)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Completed/Monovalent Booster") %>%
    dplyr::rename("Completed/Monovalent Booster" = n)

  initiated <- people %>%
    dplyr::group_by(dose_status, race)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Initiated") %>%
    dplyr::rename("Initiated" = n)

  race_table <- left_join(initiated, completed, by = "race")%>%
    left_join(additional_dose_multiple, by = "race") %>%
    dplyr::select(race, "Initiated", "Completed/Monovalent Booster", "Bivalent Booster")%>%

    dplyr::arrange(match(race, c("Black/African American", "White", "Asian/Pacific Islander",
                                 "Other/Multiracial", "Unknown")))%>%


    janitor::adorn_totals(where = c("row", "col"))%>%

    flextable::flextable() %>%
    flextable::set_header_labels(
      pat_gender = "Race"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::autofit()

  #end race plot and table




  #Start ethnicity plot and table

  unknown_ethnicity <- sum(people$ethnicity == "Other/Unknown")

  ethnicity_plot <- gg_ethnicity%>%
    ggplot2::ggplot(ggplot2::aes(fill=dose_status, y=pct_pop, x=ethnicity)) +
    ggplot2::geom_bar(stack="dodge", stat="identity") +
    ggplot2::scale_fill_manual(values=c("deepskyblue4","steelblue3", "midnightblue"))+
    ggplot2::scale_color_manual(values=c("deepskyblue4","steelblue3", "midnightblue"))+
    ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))+
    ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE))+
    ggplot2::labs(fill = "Status")+
    ggplot2::labs(color = "Status")+
    ggplot2::geom_text(ggplot2::aes(y = label_y, label = paste0(round(pct_pop*100, digits = 1), "%")), vjust = 1.2, color = "white")+
    ggplot2::geom_text(ggplot2::aes(y = label_tot, label = paste0("Total: ", round(label_tot*100, digits = 1), "%")), vjust = -1, color = "black")+
    ggplot2::labs(fill = "Status")+
    ggplot2::ggtitle("Population Vaccinated by Ethnicity")+
    ggplot2::labs(x="Ethnicity", y= "% Population Vaccinated by Ethnicity")+
    ggplot2::labs(subtitle = format(as.Date(date), "%B %d, %Y"),
                  caption = paste0(
                    "Note: Excludes vaccinated individuals with other/unknown ethnicity (n = ", format(unknown_ethnicity, big.mark = ","), ")."
                  ))+
    ggthemes::theme_fivethirtyeight()+
    theme(axis.title = element_text(face="bold")) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.caption = element_text(hjust = 0.5))+
    theme(plot.subtitle = element_text(hjust = 0.5, size = 14))+
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

#
#   ethnicity_plot2 <- gg_ethnicity2%>%
#     ggplot2::ggplot(ggplot2::aes(fill=status, y=pct_pop, x=ethnicity)) +
#     ggplot2::geom_bar(stack="dodge", stat="identity") +
#     ggplot2::scale_fill_manual(values=c("midnightblue","deepskyblue3"))+
#     ggplot2::scale_color_manual(values=c("midnightblue","deepskyblue3"))+
#     ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))+
#     ggplot2::guides(color = ggplot2::guide_legend(reverse=TRUE))+
#     ggplot2::labs(fill = "Status")+
#     ggplot2::labs(color = "Status")+
#     ggplot2::geom_text(ggplot2::aes(y = label_y, label = paste0(round(pct_pop*100, digits = 1), "%")), vjust = 1.2, color = "white")+
#     ggplot2::geom_text(ggplot2::aes(y = label_tot, label = paste0("Total: ", round(label_tot*100, digits = 1), "%")), vjust = -1, color = "black")+
#     ggplot2::labs(fill = "Status")+
#     ggplot2::ggtitle("Population Up to Date with COVID-19 Vaccination by Ethnicity")+
#     ggplot2::labs(x="Ethnicity", y= "% Population Vaccinated by Ethnicity")+
#     ggplot2::labs(subtitle = format(as.Date(date), "%B %d, %Y"),
#                   caption = paste0(
#                     "Note: Excludes vaccinated individuals with other/unknown ethnicity (n = ", format(unknown_ethnicity, big.mark = ","), ")."
#                   ))+
#     ggthemes::theme_fivethirtyeight()+
#     theme(axis.title = element_text(face="bold")) +
#     theme(plot.title = element_text(hjust = 0.5))+
#     theme(plot.caption = element_text(hjust = 0.5))+
#     theme(plot.subtitle = element_text(hjust = 0.5, size = 14))+
#     ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank()
#     )




  additional_dose_multiple <- people %>%
    dplyr::group_by(dose_status, ethnicity)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Bivalent Booster") %>%
    dplyr::rename("Bivalent Booster" = n)

  completed <- people %>%
    dplyr::group_by(dose_status, ethnicity)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Completed/Monovalent Booster") %>%
    dplyr::rename("Completed/Monovalent Booster" = n)

    initiated <- people %>%
    dplyr::group_by(dose_status, ethnicity)%>%
    dplyr::summarise(n = n()) %>%
    subset(dose_status == "Initiated") %>%
    dplyr::rename("Initiated" = n)

ethnicity_table <- left_join(initiated, completed, by = "ethnicity")%>%
    left_join(additional_dose_multiple, by = "ethnicity") %>%
    dplyr::select(ethnicity, "Initiated", "Completed/Monovalent Booster", "Bivalent Booster")%>%
  janitor::adorn_totals(where = c("row", "col"))%>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      ethnicity = "Ethnicity"
    ) %>%
    fmt_covid_table(total = TRUE) %>%
    flextable::autofit()

#end ethnicity plot and table








#table by vaccinations

vaccinations <- vac_table_recent_new(date = vac_date)

vaccinated_people <- vac_table_totals_new(date = vac_date)

#vac_map <- vac_plot_goal(date = vac_date)

vac_plot <- vac_plot_daily(date = vac_date)
vac_plot30 <- vac_plot_daily_30(date = vac_date)


#vac_map_pct <- vac_map_pct(date = vac_date)




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


# # Create vac by age plot slide
# pptx <- pptx %>%
#   officer::add_slide("Picture only", master) %>%
#   officer::ph_with(
#     value = vac_age_utd,
#     location = officer::ph_location_type("pic")
#   )


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

# pptx <- pptx %>%
#   officer::add_slide("Picture only", master) %>%
#   officer::ph_with(
#     value = sex_plot2,
#     location = officer::ph_location_type("pic")
#   )



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

# pptx <- pptx %>%
#   officer::add_slide("Picture only", master) %>%
#   officer::ph_with(
#     value = race_plot2,
#     location = officer::ph_location_type("pic")
#   )



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

# pptx <- pptx %>%
#   officer::add_slide("Picture only", master) %>%
#   officer::ph_with(
#     value = ethnicity_plot2,
#     location = officer::ph_location_type("pic")
#   )


# Create vac daily plot slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = vac_plot,
    location = officer::ph_location_type("pic")
  )


# Create vac daily plot 30 day slide
pptx <- pptx %>%
  officer::add_slide("Picture only", master) %>%
  officer::ph_with(
    value = vac_plot30,
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
  people = vac_prep(date = date, distinct = TRUE),
  date = NULL
) {

  pop <- 937166

  today <- date_vac(date)

  library("dplyr")

  title <- paste0(
    "People Vaccinated (", format(today, "%m/%d/%y"), ")"
  )
  count_people <- people %>%
    dplyr::group_by(.data[["status"]]) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::arrange(dplyr::desc(.data[["status"]])) %>%
    dplyr::mutate(pct_pop = .data[["n"]] / {{ pop }})

  count_people$pct_pop <- round(count_people$pct_pop*100, 1)

  count_people%>%
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
  data = coviData:::vac_prep(date = date),
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






