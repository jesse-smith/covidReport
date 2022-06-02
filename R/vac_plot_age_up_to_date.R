#' Plot Vaccinated Individuals who are up to date by Age Group
#'
#' `vac_up_to_date()` plots vaccinated individuals by vaccination status and age group in a bar chart.
#'
#'
#' @param .data Vaccination data, as created by
#'   \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param date Date of vaccination data
#'
#'
#' @return A `ggplot` object
#'
#' @export
vac_up_to_date <- function(data = coviData::vac_prep(date = date),
                            date = NULL)
  {

date <- coviData::date_vac(date)


vac_ind = data%>%
  coviData::vac_distinct()


vac_ind$age_at_admin <- as.double(vac_ind$age_at_admin)

vac_age_grp <- function(dbl) {

  vctrs::vec_assert(dbl, ptype = double())

  breaks <- c(0, 5, 12, 18, seq(25, 75, by = 10), 115)
  lbls <- c(
    "0-4",
    "05-11",
    "12-17",
    "18-24",
    "25-34",
    "35-44",
    "45-54",
    "55-64",
    "65-74",
    "75+"
  )

  cut(
    dbl,
    breaks = breaks,
    labels = lbls,
    right = FALSE,
    ordered_result = TRUE
  ) %>% as.character()
}

pop_2019 <- covidReport::pop_2019

library("dplyr")
# as.double(pop_2019$age) %>% vac_age_grp()%>%
#   dplyr::count(.data[["age_grp"]])

pop_by_age <- pop_2019 %>%
  dplyr::mutate(
    age_grp = as.double(.data[["age"]]) %>% vac_age_grp(), .keep = "all"
  ) %>%
  dplyr::group_by(age_grp)%>%
  dplyr::summarise(pop = sum(population))


gg_data <- vac_ind %>%
  dplyr::transmute(
    utd = status,
    age_grp = .data[["age_at_admin"]] %>% vac_age_grp()
  ) %>%
  dplyr::count(.data[["utd"]], .data[["age_grp"]]) %>%
  tidyr::pivot_wider(names_from = "utd", values_from = "n") %>%

  tidyr::pivot_longer(
    c("Not up to date", "Up to date"),
    names_to = "utd",
    values_to = "n",
    names_transform = list(full = as.logical)
  )%>%
  dplyr::left_join(pop_by_age)%>%
  dplyr::mutate(
    pct_pop = (n/pop), .keep = "all"
  )%>%
  subset(age_grp != "0-4")



ce <- gg_data %>%
  arrange(age_grp, rev(utd))



ce <- ce %>%
  group_by(age_grp) %>%
  mutate(label_y = cumsum(pct_pop)) %>%
  mutate(total_label = dplyr::case_when(
    utd == "Not up to date" ~ label_y
  ))

library("ggplot2")

ce%>%
  ggplot(aes(fill=utd, y=pct_pop, x=age_grp)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("midnightblue","deepskyblue3"))+
  geom_text(aes(y = label_y, label = paste0(round(pct_pop*100, digits = 1), "%")), vjust = 1.2, color = "white")+
  geom_text(aes(y = total_label, label = paste0("Total: ", round(total_label*100, digits = 1), "%")), vjust = -1, color = "black")+
  ggplot2::labs(fill = "Status")+
  ggplot2::ggtitle("Population Up to Date with COVID-19 Vaccination by Age")+
  ggplot2::labs(x="Age Group", y= "% Population")+
  ggplot2::labs(subtitle = format(as.Date(date), "%B %d, %Y"))+
  ggthemes::theme_fivethirtyeight()+
  theme(axis.title = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14))+
  ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

}

add_daily_vac_title_caption <- function(gg_obj, date) {

  caption <- paste0(
    #"(N = ", format(missing, big.mark = ","), ")\n",
    "Data Source: Tennessee Immunization Information System (TennIIS)"
  )

  add_title_caption(
    gg_obj,
    title = "New COVID-19 Vaccinations by Vaccination Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = caption
  )
}
