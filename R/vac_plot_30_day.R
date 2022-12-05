#' Plot Daily New Vaccinations by Administration Date
#'
#' @param data Vaccinations data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param date The report date of the data; defaults to the most recent date
#'
#' @param delay Number of days to ignore 7 day average (due to incomplete data);
#'   default is calculated using
#'   \code{\link[covidModel:estimate_delay]{estimate_delay()}}
#'
#' @return A `ggplot` object
#'
#' @export
vac_plot_daily_30 <- function(
  date = NULL
) {



  # Date for current (and previous) counts
  date <- date_vac(date)
  min_date <- date - 30

  # Date for current (and previous) counts
  date <- date_vac(date)


  vac_date <- date


  vac_data <- coviData::vac_prep(date = vac_date)




  # Label numbers


  #Vacs administered by date
  vac_recent <- gt::as_raw_html(vac_table_recent(vac_data, date = vac_date))

  n_avg_vac <- vac_recent %>%
    as_tbl() %>%
    dplyr::mutate(
      dplyr::across(.fns = ~ as.integer(stringr::str_remove_all(.x, "[^0-9]")))
    ) %>%
    dplyr::pull(2L) %>%
    divide_by(7) %>%
    round()

  n_vac_dose1_2 <-vac_recent %>%
    as_tbl() %>%
    dplyr::mutate(
      dplyr::across(.fns = ~ as.integer(stringr::str_remove_all(.x, "[^0-9]")))
    ) %>%
    dplyr::pull(1L) %>%
    divide_by(1) %>%
    round()



  delay = 0L

  vac_data$vacc_date <- lubridate::mdy(vac_data$vacc_date)
  vac_data$vacc_date <- as.character(vac_data$vacc_date)

  vac_data$id <- paste0(vac_data$asiis_pat_id_ptr, vac_data$vacc_date)

  dose1 <- subset(vac_data, dose_count == 1)
  dose2 <- subset(vac_data, dose_count == 2)
  dose3 <- subset(vac_data, dose_count == 3)
  dose4 <- subset(vac_data, dose_count == 4)
  dose5 <- subset(vac_data, dose_count == 5)
  dose6 <- subset(vac_data, dose_count == 6)
  doseoth <- subset(vac_data, dose_count > 6)

  gg_data_dose1 <- prep_daily_vac_data(
    data = dose1,
    min_date = min_date,
    date = date,
    delay = delay
  )
  gg_data_dose1$Dose <- "1"

  gg_data_dose2 <- prep_daily_vac_data(
    data = dose2,
    min_date = min_date,
    date = date,
    delay = delay
  )
  gg_data_dose2$Dose <- "2"



  gg_data_dose3 <- prep_daily_vac_data(
    data = dose3,
    min_date = min_date,
    date = date,
    delay = delay
  )
  gg_data_dose3$Dose <- "3"


  gg_data_dose4 <- prep_daily_vac_data(
    data = dose4,
    min_date = min_date,
    date = date,
    delay = delay
  )
  gg_data_dose4$Dose <- "4"


  gg_data_dose5 <- prep_daily_vac_data(
    data = dose5,
    min_date = min_date,
    date = date,
    delay = delay
  )
  gg_data_dose5$Dose <- "5"


  gg_data_dose6 <- prep_daily_vac_data(
    data = dose6,
    min_date = min_date,
    date = date,
    delay = delay
  )
  gg_data_dose6$Dose <- "6"


  gg_data_doseoth <- prep_daily_vac_data(
    data = doseoth,
    min_date = min_date,
    date = date,
    delay = delay
  )
  gg_data_doseoth$Dose <- "Other"


  #output daily vaccine counts to v drive for google sheets
  data_dose1 <- gg_data_dose1%>%
    dplyr::rename(dose1 = n)%>%
    dplyr::select(vac_date, dose1)

  data_dose2 <- gg_data_dose2%>%
    dplyr::rename(dose2 = n)%>%
    dplyr::select(vac_date, dose2)

  data_dose3 <- gg_data_dose3%>%
    dplyr::rename(dose3 = n)%>%
    dplyr::select(vac_date, dose3)

  data_dose4 <- gg_data_dose4%>%
    dplyr::rename(dose4 = n)%>%
    dplyr::select(vac_date, dose4)

  data_dose5 <- gg_data_dose5%>%
    dplyr::rename(dose5 = n)%>%
    dplyr::select(vac_date, dose5)

  data_dose6 <- gg_data_dose6%>%
    dplyr::rename(dose6 = n)%>%
    dplyr::select(vac_date, dose6)

  data_doseoth <- gg_data_doseoth%>%
    dplyr::rename(doseoth = n)%>%
    dplyr::select(vac_date, doseoth)

  all <- dplyr::left_join(data_dose1, data_dose2)%>%
    dplyr::left_join(data_dose3)%>%
    dplyr::left_join(data_dose4)%>%
    dplyr::left_join(data_dose5)%>%
    dplyr::left_join(data_dose6)%>%
    dplyr::left_join(data_doseoth)

  all$total <- all$dose1 + all$dose2 + all$dose3 + all$dose4 + all$dose5 + all$dose6 + all$doseoth



  gg_data <- dplyr::full_join(gg_data_dose1, gg_data_dose2)%>%
    dplyr::full_join(gg_data_dose3)%>%
    dplyr::full_join(gg_data_dose4)%>%
    dplyr::full_join(gg_data_dose5)%>%
    dplyr::full_join(gg_data_dose6)%>%
    dplyr::full_join(gg_data_doseoth)



  avg <- dplyr::full_join(gg_data_dose1, gg_data_dose2, by="vac_date")%>%
    dplyr::full_join(gg_data_dose3, by="vac_date")%>%
    dplyr::full_join(gg_data_dose4, by="vac_date")%>%
    dplyr::full_join(gg_data_dose5, by="vac_date")%>%
    dplyr::full_join(gg_data_dose6, by="vac_date")%>%
    dplyr::full_join(gg_data_doseoth, by="vac_date")


  dates <- avg %>% dplyr::select(vac_date)

  avg <- avg %>% dplyr::select(dplyr::starts_with("avg"))%>%
    dplyr::mutate(sum_of_rows = rowSums(.))%>%
    dplyr::select(sum_of_rows)%>%
    dplyr::bind_cols(dates)%>%
    dplyr::rename(avg_total = sum_of_rows)%>%
    dplyr::select(vac_date, avg_total)


  gg_data2 <- dplyr::full_join(gg_data, avg)

  n_doseoth <- nrow(doseoth)
  n_dose6 <- nrow(dose6)
  n_dose5 <- nrow(dose5)
  n_dose4 <- nrow(dose4)
  n_dose3 <- nrow(dose3)
  n_dose2 <- nrow(dose2)
  n_dose1 <- nrow(dose1)

  n_total_vac <- n_doseoth + n_dose6 + n_dose5 + n_dose4 + n_dose3 + n_dose2 + n_dose1
  n_plotted <- sum(gg_data[["n"]], na.rm = TRUE)
  n_missing <- n_total_vac - n_plotted

  #n_vac_yest <- nrow(coviData:::vac_prep(date = vac_date - 1))
  n_new <- sum(lubridate::mdy(vac_data$insert_date) == date, na.rm = TRUE)

  #get number of doses administered in last 7 days
  last_7_day <- subset(gg_data, (date-6) <= gg_data$vac_date & gg_data$vac_date <= date)
  n_7day_vac <- sum(last_7_day$n)

  #get number of doses administered in last 30 days
  last_30_day <- subset(gg_data, (date-29) <= gg_data$vac_date & gg_data$vac_date <= date)
  n_30day_vac <- sum(last_30_day$n)

  library(ggplot2)
  library(forcats)

  try_plot <- ggplot2::ggplot(gg_data2, ggplot2::aes(x = vac_date, y = n, fill = fct_rev(Dose))) +
    ggplot2::geom_col()+
    scale_fill_manual(values=c("slategray4", "mediumorchid3","mediumpurple1", "mediumpurple4", "midnightblue","deepskyblue3", "cadetblue2"))+
    labs(fill = "Dose")



  # try_plot <- ggplot(gg_data, aes(x = vac_date, y = n, fill = dose)) +
  #    geom_bar(stat = "identity") + theme(legend.position = "none") +
  #   scale_color_manual(values=c("midnightblue","deepskyblue2" ))

  #write.csv(gg_data, "C:/Users/allison.plaxco/Desktop/ggdata.csv")



  try_plot %>%
    set_ts_vac_theme() %>%
    add_daily_vac_label30(total = n_30day_vac, new7 = n_7day_vac, new = n_new) %>%
    add_daily_vac_axis_labels() %>%
    add_daily_vac_title_caption30(date = date)
}


#' Add Label to Daily Case Plot
#'
#' Adds a label in the upper-left with 'Total' and 'New' cases
#'
#' @param gg_obj A `ggplot` object
#'
#' @param total Total cases in data
#'
#' @param new New cases for report date
#'
#' @return The `ggplot` object with the added label
#'
#' @noRd
add_daily_vac_label30 <- function(gg_obj, total, new7, new) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    "30 Day Total Vaccinations = ", format(total, big.mark = ","), "\n",
    "Past 7 Day Vaccinations = ", format(new7, big.mark = ","), "\n",
    "New Reported Vaccinations = ", format(new, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = 1200L,
      label = label,
      color = "midnightblue",
      fill = "#f0f0f0",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      label.size = 1
    )
}


#' Add Title, Subtitle, and Caption to Daily Case Plot
#'
#' Adds title `"New COVID-19 Vaccinations by Vaccinations Date"`, a subtitle
#' displaying the report date, and a caption stating number missing and data
#' source
#'
#' @param gg_obj A `ggplot` object
#'
#' @param date The report date to add as subtitle
#'
#' @param missing Number of observations missing from graphic
#'
#' @return The `ggplot` object with added title and subtitle
#'
#' @noRd
add_daily_vac_title_caption30 <- function(gg_obj, date) {

  caption <- paste0(
    "Data Source: Tennessee Immunization Information System (TennIIS)\n",
    "Only bivalent booster doses are authorized for age 12+ as of August 31, 2022 and for age 5-11 as of October 12, 2022"
  )

  add_title_caption(
    gg_obj,
    title = "30-Day New COVID-19 Vaccinations by Vaccination Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = caption
  )
}



