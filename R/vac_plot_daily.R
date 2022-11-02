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
vac_plot_daily <- function(
  date = NULL
) {

  min_date <- lubridate::as_date("2020-12-15")

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

  write.csv(all, file = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/COVID-19 Vaccine Reporting/daily vac counts/dose_counts_", date, ".csv"))
  #end output daily vaccint counts to v drive for google sheets

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

  n_vac_yest <- nrow(coviData:::vac_prep(date = vac_date - 1))
  n_new <- n_total_vac - n_vac_yest

  #get number of doses administered in last 7 days
  last_7_day <- subset(gg_data, (date-6) <= gg_data$vac_date & gg_data$vac_date <= date)
  n_7day_vac <- sum(last_7_day$n)

  library(ggplot2)
  library(forcats)

  try_plot <- ggplot2::ggplot(gg_data2, ggplot2::aes(x = vac_date, y = n, fill = fct_rev(Dose))) +
    ggplot2::geom_col()+
    scale_fill_manual(values=c("slategray4", "mediumorchid3","mediumpurple1", "mediumpurple4", "midnightblue","deepskyblue3", "cadetblue2"))+
    labs(fill = "Dose") +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["avg_total"]]),
      color = "red",
      size = 1.25,
      show.legend = FALSE
    )



 # try_plot <- ggplot(gg_data, aes(x = vac_date, y = n, fill = dose)) +
 #    geom_bar(stat = "identity") + theme(legend.position = "none") +
 #   scale_color_manual(values=c("midnightblue","deepskyblue2" ))

  #write.csv(gg_data, "C:/Users/allison.plaxco/Desktop/ggdata.csv")



try_plot %>%
    set_ts_vac_theme() %>%
    add_daily_vac_scale() %>%
    #add_daily_vac_curve() %>%
    add_vac_events(lab_y = 14000L, color = "grey60", size = 3) %>%
    add_daily_vac_label(total = n_total_vac, new7 = n_7day_vac, new = n_new) %>%
    add_daily_vac_axis_labels() %>%
    add_daily_vac_title_caption(date = date, missing = n_missing)
}

#' Prepare Data for Plotting Daily New Cases
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param min_date Minimum plotting date
#'
#' @param date Report date
#'
#' @param delay Number of days to ignore moving average (due to incomplete data)
#'
#' @return A `tibble` with columns `report_date`, `n`, and `avg`
#'
#' @noRd
prep_daily_vac_data <- function(data, min_date, date, delay) {
  data %>%
    dplyr::transmute(
      id = .data[["id"]],
      vac_date = lubridate::ymd(
        .data[["vacc_date"]])
      )%>%
    dplyr::filter(
      {{ min_date }} <= .data[["vac_date"]],
      .data[["vac_date"]] <= {{ date }}
    ) %>%
    dplyr::arrange(.data[["vac_date"]], .data[["id"]]) %>%
    dplyr::distinct(.data[["id"]], .keep_all = TRUE) %>%
    dplyr::count(.data[["vac_date"]]) %>%
    dplyr::arrange(.data[["vac_date"]]) %>%
    tidyr::complete(
      "vac_date" = seq(min_date, date, by = 1L),
      fill = list(n = 0L)
    ) %>%
    timetk::tk_augment_slidify(
      .data[["n"]],
      .period = 7L,
      .f = mean,
      na.rm = TRUE,
      .align = "right",
      .names = "avg"
    ) #%>%
    # dplyr::mutate(
    #   avg = vec_assign(
    #     .data[["avg"]],
    #     i = (NROW(.) - delay + 0L):NROW(.),
    #     value = NA_real_
    #   )
    # )
}

#' Set Theme for Time Series Plots
#'
#' Sets ggplot2 theme using
#' \code{\link[coviData:set_covid_theme]{set_covid_theme()}} and rotates
#' x-axis labels by 45 degrees.
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with adjusted theme
#'
#' @noRd
set_ts_vac_theme <- function(gg_obj) {
  set_covid_theme(gg_obj) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )#+
    #ggplot2::theme(legend.position = "none")
}

#' Add x- and y-axis Scales to Daily Case Plot
#'
#' Adds x-axis scale with monthly breaks using
#' \code{\link[coviData:add_scale_month]{add_scale_month()}} and y-axis scale
#' with breaks by 100.
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with scales set
#'
#' @noRd
add_daily_vac_scale <- function(gg_obj) {

  breaks <- seq(0L, 15000L, by = 2000L)

  label_fn <- rlang::as_function(~ format(.x, big.mark = ","))

  add_scale_month(gg_obj) +
    ggplot2::scale_y_continuous(breaks = breaks, labels = label_fn)
}

#' Add Daily Case Curves to Plot
#'
#' Adds a \code{\link[ggplot2:geom_col]{geom_col()}} curve and a
#' \code{\link[ggplot2:geom_line]{geom_line()}} curve to the plot
#'
#' @param gg_obj A `ggplot` object
#'
#' @param The `ggplot` object with the added geom
#'
#' @noRd
add_daily_vac_curve <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_col(
      fill = "midnightblue",
      width = 1,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["avg"]]),
      color = "darkorange",
      size = 1.25,
      show.legend = FALSE
    )
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
add_daily_vac_label <- function(gg_obj, total, new7, new) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    "Total Vaccinations = ", format(total, big.mark = ","), "\n",
    "Past 7 Day Vaccinations = ", format(new7, big.mark = ","), "\n",
    "New Reported Vaccinations = ", format(new, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = 8500L,
      label = label,
      color = "midnightblue",
      fill = "#f0f0f0",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      label.size = 1
    )
}

#' Add Axis Labels to Daily Case Plot
#'
#' Adds `"Vaccination Date"` x-axis label and `"New Vaccinations"` y-axis label
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with added axis labels
#'
#' @noRd
add_daily_vac_axis_labels <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Vaccination Date", ylab = "New Vaccinations")
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
add_daily_vac_title_caption <- function(gg_obj, date, missing) {

  caption <- paste0(
    "Excludes all vaccinations before 12/15/2020 and additional doses before FDA Approval", "\n",
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






add_vac_events <- function(gg_obj, lab_y, ...) {
  gg_obj  %>%
    add_event(
      "2020-12-26",
      "Face Mask Order 4\nSafer-at-Home",
      lab_y = lab_y,
      vjust = 0.6,
      ...
    ) %>%
    add_event("2021-01-23", "Safer-At-Home Lifted", lab_y = lab_y, ...) %>%
    add_event("2021-02-20", "Broaden Safety Measures", lab_y = lab_y, ...) %>%
    add_event("2021-03-20", "Lift More Restrictions", lab_y = lab_y, ...) %>%
    add_event("2021-04-17", "Emphasize Vaccination", lab_y = lab_y, ...) %>%
    add_event("2021-05-15", "Ease Mask Requirements", lab_y = lab_y, ...)%>%
    add_event("2021-06-12", "Masks Mandatory in Government Buildings/Public Transportation", lab_y = lab_y, ...) %>%
    add_event("2021-07-10", "Mask Recomendations for Unvaccinated People", lab_y = lab_y, ...) %>%
    add_event("2021-08-09", "Masks Mandatory Indoors for Schools", lab_y = lab_y, ...) %>%
    add_event("2021-08-13", "FDA Authorizes Additional Dose", lab_y = lab_y, ...)%>%
    add_event("2021-08-20", "Masks Mandatory for Public Indoor Settings", lab_y = lab_y, ...)%>%
    add_event("2021-10-27", "Masks Mandatory in Schools and Highly Recommended for Public Indoor Settings", lab_y = lab_y, ...) %>%
    add_event("2021-12-01", "Masks Mandatory for Schools Covered by ADA and Highly Recommended in All Other Schools", lab_y = lab_y, ...)%>%
    add_event("2022-03-29", "FDA Authorizes Second Additional Dose, Based on Age and Immune Status", lab_y = lab_y, ...)
}
