#' Plot 30 day avg number of cases resulting in death by Specimen Collection Date
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param date The report date of the data; defaults to the most recent date
#'
#' @param delay Number of days to ignore in 30 day average (due to incomplete data);
#'   default is calculated using
#'   \code{\link[covidModel:estimate_delay]{estimate_delay()}}
#'
#' @return A `ggplot` object
#'
#' @export
death_plot_daily_30 <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL,
  delay = 34
) {

  min_date <- lubridate::as_date("2020-03-08")

  deaths <- filter_deaths(data)

  # Date for current (and previous) counts
  date <- date_inv(date)

  if (is.null(delay)) {
    rpt_data <- dplyr::as_tibble(coviData::load_report_date())

    complete_date <- data %>%
      dplyr::left_join(rpt_data, by = "inv_local_id") %>%
      dplyr::mutate(
        collection_date = lubridate::as_date(.data[["collection_date"]])
      ) %>%
      covidModel::estimate_delay(today = date) %>%
      dplyr::pull("collection_date")

    delay <- date - complete_date
  }

  # Label numbers
   n_total_case <- NROW(data)
   n_total_death <- NROW(deaths)
  # n_prev <- NROW(read_inv_id(date = date - 1L))
  # n_new <- n_total - n_prev

  gg_data_case <- prep_daily_data(
    data,
    min_date = min_date,
    date = date,
    delay = delay
  )


  gg_data_death <- prep_daily_data_death_scd(
    deaths,
    min_date = min_date,
    date = date,
    delay = delay
  )

  gg_data_death <- dplyr::rename(gg_data_death, n_death = n, avg_death = avg)
  gg_data <- dplyr::full_join(gg_data_case, gg_data_death)

  #daily percent of deaths out of cases by SpColDate
  gg_data$percent_death_over_case <- (gg_data$n_death/gg_data$n)*100

  #cut the graph off according to the delay
  gg_data<-subset(gg_data, test_date < Sys.Date()-(delay-1))

  #creates a rolling 30 day avg percent of deaths out of cases by SpColDate
  gg_data2 <- gg_data %>% timetk::tk_augment_slidify(
    .data[["percent_death_over_case"]],
    .period = 30L,
    .f = mean,
    na.rm = TRUE,
    .align = "right",
    .names = "30_avg_death_percent"
  )

  n_plotted_case <- sum(gg_data[["n"]], na.rm = TRUE)
  n_missing_case <- n_total_case - n_plotted_case

  n_plotted_death <- sum(gg_data[["n_death"]], na.rm = TRUE)
  n_missing_death <- n_total_death - n_plotted_death

  gg_data2 %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["test_date"]], y = .data[["n"]])
    ) %>%
    set_ts_theme() %>%
    add_daily_scale_death_percent() %>%
    add_daily_curve_death_30() %>%
    add_covid_events(lab_y = 6L, color = "grey60", size = 3) %>%
    add_daily_label_death_30(deaths = n_total_death) %>%
    add_daily_label_case_30(cases = n_total_case) %>%
    add_daily_axis_labels_death_scd() %>%
    add_daily_title_caption_death_percent(date = date, missing = n_missing_death)
}


#' Prepare Data for Plotting Deaths by Specimen Collection Date
#'
#' @param data Death data, as output by
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
prep_daily_data_death_scd <- function(deaths, min_date, date, delay) {
  deaths %>%
    dplyr::transmute(
      id = .data[["inv_local_id"]],
      test_date = coviData::std_dates(
        .data[["specimen_coll_dt"]],
        orders = c("ymdT", "ymdHM", "ymd"),
        train = FALSE,
        force = "dt"
      )
    ) %>%
    dplyr::filter(
      {{ min_date }} <= .data[["test_date"]],
      .data[["test_date"]] <= {{ date }}
    ) %>%
    dplyr::arrange(.data[["test_date"]], .data[["id"]]) %>%
    dplyr::distinct(.data[["id"]], .keep_all = TRUE) %>%
    dplyr::count(.data[["test_date"]]) %>%
    dplyr::arrange(.data[["test_date"]]) %>%
    tidyr::complete(
      "test_date" = seq(min_date, date, by = 1L),
      fill = list(n = 0L)
    ) %>%
    timetk::tk_augment_slidify(
      .data[["n"]],
      .period = 7L,
      .f = mean,
      na.rm = TRUE,
      .align = "right",
      .names = "avg"
    ) %>%
    dplyr::mutate(
      avg = vec_assign(
        .data[["avg"]],
        i = (NROW(.) - delay + 1L):NROW(.),
        value = NA_real_
      )
    )
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
add_daily_scale_death_percent <- function(gg_obj) {

  breaks <- seq(0L, 6L, by = 1L)

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

add_daily_curve_death_30 <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["30_avg_death_percent"]]),
      color = "firebrick4",
      size = 1.25,
      show.legend = FALSE
    )
}



# add_daily_curve_death_percent <- function(gg_obj) {
#   gg_obj +
#     ggplot2::geom_line(
#       ggplot2::aes(y = .data[["percent_death_over_case"]]),
#       fill = "green",
#       width = 1,
#       show.legend = FALSE
#     )
# }

#' Add Death Label to Plot
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
add_daily_label_death_30 <- function(gg_obj, deaths) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    " Total Deaths = ", format(deaths, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = 1.75L,
      label = label,
      color = "firebrick4",
      fill = "#f0f0f0",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      label.size = 1
    )
}


#' Add Case Label to Plot
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
add_daily_label_case_30 <- function(gg_obj, cases) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    "Total Cases = ", format(cases, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = 2L,
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
#' Adds `"Specimen Collection Date"` x-axis label and `"New Cases"` y-axis label
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with added axis labels
#'
#' @noRd
add_daily_axis_labels_death_scd <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Specimen Collection Date", ylab = "% Cases Resulting in Death")
}

#' Add Title, Subtitle, and Caption to Daily Case Plot
#'
#' Adds title `"New COVID-19 Cases by Specimen Collection Date"`, a subtitle
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
add_daily_title_caption_death_percent <- function(gg_obj, date, missing) {

  caption <- paste0(
    "Excludes cases and deaths with missing specimen collection dates ","\n",
    "Specimen collection dates in the past 34 days are excluded","\n",
    "Data Source: National Electronic Disease Surveillance System (NEDSS)"
  )

  add_title_caption(
    gg_obj,
    title = paste0("30-Day Rolling Average of COVID-19 Deaths as a Percent of Cases", "\n",
    "by Specimen Collection Date"),
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = caption
  )
}
