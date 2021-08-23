#' Plot Daily New Cases by Specimen Collection Date
#'
#' @param data Case data, as output by
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
case_plot_daily_ped_only <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL,
  delay = NULL
) {




# Date for current (and previous) counts
  date <- date_inv(date)
#current day's peds data
  data$calc_age <- active_trans_age(data)
  data_ped <- subset(data, data$calc_age < 18)
#previous day's peds data
  data2 = pos(process_inv(read_inv(date = date-1)))
  data2$calc_age <- active_trans_age(data2)
  data_ped2 <- subset(data2, data2$calc_age < 18)


  min_date <- lubridate::as_date("2020-03-08")



  if (is.null(delay)) {
    rpt_data <- dplyr::as_tibble(coviData::load_report_date())

    complete_date <- data_ped %>%
      dplyr::left_join(rpt_data, by = "inv_local_id") %>%
      dplyr::mutate(
        collection_date = lubridate::as_date(.data[["collection_date"]])
      ) %>%
      covidModel::estimate_delay(today = date) %>%
      dplyr::pull("collection_date")

    delay <- date - complete_date
  }

  # Label numbers
  n_total_ped <- NROW(data_ped)
  n_prev_ped <- NROW(data_ped2)
  n_new_ped <- n_total_ped - n_prev_ped

  gg_data <- prep_daily_data(
    data_ped,
    min_date = min_date,
    date = date,
    delay = delay
  )

  n_plotted <- sum(gg_data[["n"]], na.rm = TRUE)
  n_missing <- n_total_ped - n_plotted

  gg_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["test_date"]], y = .data[["n"]])
    ) %>%
    set_ts_theme() %>%
    add_daily_scale() %>%
    add_daily_curve_ped() %>%
    add_covid_events(lab_y = 500L, color = "grey60", size = 3) %>%
    add_daily_label_ped(total = n_total_ped, new = n_new_ped) %>%
    add_daily_axis_labels() %>%
    add_daily_title_caption_ped(date = date, missing = n_missing)
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
add_daily_curve_ped <- function(gg_obj) {
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
add_daily_label_ped <- function(gg_obj, total, new) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    "Total Cases = ", format(total, big.mark = ","), "\n",
    " New Reported Cases = ", format(new, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = 300L,
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
add_daily_title_caption_ped <- function(gg_obj, date, missing) {

  caption <- paste0(
    "Excludes cases with missing specimen collection dates ",
    "(N = ", format(missing, big.mark = ","), ")\n",
    "Data Source: National Electronic Disease Surveillance System (NEDSS)"
  )

  add_title_caption(
    gg_obj,
    title = "New Pediatric COVID-19 Cases by Specimen Collection Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = caption
  )
}
