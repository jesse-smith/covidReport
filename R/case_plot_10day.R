#' Plot Daily New Cases by Specimen Collection Date in the last 10 days
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
case_plot_10day <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL,
  delay = NULL
) {


  # Date for current (and previous) counts
  date <- date_inv(date)


  min_date <- lubridate::as_date("2020-03-08")

  start_date <- date - 10



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


  #ped data

  gg_data <- prep_daily_data(
    data,
    min_date = min_date,
    date = date,
    delay = delay
  )





  gg_data <- subset(gg_data, test_date >= start_date)

  # Label numbers
  n_total <- NROW(data)

  n_prev <- NROW(read_inv_id(date = date - 1L))
  n_new <- n_total - n_prev


  n_total_10 <- sum(gg_data[["n"]], na.rm = TRUE)


  gg_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["test_date"]], y = .data[["n"]])
    ) %>%
    set_ts_theme() %>%
    add_recent_scale(data = gg_data) %>%
    add_10day_curve() %>%
    add_daily_axis_labels() %>%
    add_10day_title(total_10 = n_total_10, new = n_new, date = date)
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
add_10day_curve <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_col(
      fill = "midnightblue",
      width = 1,
      show.legend = FALSE
    )
}




#' Add Title, Subtitle, and Caption to Daily Case Plot
#'
#' Adds title `"10-Day COVID-19 Cases by Specimen Collection Date"`, a subtitle
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
add_10day_title <- function(gg_obj, total_10, new, date) {

  caption <- paste0(
    "10-Day Cases by Specimen Collection Date = ", format(total_10, big.mark = ","), ", ",
    " New Reported Cases = ", format(new, big.mark = ","), "\n",
    "Data Source: National Electronic Disease Surveillance System (NEDSS)\n"
  )

  add_title_caption(
    gg_obj,
    title = "10-Day COVID-19 Cases by Specimen Collection Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = caption
  )
}


