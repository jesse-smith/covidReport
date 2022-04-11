#' Plot Daily New Cases by Specimen Collection Date in the last 30 days
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
case_plot_daily_recent <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL,
  delay = NULL
) {


  # Date for current (and previous) counts
  date <- date_inv(date)


  min_date <- lubridate::as_date("2020-03-08")

  start_date <- date - 30



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
  data$calc_age <- active_trans_age(data)
  data_ped <- subset(data, data$calc_age < 18)

  data2 = pos(process_inv(read_inv(date = date-1)))
  data2$calc_age <- active_trans_age(data2)
  data_ped2 <- subset(data2, data2$calc_age < 18)

  gg_data_all <- prep_daily_data(
    data,
    min_date = min_date,
    date = date,
    delay = delay
  )

  gg_data_ped <- prep_daily_data_ped(
    data,
    min_date = min_date,
    date = date,
    delay = delay
  )

  gg_data_ped <- dplyr::rename(gg_data_ped, n_ped = n, avg_ped = avg)

  gg_data <- dplyr::full_join(gg_data_all, gg_data_ped)

  gg_data <- subset(gg_data, test_date >= start_date)

  # Label numbers
  n_total <- NROW(data)

  n_prev <- NROW(read_inv_id(date = date - 1L))
  n_new <- n_total - n_prev


  n_total_30 <- sum(gg_data[["n"]], na.rm = TRUE)


  gg_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["test_date"]], y = .data[["n"]])
    ) %>%
    set_ts_theme() %>%
    add_recent_scale(data = gg_data) %>%
    add_daily_curve_ped_all() %>%
    add_daily_axis_labels() %>%
    add_recent_title(total_30 = n_total_30, new = n_new, date = date)
}





#' Add Title, Subtitle, and Caption to Daily and Ped Case Plot
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
add_recent_title <- function(gg_obj, total_30, new, date) {

  caption <- paste0(
    "30-Day Cases by Specimen Collection Date = ", format(total_30, big.mark = ","), ", ",
    " New Reported Cases = ", format(new, big.mark = ","), "\n",
    "Data Source: National Electronic Disease Surveillance System (NEDSS)\n",
    "Note: Pediatric Cases Denoted in Red"
  )

  add_title_caption(
    gg_obj,
    title = "30-Day COVID-19 Cases by Specimen Collection Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = caption
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
add_recent_scale <- function(gg_obj, data = gg_data) {

  breaks <- seq(0L, plyr::round_any(max(data$n), 10), by = plyr::round_any(max(data$n), 10)/10)

  label_fn <- rlang::as_function(~ format(.x, big.mark = ","))

  gg_obj +
    ggplot2::scale_y_continuous(breaks = breaks, label = label_fn)
}


