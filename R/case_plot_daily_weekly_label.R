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
case_plot_daily_week <- function(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL,
  delay = NULL
) {

  min_date <- lubridate::as_date("2020-03-08")

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


  #ped data
  data$calc_age <- active_trans_age(data)
  data_ped <- subset(data, data$calc_age < 18)

  data2 = pos(process_inv(read_inv(date = date-7)))
  data2$calc_age <- active_trans_age(data2)
  data_ped2 <- subset(data2, data2$calc_age < 18)

  # Label numbers
  n_total <- NROW(data)
  n_prev <- NROW(read_inv_id(date = date - 7L))
  n_new <- n_total - n_prev

  #Ped label numbers
  n_total_ped <- NROW(data_ped)
  n_prev_ped <- NROW(data_ped2)
  n_new_ped <- n_total_ped - n_prev_ped

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

  n_plotted <- sum(gg_data[["n"]], na.rm = TRUE)
  n_missing <- n_total - n_plotted

  gg_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["test_date"]], y = .data[["n"]])
    ) %>%
    set_ts_theme() %>%
    add_daily_scale() %>%
    add_daily_curve_ped_all() %>%
    add_covid_events(lab_y = 4000L, color = "grey60", size = 3) %>%
    add_weekly_label(total = n_total, new = n_new)%>%
    add_daily_axis_labels() %>%
    add_daily_title_caption_ped_all(date = date, missing = n_missing)
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
add_weekly_label <- function(gg_obj, total, new) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  label <- paste0(
    "Total Cases = ", format(total, big.mark = ","), "\n",
    "7-Day New Reported Cases = ", format(new, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = 1600L,
      label = label,
      color = "midnightblue",
      fill = "#f0f0f0",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      label.size = 1
    )
}



