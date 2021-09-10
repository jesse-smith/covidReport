#' Plot Daily New Cases by Report Date
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
case_plot_report <- function(
  date = NULL,
  delay = 0
) {


  data <- readxl::read_xlsx("C:/Users/allison.plaxco/Documents/Limited Dataset/status.xlsx",
                                    sheet=1, col_names = TRUE)%>%
    coviData::preprocess()


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

  # Label numbers
  n_total <- NROW(read_inv_id(date = date))
  n_prev <- NROW(read_inv_id(date = date - 1L))
  n_new <- n_total - n_prev

  gg_data <- subset(data, select=c("date", "daily_new_cases"))

  gg_data<-dplyr::rename(gg_data, "n" = "daily_new_cases")%>%
    timetk::tk_augment_slidify(
      .data[["n"]],
      .period = 7L,
      .f = mean,
      na.rm = TRUE,
      .align = "right",
      .names = "avg"
    )

  gg_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["date"]], y = .data[["n"]])
    ) %>%
    set_ts_theme() %>%
    add_daily_scale() %>%
    add_report_curve() %>%
    add_covid_events(lab_y = 1200L, color = "grey32", size = 3.2) %>%
    add_report_label(total = n_total, new = n_new) %>%
    add_report_axis_labels() %>%
    add_report_title_caption(date = date, missing = n_missing)
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
add_report_curve <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_col(
      fill = "deepskyblue3",
      width = 1,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["avg"]]),
      color = "goldenrod1",
      size = 1.25,
      show.legend = FALSE
    )
}

#' Add Label to Daily Case Plot by Report Date
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
add_report_label <- function(gg_obj, total, new) {

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
      y = 800L,
      label = label,
      color = "deepskyblue3",
      fill = "#f0f0f0",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      label.size = 1
    )
}

#' Add Axis Labels to Daily Case Plot by Report date
#'
#' Adds `"Report Date"` x-axis label and `"New Cases"` y-axis label
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The `ggplot` object with added axis labels
#'
#' @noRd
add_report_axis_labels <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Report Date", ylab = "New Cases")
}

#' Add Title, Subtitle, and Caption to Daily Case Plot
#'
#' Adds title `"New COVID-19 Cases by Report Date"`, a subtitle
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
add_report_title_caption <- function(gg_obj, date, missing) {

  caption <- paste0(
    "Data Source: National Electronic Disease Surveillance System (NEDSS)"
  )

  add_title_caption(
    gg_obj,
    title = "New COVID-19 Cases by Report Date",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = caption
  )
}
