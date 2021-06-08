#' Plot Active Case Rates by Age
#'
#' @param data NBS case data, as returned by
#'   \code{\link[coviData:read-nbs]{pos(process_inv())}}
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return A `ggplot`
#'
#' @export
active_plot_age <- function(
  data = pos(process_inv(read_inv(date))),
  date = NULL
) {

  date <- date_inv(date)

  gg_data <- data %>%
    active_calc_age(date = date) %>%
    dplyr::select("age_grp", "rate")

  gg_data %>%
    active_age_ggplot() %>%
    set_covid_theme() %>%
    add_active_age_axis_labels() %>%
    add_active_age_col() %>%
    add_active_age_col_labels() %>%
    remove_x_grid() %>%
    add_active_age_scale() %>%
    add_active_age_title_caption(date = date)
}

active_age_ggplot <- function(data) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[["age_grp"]], y = 1e5 * .data[["rate"]])
  )
}

add_active_age_col <- function(gg_obj) {
  gg_obj + ggplot2::geom_col(fill = "midnightblue")
}

add_active_age_axis_labels <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Age", ylab = "Rate per 100,000 Population")
}

add_active_age_col_labels <- function(gg_obj) {

  y <- gg_obj[["mapping"]][["y"]]

  gg_obj +
    ggplot2::geom_label(
      ggplot2::aes(label = format(round(!!y, digits = 1L), big.mark = ",")),
      color = "#f0f0f0",
      fill  = "midnightblue",
      size  = 4.5,
      vjust = 1,
      label.size = 0
    )
}

add_active_age_scale <- function(gg_obj) {

  y_max <- gg_obj[["mapping"]][["y"]] %>%
    rlang::eval_tidy(data = gg_obj[["data"]]) %>%
    max(na.rm = TRUE)

  magnitude <- floor(log10(y_max))

  scale_decision <- y_max / 10^magnitude

  if (scale_decision < 1.4) {
    scale_by <- 10^(magnitude-1)
  } else if (scale_decision < 3.5) {
    scale_by <- 2*10^(magnitude-1)
  } else if (scale_decision < 7) {
    scale_by <- 5*10^(magnitude-1)
  } else {
    scale_by <- 10^magnitude
  }

  breaks <- seq(0, 2 * y_max, by = scale_by)

  labels <- format(breaks, big.mark = ",")

  gg_obj +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels
    )
}

add_active_age_title_caption <- function(gg_obj, date) {
  sub <-
  cap <- "Data Source: National Electronic Disease Surveillance System (NEDSS)"
  add_title_caption(
    gg_obj,
    title = "Active Cases by Age (per 100k)",
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = cap
  )
}
