plot_cumulative <- function(.data, .col = "report_date") {

  col_nm <- select_colnames(.data, .col)

  assert_cols(.data, col_nm, ptype = lubridate::Date(), n = 1L)

  gg_data <- count_daily(.data, col_nm)

  lab_y <- max(gg_data[["n"]], na.rm = TRUE)

  gg_data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[col_nm]], y = .data[["n"]])) %>%
    set_covid_theme() %>%
    add_cumulative_scale() %>%
    add_cumulative_curve() %>%
    add_cumulative_label() %>%
    add_cumulative_axis_labels() %>%
    add_cumulative_title_caption()

}

add_cumulative_scale <- function(gg_obj) {

  breaks <- seq(0L, 1e6L, by = 1e4L)

  labels <- breaks %>% divide_by(1e3L) %>% paste0("k")

  add_scale_month(gg_obj) +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels
    )
}

add_cumulative_curve <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_col(
      fill = "midnightblue",
      width = 1,
      show.legend = FALSE
    )
}

add_cumulative_label <- function(gg_obj) {

  x <- gg_var(gg_obj, "x")

  y <- gg_var(gg_obj, "y")

  min_date <- min(gg_obj[["data"]][[x]], na.rm = TRUE)

  total_cases <- max(gg_obj[["data"]][[y]], na.rm = TRUE)

  new_cases <- gg_obj[["data"]][[y]] %>% diff() %>% vec_slice(i = vec_size(.))

  label <- paste0(
    "Total Cases = ", format(total_cases, big.mark = ","), "\n",
    " New Cases = ", format(new_cases, big.mark = ",")
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = min_date,
      y = total_cases,
      label = label,
      color = "midnightblue",
      fill = "#f0f0f0",
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      label.size = 1
    )
}

add_cumulative_axis_labels <- function(gg_obj) {
  add_axis_labels(gg_obj, xlab = "Report Date", ylab = "Cumulative Cases")
}

add_cumulative_title_caption <- function(gg_obj) {
  add_title_caption(
    gg_obj,
    title = "Cumulative COVID-19 Cases by Report Date",
    subtitle = format(Sys.Date(), "%B %d, %Y")
  )
}
