#' Plot Active Cases by a Categorical Variable
#'
#' @param data Data from an `active_calc_*()` function
#'
#' @param grp The grouping variable
#'
#' @return A `ggplot`
#'
#' @keywords internal
active_plot_ <- function(
  data,
  grp = c("age", "sex", "race", "ethnicity"),
  date = NULL
) {
  grp <- rlang::arg_match(grp)[[1L]]

  data %>%
    dplyr::select("grp", "rate") %>%
    dplyr::filter(as.character(.data[["grp"]]) != "Missing") %>%
    dplyr::mutate(grp = forcats::fct_drop(.data[["grp"]], "Missing")) %>%
    active_ggplot_() %>%
    set_covid_theme() %>%
    add_active_axis_labels_(grp = grp) %>%
    add_active_col_() %>%
    add_active_col_labels_() %>%
    remove_x_grid() %>%
    add_active_scale_() %>%
    add_active_title_caption_(grp = grp, date = date)
}


active_ggplot_ <- function(data) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[["grp"]], y = 1e5 * .data[["rate"]])
  )
}

add_active_axis_labels_ <- function(gg_obj, grp) {
  Grp <- stringr::str_to_title(grp)
  add_axis_labels(gg_obj, xlab = Grp, ylab = "Rate per 100,000 Population")
}

add_active_col_ <- function(gg_obj) {
  gg_obj + ggplot2::geom_col(fill = "midnightblue")
}

add_active_col_labels_ <- function(gg_obj) {

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

add_active_scale_ <- function(gg_obj) {

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

add_active_title_caption_ <- function(gg_obj, grp, date) {
  Grp <- stringr::str_to_title(grp)
  cap <- "Data Source: National Electronic Disease Surveillance System (NEDSS)"
  add_title_caption(
    gg_obj,
    title = paste("Active Cases by", Grp, "(per 100k)"),
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = cap
  )
}
