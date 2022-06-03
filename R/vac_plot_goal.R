#' Plot Vaccination Totals Figure
#'
#' @param data Vaccination date from \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param date Date of file to pull; defaults to most recent
#'
#' @param n_goal Goal for number of people vaccinated
#'
#' @param n_max Maximum number of people (population size)
#'
#' @return A `ggplot` object
#'
#' @export
vac_plot_goal <- function(
  data = coviData::vac_prep(date = date, distinct = TRUE),
  date = NULL,
  n_goal = 0.7 * n_max,
  n_max = 937166
) {
  # Get counts
  #counts <- vac_count(data)

  data <- data %>%
    dplyr::mutate(
      dose_status = dplyr::case_when(
        is.na(.data[["recip_fully_vacc"]]) ~ "Initiated",
        .data[["recip_fully_vacc"]] == FALSE ~ "Initiated",
        .data[["recip_fully_vacc"]] == TRUE & is.na(.data[["boost_dose1"]]) & is.na(.data[["boost_dose2"]]) ~ "Completed",
        .data[["recip_fully_vacc"]] == TRUE & !is.na(.data[["boost_dose2"]]) ~ "Additional Dose (Multiple)",
        .data[["recip_fully_vacc"]] == TRUE & !is.na(.data[["boost_dose1"]]) ~ "Additional Dose (One)"
      ))

  n_initiated <- sum(data$dose_status == "Initiated", na.rm = TRUE)
  n_completed <- sum(data$dose_status == "Completed", na.rm = TRUE)
  n_additional1 <- sum(data$dose_status == "Additional Dose (One)", na.rm = TRUE)
  n_additional2 <- sum(data$dose_status == "Additional Dose (Multiple)", na.rm = TRUE)

  n_additional12 <- n_additional2 + n_additional1
  n_completed2 <- n_additional2 + n_additional1 + n_completed
  n_initiated2 <- n_additional2 + n_additional1 + n_completed + n_initiated

  n_initiated <- n_initiated2
  n_completed <- n_completed2
  n_additional1 <- n_additional12

  date_updated <- date_vac(date)

  covidReport::shelby_poly %>%
    set_vaccination_count_max(n_max = n_max) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) %>%
    set_covid_theme() %>%
    set_axis_limits(xlim = c(0, 1), ylim = c(0, n_max)) %>%
    add_vaccination_scale() %>%
    add_vaccination_polygon() %>%
    add_vaccination_count_fill(
      n_initiated = n_initiated,
      n_completed = n_completed,
      n_additional1 = n_additional1,
      n_additional2 = n_additional2
    ) %>%
    add_vaccination_goal_marker(n_goal = n_goal, n_max = n_max) %>%
    add_axis_labels(ylab = "People") %>%
    add_vaccination_labels(
      n_initiated = n_initiated,
      n_completed = n_completed,
      n_additional1 = n_additional1,
      n_additional2 = n_additional2,
      n_max = n_max
    ) %>%
    add_vaccination_title_caption(
      date_updated = date_updated,
      n_goal = n_goal,
      n_max = n_max
    )
}

set_vaccination_count_max <- function(.data, n_max) {
  dplyr::mutate(.data, y = n_max * .data[["y"]])
}

add_vaccination_scale <- function(gg_obj) {

  breaks <- seq(0L, 1e6L, 1e5L)

  labels <- paste0(breaks %/% 1e3, "k")

  gg_obj +
    # Hide x scale
    ggplot2::scale_x_continuous(NULL, breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = breaks, labels = labels)
}

add_vaccination_polygon <- function(gg_obj) {
  gg_obj + ggplot2::geom_polygon(fill = "grey83")
}

add_vaccination_count_fill <- function(gg_obj, n_initiated, n_completed, n_additional1, n_additional2) {

  # Create fill polygon
  y_initiated <- rlang::expr(pmin(.data[["y"]], n_initiated))
  y_completed <- rlang::expr(pmin(.data[["y"]], n_completed))
  y_additional1 <- rlang::expr(pmin(.data[["y"]], n_additional1))
  y_additional2 <- rlang::expr(pmin(.data[["y"]], n_additional2))

  # Create and assign fill colors


  gg_obj +
    ggplot2::geom_polygon(ggplot2::aes(y = !!y_initiated), fill = "midnightblue") +
    ggplot2::geom_polygon(ggplot2::aes(y = !!y_completed), fill = "steelblue3")+
    ggplot2::geom_polygon(ggplot2::aes(y = !!y_additional1), fill = "deepskyblue4")+
    ggplot2::geom_polygon(ggplot2::aes(y = !!y_additional2), fill = "slategray4")
}

add_vaccination_goal_marker <- function(gg_obj, n_goal, n_max) {

  segment_range <- covidReport::shelby_poly %>%
    dplyr::filter(dplyr::near(.data[["y"]], n_goal/n_max, tol = 1e-3)) %>%
    dplyr::pull(.data[["x"]]) %>%
    range() %>%
    set_names(c("min", "max"))

  gg_obj +
    ggplot2::annotate(
      "segment",
      x = segment_range[["min"]],
      y = n_goal,
      xend = segment_range[["max"]],
      yend = n_goal,
      color = "goldenrod3",
      size = 1
    ) +
    ggplot2::annotate(
      "text",
      x = segment_range[["min"]],
      y = n_goal,
      label = paste0("Goal (", round(100*n_goal/n_max), "%)"),
      color = "goldenrod3",
      fontface = "bold",
      size = 5,
      hjust = 1,
      vjust = 1/3
    )
}

add_vaccination_labels <- function(
  gg_obj,
  n_initiated,
  n_completed,
  n_additional1,
  n_additional2,
  n_max
) {

  # Colors
  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)
  pal_purple <- ggsci::pal_material("deep-purple", n = 10L, reverse = TRUE)
  indigo <- pal_indigo(6L)[[6L]]
  purple <- pal_purple(2L)[[2L]]

  x_init_pct <- mean(c(
    get_vaccination_label_x_coord(gg_obj, n_initiated, side = "left"),
    get_vaccination_label_x_coord(gg_obj, n_initiated, side = "right")
  ))

  x_comp_pct <- mean(c(
    get_vaccination_label_x_coord(gg_obj, n_completed, side = "left"),
    get_vaccination_label_x_coord(gg_obj, n_completed, side = "right")
  ))

  x_add_pct1 <- mean(c(
    get_vaccination_label_x_coord(gg_obj, n_additional1, side = "left"),
    get_vaccination_label_x_coord(gg_obj, n_additional1, side = "right")
  ))

  x_add_pct2 <- mean(c(
    get_vaccination_label_x_coord(gg_obj, n_additional2, side = "left"),
    get_vaccination_label_x_coord(gg_obj, n_additional2, side = "right")
  ))

  x_both_pct <- c(x_init_pct, x_comp_pct, x_add_pct1, x_add_pct2)

  x_pct <- x_both_pct[which.min(abs(x_both_pct - 0.5))]

  # Get goal percentage
  pct_init <- round(100 * n_initiated / n_max, digits = 1L)
  pct_comp <- round(100 * n_completed / n_max, digits = 1L)
  pct_add1 <- round(100 * n_additional1 / n_max, digits = 1L)
  pct_add2 <- round(100 * n_additional2 / n_max, digits = 1L)

  # Create label text
  label_init <- paste0(
    "Residents Vaccinated (At Least One Dose): ",
    format(n_initiated, big.mark = ",", scientific = FALSE), "\n",
    "(", pct_init, "% of population)"
  )
  label_comp <- paste0(
    "Residents Vaccinated (Completed): ",
    format(n_completed - n_additional1 - n_additional2, big.mark = ",", scientific = FALSE), "\n",
    "(", pct_comp - pct_add1 - pct_add2, "% of population)"
  )
  label_add1 <- paste0(
    "Residents Vaccinated (One Additional Dose): ",
    format(n_additional1 - n_additional2, big.mark = ",", scientific = FALSE), "\n",
    "(", pct_add1 - pct_add2, "% of population)"
  )

  label_add2 <- paste0(
    "Residents Vaccinated (Multiple Additional Dose): ",
    format(n_additional2, big.mark = ",", scientific = FALSE), "\n",
    "(", pct_add2, "% of population)"
  )

  gg_obj +
    ggplot2::annotate(
      "label",
      x = x_pct,
      y = c(n_initiated, n_completed, n_additional1, n_additional2),
      label = c(label_init, label_comp, label_add1, label_add2),
      color = c("midnightblue", "steelblue3", "deepskyblue4", "slategray4"),
      fill = "#f0f0f0",
      label.size = 1,
      vjust = c(0, 1, 1, 0),
      fontface = "bold",
      size = 5
    )
}

add_vaccination_title_caption <- function(gg_obj, date_updated, n_goal, n_max) {

  n_chr   <- format(round(n_goal), scientific = FALSE, big.mark = ",")

  pct_chr <- scales::percent(n_goal / n_max)

  caption <- paste0(
    "Vaccination goal is ", n_chr, " people ",
    "(~", pct_chr, " of the Shelby County population)\n",
    "Data Source: Tennessee Immunization Information System (TennIIS)"
  )

  add_title_caption(
    gg_obj,
    title = "Shelby County Vaccination Goal",
    subtitle = format(as.Date(date_updated), "%B %d, %Y"),
    caption = caption
  )
}

get_vaccination_label_x_coord <- function(
  gg_obj,
  n,
  side = c("left", "right")
) {

  side <- rlang::arg_match(side)[[1L]]

  choose_side <- if (side == "left") base::min else base::max

  # Get observations with two closest y coordinates
  top_2 <- gg_obj[["data"]] %>%
    dplyr::mutate(n_dist = abs(n - .data[["y"]])) %>%
    dplyr::arrange(.data[["n_dist"]]) %>%
    dplyr::slice_head(n = 2L)

  # Get closest y observation
  closest_y <- top_2 %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::pull(.data[["y"]])

  # If `closest_y` is almost equal to `n`, use the associated x coordinate
  # Otherwise, use the x coordinate further to the chosen `side`
  if (dplyr::near(closest_y, n)) {
    top_2 %>%
      dplyr::slice_head(n = 1L) %>%
      dplyr::pull(.data[["x"]])
  } else {
    top_2 %>%
      dplyr::filter(.data[["x"]] == choose_side(.data[["x"]])) %>%
      dplyr::pull(.data[["x"]])
  }
}
