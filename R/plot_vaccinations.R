#' Plot Vaccination Totals Figure
#'
#' @param data Vaccination date from \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param date Date of file to pull; defaults to most recent
#'
#' @param n_first Number of first doses given
#'
#' @param n_second Number of second doses given
#'
#' @param n_goal Goal for number of people vaccinated
#'
#' @param n_max Maximum number of people (population size)
#'
#' @param date_updated The date to use for the figure subtitle; defaults to last
#'   updated date in the file
#'
#' @param resident_only Should the figure only count Shelby County residents?
#'
#' @return A `ggplot` object
#'
#' @export
plot_vaccinations <- function(
  data = coviData::vac_prep(coviData::vac_load(date = date)),
  date = NULL,
  n_first = NULL,
  n_second = NULL,
  n_goal = 700000,
  n_max  = 937166,
  date_updated = NULL,
  resident_only = TRUE
) {

  any_null <- any(is.null(n_first), is.null(n_second))

  # Get vaccination counts
  if (any_null) {
    vaccinations <- coviData::vac_load(date = date) %>%
      coviData::vac_prep(distinct = FALSE) %>%
      vac_count(resident_only = resident_only)
  }

  if (is.null(n_first)) {
    n_first <- vaccinations %>%
      dplyr::filter(dose_count == 1L) %>%
      dplyr::pull(n)
  }

  if (is.null(n_second)) {
    n_second <- vaccinations %>%
      dplyr::filter(dose_count == 2L) %>%
      dplyr::pull(n)
  }

  if (is.null(date_updated)) {
    date_updated <- vac_date(date, resident_only = resident_only)
  }

  covidReport::shelby_poly %>%
    set_vaccination_count_max(n_max = n_max) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) %>%
    set_covid_theme() %>%
    set_axis_limits(xlim = c(0, 1), ylim = c(0, n_max)) %>%
    add_vaccination_scale() %>%
    add_vaccination_polygon() %>%
    add_vaccination_count_fill(n_first = n_first, n_second = n_second) %>%
    add_vaccination_goal_marker(n_goal = n_goal, n_max = n_max) %>%
    add_axis_labels(ylab = "Vaccinations (1st Doses)") %>%
    add_vaccination_labels(
      n_goal = n_goal,
      n_first = n_first,
      n_second = n_second
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

add_vaccination_count_fill <- function(gg_obj, n_first, n_second) {

  # Create fill polygon
  y_partial <- rlang::expr(pmin(.data[["y"]], n_first))

  # Create and assign fill colors
  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)

  fill_color <- pal_indigo(8L)[[8L]]

  gg_obj +
    ggplot2::geom_polygon(ggplot2::aes(y = !!y_partial), fill = fill_color)
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
      label = "Goal",
      color = "goldenrod3",
      fontface = "bold",
      size = 5,
      hjust = 1,
      vjust = 1/3
    )
}

add_vaccination_labels <- function(gg_obj, n_goal, n_first, n_second) {

  # Create and assign colors
  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)

  color <- pal_indigo(8L)[[8L]]

  x_pct <- mean(c(
    get_vaccination_label_x_coord(gg_obj, n_first, side = "left"),
    get_vaccination_label_x_coord(gg_obj, n_first, side = "right")
  ))

  # Get goal percentage
  pct_goal <- round(100*n_first / n_goal, digits = 1L)

  # Create label text

  label_count <- paste0(
    "1st Doses: ",
    format(n_first, big.mark = ",", scientific = FALSE), "\n",
    "2nd Doses: ",
    format(n_second, big.mark = ",", scientific = FALSE), "\n",
    "(", pct_goal, "% of goal)"
  )

  label_pct <- paste0("(", pct_goal, "% of goal)")

  # Create total label
  # label_pct <- paste0(pct_goal, "% of goal")

  gg_obj +
    ggplot2::annotate(
      "label",
      x = x_pct,
      y = n_first,
      label = label_count,
      color = color,
      fill = "#f0f0f0",
      label.size = 1,
      vjust = 0,
      fontface = "bold",
      size = 5
    )
    # ggplot2::annotate(
    #   "label",
    #   x = x_pct,
    #   y = n_first,
    #   label = label_pct,
    #   color = "goldenrod3",
    #   fill = "#f0f0f0",
    #   label.size = 0,
    #   vjust = 0,
    #   fontface = "bold",
    #   size = 5
    # )
}

add_vaccination_title_caption <- function(gg_obj, date_updated, n_goal, n_max) {

  n_chr   <- format(n_goal, scientific = FALSE, big.mark = ",")

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
