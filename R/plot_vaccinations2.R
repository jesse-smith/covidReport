plot_vaccinations2 <- function(
  date = NULL,
  n_first = NULL,
  n_second = NULL,
  n_goal = 656016,
  n_max  = 937166,
  date_updated = date,
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
    date_updated <- vac_date_updated(date)
  }

  shelby_poly %>%
    set_vaccination_count_max(n_max = n_max) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) %>%
    set_covid_theme() %>%
    set_axis_limits(xlim = c(0, 1), ylim = c(0, n_max)) %>%
    add_vaccination_scale() %>%
    add_vaccination_polygon() %>%
    add_vaccination_count_fill2(n_first = n_first, n_second = n_second) %>%
    add_vaccination_goal_marker(n_goal = n_goal, n_max = n_max) %>%
    add_axis_labels(ylab = "Vaccinations") %>%
    add_vaccination_labels2(
      n_goal = n_goal,
      n_first = n_first,
      n_second = n_second
    ) %>%
    add_vaccination_title_caption(date_updated = date_updated, n_goal = n_goal)
}

add_vaccination_count_fill2 <- function(gg_obj, n_first, n_second) {

  # Create fill polygon
  y_partial <- rlang::expr(pmin(.data[["y"]], n_first))

  # Create and assign fill colors
  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)

  fill_color <- pal_indigo(8L)[[8L]]

  gg_obj +
    ggplot2::geom_polygon(ggplot2::aes(y = !!y_partial), fill = fill_color)
}

add_vaccination_labels2 <- function(gg_obj, n_goal, n_first, n_second) {

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

  label <- paste0(
    "1st Doses: ",
    format(n_first, big.mark = ",", scientific = FALSE), "\n",
    "2nd Doses: ",
    format(n_second, big.mark = ",", scientific = FALSE), "\n",
    "(", pct_goal, "% of goal)"
  )

  # Create total label
  # label_pct <- paste0(pct_goal, "% of goal")

  gg_obj +
    ggplot2::annotate(
      "label",
      x = x_pct,
      y = n_first,
      label = label,
      color = color,
      fill = "#f0f0f0",
      label.size = 1,
      vjust = 0,
      fontface = "bold",
      size = 5
    )
    # ggplot2::annotate(
    #   "text",
    #   x = 0L,
    #   y = 850e3L,
    #   label = label_count,
    #   color = "grey30",
    #   fontface = "bold",
    #   size = 5,
    #   hjust = 0
    # )

}
