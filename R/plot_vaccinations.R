plot_vaccinations <- function(
  .data = NULL,
  n_partial = NULL,
  n_full = NULL,
  n_goal = 656600,
  n_max  = 937166,
  date_updated = NULL
) {

  if (is.null(n_partial)) {
    # Get `n_partial` from `.data`
  }

  if (is.null(n_full)) {
    # Get `n_full` from `.data`
  }

  shelby_poly %>%
    set_vaccination_count_max(n_max) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) %>%
    set_covid_theme() %>%
    set_axis_limits(xlim = c(0, 1), ylim = c(0, n_max)) %>%
    add_vaccination_scale() %>%
    add_vaccination_polygon() %>%
    add_vaccination_count_fill(n_partial = n_partial, n_full = n_full) %>%
    add_vaccination_goal_marker(n_goal = n_goal) %>%
    add_axis_labels(ylab = "Vaccinations") %>%
    add_vaccination_labels(
      n_goal = n_goal,
      n_partial = n_partial,
      n_full = n_full
    ) %>%
    add_vaccination_title_caption(date_updated = date_updated)
}

set_vaccination_count_max <- function(.data, n_max) {
  dplyr::mutate(.data, y = n_max * .data[["y"]])
}

add_vaccination_polygon <- function(gg_obj) {
  gg_obj + ggplot2::geom_polygon(fill = "grey83")
}

add_vaccination_count_fill <- function(gg_obj, n_partial, n_full) {

  # Create fill polygons
  y_partial <- rlang::expr(
    .data[["y"]] %>%
      # Truncate top to total vaccinations
      pmin(n_partial + n_full) %>%
      # Truncate bottom to full vaccinations
      pmax(n_full)
  )
  y_full <- rlang::expr(pmin(.data[["y"]], n_full))

  # Create and assign fill colors
  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)

  partial_color <- pal_indigo(8L)[[8L]]
  full_color <- pal_indigo(1L)[[1L]]

  gg_obj +
    # Partial
    ggplot2::geom_polygon(
      ggplot2::aes(y = !!y_partial),
      fill = partial_color
    ) +
    # Full
    ggplot2::geom_polygon(
      ggplot2::aes(y = !!y_full),
      fill = full_color
    )
}

add_vaccination_scale <- function(gg_obj) {

  breaks <- seq(0L, 1e6L, 1e5L)

  labels <- paste0(breaks %/% 1e3, "k")

  gg_obj +
    # Hide x scale
    ggplot2::scale_x_continuous(NULL, breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = breaks, labels = labels)
}

add_vaccination_goal_marker <- function(gg_obj, n_goal) {
  gg_obj +
    ggplot2::geom_hline(yintercept = n_goal, color = "goldenrod", size = 1) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = n_goal * 1.01,
      label = "Goal",
      color = "goldenrod",
      fontface = "bold",
      size = 5,
      hjust = 0,
      vjust = 0
    )
}

add_vaccination_labels <- function(gg_obj, n_goal, n_partial, n_full) {

  # Create and assign colors
  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)
  color_full <- pal_indigo(1L)[[1L]]
  color_partial <- pal_indigo(8L)[[8L]]

  # Get x coordinates
  x_partial <- get_vaccination_label_x_coord(
    gg_obj,
    n = n_partial + n_full,
    side = "right"
  )
  x_full <- get_vaccination_label_x_coord(gg_obj, n = n_full, side = "left")

  x_total <- mean(c(
    get_vaccination_label_x_coord(gg_obj, n_partial + n_full, side = "left"),
    get_vaccination_label_x_coord(gg_obj, n_partial + n_full, side = "right")
  ))

  # Get goal percentage
  pct_goal <- round(100*(n_partial + n_full) / n_goal, digits = 1L)

  # Create label text

  label_partial <- paste0(
    "Partially Vaccinated: ",
    format(n_partial, big.mark = ",", scientific = FALSE)
  )

  label_full <- paste0(
    "Fully Vaccinated: ",
    format(n_full, big.mark = ",", scientific = FALSE)
  )

  if (n_full == 0L) {

    # Create total label (just partially vaccinated in this case)
    label_total <- paste0(
      "Partially Vaccinated\n",
      format(n_partial + n_full, big.mark = ",", scientific = FALSE), "\n",
      " (", pct_goal, "% of goal)"
    )

    gg_obj +
      ggplot2::annotate(
        "label",
        x = x_total,
        y = n_partial + n_full,
        label = label_total,
        color = color_partial,
        fill = "#f0f0f0",
        label.size = 1,
        vjust = 0,
        fontface = "bold",
        size = 5
      )
  } else if (n_partial == 0) {
    # Create total label
    label_total <- paste0(
      "Fully Vaccinated\n",
      format(n_partial + n_full, big.mark = ",", scientific = FALSE), "\n",
      " (", pct_goal, "% of goal)"
    )

    gg_obj +
      ggplot2::annotate(
        "label",
        x = x_total,
        y = n_partial + n_full,
        label = label_total,
        color = color_full,
        fill = "#f0f0f0",
        label.size = 1,
        vjust = 0,
        fontface = "bold",
        size = 5
      )


  } else {
    # Create total label
    label_total <- paste0(
      "Total Vaccinated\n",
      format(n_partial + n_full, big.mark = ",", scientific = FALSE), "\n",
      " (", pct_goal, "% of goal)"
    )

    gg_obj +
      ggplot2::annotate(
        "label",
        x = x_total,
        y = n_partial + n_full,
        label = label_total,
        color = "grey30",
        fill = "#f0f0f0",
        label.size = 1,
        vjust = 0,
        fontface = "bold",
        size = 5
      ) +
      ggplot2::annotate(
        "label",
        x = x_partial,
        y = n_partial + n_full,
        label = label_partial,
        color = color_partial,
        fill = "#f0f0f0",
        hjust = 1,
        vjust = 0
      ) +
      ggplot2::annotate(
        "label",
        x = x_full,
        y = n_full,
        label = label_full,
        color = color_full,
        fill = "#f0f0f0",
        hjust = 0,
        vjust = 0
      )
  }

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

add_vaccination_title_caption <- function(gg_obj, date_updated) {

  caption <- paste0(
    "Vaccination goal is 656,600 people, ",
    "or ~70% of the Shelby County population\n",
    "Data Source: Tennessee Immunization Information System (TennIIS)"
  )

  add_title_caption(
    gg_obj,
    title = "Shelby County Vaccinations",
    subtitle = format(as.Date(date_updated), "%B %d, %Y"),
    caption = caption
  )
}
