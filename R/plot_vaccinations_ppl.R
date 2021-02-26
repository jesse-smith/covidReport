plot_vaccinations_ppl <- function(
  date = NULL,
  n_partial = NULL,
  n_full = NULL,
  n_goal = 656016,
  n_max  = 937166,
  date_updated = date,
  resident_only = TRUE
) {

  any_null <- any(is.null(n_partial), is.null(n_full))

  # Get vaccination counts
  if (any_null) {
    vaccinations <- coviData::vac_load(date = date) %>%
      coviData::vac_prep(distinct = TRUE) %>%
      vac_count(resident_only = resident_only)
  }

  if (is.null(n_partial)) {
    n_partial <- vaccinations %>%
      dplyr::filter(dose_count == 1L) %>%
      dplyr::pull(n)
  }

  if (is.null(n_full)) {
    n_full <- vaccinations %>%
      dplyr::filter(dose_count == 2L) %>%
      dplyr::pull(n)
  }

  if (is.null(date_updated)) {
    date_updated <- vac_date(
      date,
      resident_only = resident_only
    )
  }

  covidReport::shelby_poly %>%
    set_vaccination_count_max(n_max = n_max) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) %>%
    set_covid_theme() %>%
    set_axis_limits(xlim = c(0, 1), ylim = c(0, n_max)) %>%
    add_vaccination_scale() %>%
    add_vaccination_polygon() %>%
    add_vaccination_count_fill_ppl(n_partial = n_partial, n_full = n_full) %>%
    add_vaccination_goal_marker(n_goal = n_goal, n_max = n_max) %>%
    add_axis_labels(ylab = "Vaccinations") %>%
    add_vaccination_labels_ppl(
      n_goal = n_goal,
      n_partial = n_partial,
      n_full = n_full
    ) %>%
    add_vaccination_title_caption(date_updated = date_updated, n_goal = n_goal)
}

add_vaccination_count_fill_ppl <- function(gg_obj, n_partial, n_full) {

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



add_vaccination_labels_ppl <- function(gg_obj, n_goal, n_partial, n_full) {

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
