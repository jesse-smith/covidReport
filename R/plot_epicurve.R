plot_epicurve <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date(),
  iterations = ceiling(1e4 / (1 - burn)),
  burn = 0.2
) {

  # Get character variables of column names
  collect_nm <- select_colnames(.data, .collection_date)
  report_nm <- select_colnames(.data, .report_date)

  # Check and remove inconsistencies in linelist data
  checked_data <- check_linelist_dates(
    .data,
    .collection_date = collect_nm,
    .report_date = report_nm,
    today = today
  )

  # Calculate number missing for caption
  n_missing <- vec_size(.data) - vec_size(checked_data)

  # Get data ready for plotting
  gg_data <- checked_data %>%
    count_epicurve(
      .collection_date = collect_nm,
      .report_date = report_nm,
      today = today,
      iterations = iterations,
      burn = burn
    ) %>%
    average_epicurve()

  # Remove linelist to save space
  remove(.data)

  # Create chart - function names should be self-explanatory
  gg_data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[".t"]])) %>%
    set_covid_theme() %>%
    add_epicurve_scale() %>%
    add_epicurve_count_cols() %>%
    add_epicurve_avg_line() %>%
    add_epicurve_events() %>%
    add_epicurve_label() %>%
    add_epicurve_title_caption(n_missing)

}

add_epicurve_scale <- function(gg_obj) {

  # Create y-axis reference points
  breaks <- seq(0L, 1e4L, by = 1e2L)

  # Format y-axis labels with commas
  labels <- format(breaks, big.mark = ",")

  # Add axis scales
  add_scale_month(gg_obj) +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels
    )
}

count_epicurve <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date(),
  iterations = ceiling(0.5 * 1e4 / (1 - burn)),
  burn = 0.2
) {

  # Get names of selected columns
  collect_nm <- select_colnames(.data, .collection_date)
  report_nm <- select_colnames(.data, .report_date)

  # Count daily new cases by collection date
  collection_counts <- .data %>%
    count_daily(collect_nm) %>%
    dplyr::transmute(.t = .data[[collect_nm]], collected = .data[["n"]])

  # Count newly reported cases by collection date
  new_counts <- .data %>%
    count_new_daily(collect_nm, report_nm, today = today) %>%
    dplyr::transmute(.t = .data[[collect_nm]], new = .data[["n"]])

  # Nowcast cases up to current date (given by `today`)
  predicted_counts <- covidModel::nowcast_cases(
    .data,
    .collection_date,
    .report_date,
    today = today,
    iterations = iterations,
    burn = burn
  ) %>%
    dplyr::filter(.data[["predicted"]]) %>%
    dplyr::select(.t = collect_nm, predicted = "cases")

  # Combine results and ensure all are integer
  dplyr::full_join(
    collection_counts,
    new_counts,
    by = ".t"
  ) %>%
    dplyr::left_join(
      predicted_counts,
      by = ".t"
    ) %>%
    dplyr::mutate(
      dplyr::across(c("collected", "new", "predicted"), as.integer)
    ) %>%
    tidyr::replace_na(list(collected = 0L, new = 0L))
}

average_epicurve <- function(
  .data,
  .collected = "collected",
  .predicted = "predicted",
  .t = ".t"
) {

  # Get names of selected columns
  collect_nm <- select_colnames(.data, .collected)
  predict_nm <- select_colnames(.data, .predicted)
  t_nm <- select_colnames(.data, .t)

  # Calculate 7-day average
  .data %>%
    # Get maximum of predicted and observed values for use in averaging where
    # observations are incomplete
    dplyr::mutate(
      counts = .data[[predict_nm]] %>%
        tidyr::replace_na(0L) %>%
        pmax(.data[[collect_nm]])
    ) %>%
    # Calculate 7-day rolling average
    timetk::tk_augment_slidify(
      .value = "counts",
      .period = 7L,
      ~ mean(.x, na.rm = TRUE),
      .align = "right",
      .partial = TRUE,
      .names = "average"
    ) %>%
    # Clean up
    dplyr::select(-"counts") %>%
    dplyr::mutate(
      average = dplyr::if_else(
        is.na(.data[["predicted"]]),
        .data[["average"]],
        NA_real_
      )
    )
}

add_epicurve_count_cols <- function(gg_obj) {

  # Create color palette
  pal_indigo <- ggsci::pal_material("indigo", n = 4L, reverse = TRUE)

  # Assign fill colors
  collect_color <- pal_indigo(1L)[[1L]]
  pred_color <- "grey30"

  # Add counts to figure
  gg_obj +
    ggplot2::geom_col(
      ggplot2::aes(y = .data[["predicted"]]),
      width = 1,
      fill = pred_color,
      alpha = 0.5
    ) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data[["collected"]]),
      width = 1,
      fill = collect_color
    ) +
    ggplot2::geom_col(
      ggplot2::aes(y = pmax(0L, .data[["new"]])),
      width = 1,
      fill = collect_color,
      # Newly observed cases are marked with off-white outline
      color = "#f0f0f0",
      size = 0.05
    )
}

add_epicurve_avg_line <- function(gg_obj) {
  gg_obj +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["average"]]),
      color = "darkorange",
      size = 1.25
    )
}

add_epicurve_events <- function(gg_obj) {

  # Calculate vertical placement of event labels
  lab_y <- gg_obj[["data"]] %>%
    dplyr::select(-".t") %>%
    max(na.rm = TRUE)

  # Add event labels
  gg_obj %>%
    add_covid_events(lab_y = lab_y) %>%
    add_event("2020-03-09", "Schools Close", lab_y = lab_y)
}

add_epicurve_label <- function(gg_obj) {

  # Get numbers for label
  total_cases <- sum(gg_obj[["data"]][["collected"]], na.rm = TRUE)
  new_reported_cases <- sum(gg_obj[["data"]][["new"]], na.rm = TRUE)

  # Create label text
  label <- paste0(
    "Total Cases = ", format(total_cases, big.mark = ","), "\n",
    "New Reported Cases = ", format(new_reported_cases, big.mark = ",")
  )

  # Create and assign background color
  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)
  box_color <- pal_indigo(9L)[[9L]]

  # Get x and y axis values for label placement
  # x is at beginning of series
  # y is at 80% of July peak
  x <- gg_obj %>%
    purrr::pluck("data") %>%
    dplyr::pull(".t") %>%
    min(na.rm = TRUE)

  y <- gg_obj %>%
    purrr::pluck("data") %>%
    dplyr::filter(
      dplyr::between(.data[[".t"]],as.Date("2020-07-01"), as.Date("2020-07-31"))
    ) %>%
    dplyr::select(-".t") %>%
    max(na.rm = TRUE) %>%
    multiply_by(0.8)

  # Add label to plot
  gg_obj +
    ggplot2::annotate(
      "label",
      x = x,
      y = y,
      label = label,
      fill = box_color,
      color = "grey30",
      fontface = "bold",
      hjust = 0,
      vjust = 1
    )
}

add_epicurve_title_caption <- function(gg_obj, n_missing) {

  # Create caption text
  caption <- paste0(
    "Note: Chart excludes cases with missing specimen collection date ",
    "(N = ", n_missing, ")\n",
    "Data Source: National Electronic Disease Surveillance System (NEDSS)"
  )

  # Add title/subtitle/caption
  add_title_caption(
    gg_obj,
    title = "COVID-19 Cases by Specimen Collection Date",
    subtitle = format(Sys.Date(), "%m/%d/%y"),
    caption = caption
  )
}
