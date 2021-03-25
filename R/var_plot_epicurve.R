var_plot_epicurve <- function(data = coviData::var_parse(), avg = TRUE) {

  var_cases <- var_count_daily() %>% dplyr::rename(variant = "n")

  all_cases <- coviData::process_positive_people() %>%
    dplyr::left_join(
      dplyr::as_tibble(coviData::load_report_date()),
      by = "inv_local_id"
    ) %>%
    dplyr::transmute(
      .data[["inv_local_id"]],
      collection_date = lubridate::as_date(.data[["collection_date"]])
    ) %>%
    dplyr::filter(
      dplyr::between(
        .data[["collection_date"]],
        as.Date("2020-03-05"),
        lubridate::today()
      )
    ) %>%
    dplyr::distinct(.data[["inv_local_id"]], .keep_all = TRUE) %>%
    dplyr::count(.data[["collection_date"]], name = "all")

  if (avg) {
    all_cases <- dplyr::mutate(
      all_cases,
      all = timetk::slidify_vec(
        .data[["all"]],
        mean,
        na.rm = TRUE,
        .period = 7L,
        .align = "right",
        .partial = TRUE
      )
    )

    var_cases <- dplyr::mutate(
      var_cases,
      variant = timetk::slidify_vec(
        .data[["variant"]],
        mean,
        na.rm = TRUE,
        .period = 7L,
        .align = "right",
        .partial = TRUE
      )
    )
  }

  start <- min(var_cases[["collection_date"]])
  end <- coviData::load_report_date() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      dplyr::across(where(lubridate::is.POSIXt), lubridate::as_date)
    ) %>%
    covidModel::estimate_delay() %>%
    dplyr::pull("collection_date")

  dplyr::full_join(
    all_cases,
    var_cases,
    by = "collection_date"
  ) %>%
    dplyr::filter(.data[["collection_date"]] <= end) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0L)),
      norm = pmax(.data[["all"]] - .data[["variant"]], 0L),
      pct =
    )
    purrr::when(
      avg ~ dplyr::mutate(
        .,
        dplyr::across(
          c("all", "variant", "norm"),
          ~ timetk::slidify_vec(
            .x,
            mean,
            na.rm = TRUE,
            .period = 7L,
            .align = "right",
            .partial = TRUE
          )
        )
      ),
      ~ .
    ) %>%
    tidyr::pivot_longer(
      c("variant", "norm"),
      names_to = "type",
      values_to = "n"
    )

  gg_data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["collection_date"]],
        y = .data[["n"]],
        fill = .data[["type"]]
      )
    ) %>%
    coviData::set_covid_theme() %>%
    coviData::set_axis_limits(xlim = c(start, end)) %>%
    var_epi_plot_area() %>%
    coviData::add_covid_events(lab_y = 1000)
}

var_count_daily <- function(data = coviData::var_parse()) {
  data %>%
    dplyr::count(.data[["specimendate"]]) %>%
    dplyr::rename(collection_date = "specimendate")
}

var_epi_plot_area <- function(gg_obj) {
  gg_obj + ggplot2::geom_area()
}
