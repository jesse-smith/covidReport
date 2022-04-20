
#' Plot Active Case Rates by ZIP Code
#'
#' @param data Prepped case data
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @param days Number of active days to consider
#'
#' @return A `ggplot` object
#'
#' @export
active_map_rate <- function(
  data = pos(process_inv(read_inv(date = date))),
  days = 14L,
  date = NULL
) {

  if (!rlang::is_installed("RColorBrewer") || !rlang::is_installed("sf")) {
    rlang::abort(paste(
      "The {RColorBrewer} and {sf} packages must be installed",
      "to use `active_map_rate()`"
    ))
  }

  date <- date_inv(date)

  counts <- data %>%
    filter_active(days = days, date = date) %>%
    dplyr::transmute(
      # `vac_parse_zip()` is defined in coviData
      zip = vac_parse_zip(.data[["patient_zip"]]),
      zip_mrg = dplyr::case_when(
        zip == "" ~ NA_character_,
        zip %in% c('38018','38028') ~ '38018+38028',
        zip %in% c('38103','38104','38105') ~ '38103+38104+38105',
        zip %in% c('38106','38126') ~ '38106+38126',
        zip %in% c('38107','38108') ~ '38107+38108',
        zip %in% c('38112','38122') ~ '38112+38122',
        zip %in% c('38119','38120') ~ '38119+38120',
        zip %in% c('38138','38139') ~ '38138+38139',
        !zip %in% c(
          '38002','38016','38017','38053','38109',
          '38111','38114','38115','38116','38117',
          '38118','38125','38127','38128','38133',
          '38134','38135','38141'
        ) ~ "Other",
        TRUE ~ zip
      )
    ) %>%
    dplyr::count(zip = .data[["zip_mrg"]]) %>%
    dplyr::mutate(
      n = dplyr::if_else(.data[["n"]] == 0L, NA_integer_, .data[["n"]])
    )

  n_total <- sum(counts[["n"]], na.rm = TRUE)

  gg_data <- counts %>%
    dplyr::full_join(covidReport::zip_shape, by = "zip") %>%
    dplyr::mutate(
      rate = 1e5 * .data[["n"]] / .data[["pop_2019"]],
      zip = dplyr::if_else(is.na(.data[["rate"]]), NA_character_, .data[["zip"]]),
      zip_rt_lbl = dplyr::if_else(
        !is.na(.data[["zip"]]) & !is.na(.data[["rate"]]),
        paste0(.data[["zip"]], "\n", round(.data[["rate"]])),
        NA_character_
      )
    )

  n_mapped_all <-  gg_data %>%
    dplyr::filter(.data[["zip"]] != "Other", !is.na(.data[["zip"]])) %>%
    dplyr::pull("n")

  n_mapped <- sum(n_mapped_all, na.rm = TRUE)
  n_missing <- n_total - n_mapped

  str_t <- format(n_total, big.mark = ",")
  str_mp <- format(n_mapped, big.mark = ",")
  str_ms <- format(n_missing, big.mark = ",")

  #addition 4/18: find n of the lowest and highest zip codes and censor lowest if less than 10
  gg_data$max <- dplyr::case_when(
    gg_data[["rate"]] ==  max(gg_data[["rate"]], na.rm = TRUE) ~ gg_data[["n"]]
  )

  gg_data$min <- dplyr::case_when(
    gg_data[["rate"]] ==  min(gg_data[["rate"]], na.rm = TRUE) ~ gg_data[["n"]]
  )

  n_min <- ifelse(min(gg_data[["min"]], na.rm = TRUE)>= 10 , min(gg_data[["min"]], na.rm = TRUE),
                  "Censored")
  n_max <- min(gg_data[["max"]], na.rm = TRUE)
  #end addition 4/18

  #get the max zip code
  gg_data$zipmax <- dplyr::case_when(
    gg_data[["rate"]] ==  max(gg_data[["rate"]], na.rm = TRUE) ~ gg_data[["zip"]]
  )
  zip_max <- max(gg_data[["zipmax"]], na.rm = TRUE)




  rate_total <- 1e5 * n_total / 937166
  rate_min <- min(gg_data[["rate"]], na.rm = TRUE)
  rate_max <- max(gg_data[["rate"]], na.rm = TRUE)

  str_rt_t <- round(rate_total, digits = 1L)
  str_rt_min <- round(rate_min, digits = 1L)
  str_rt_max <- round(rate_max, digits = 1L)



  caption <- paste0(
    "Data Source: National Electronic Disease Surveillance System (NEDSS)\n",
    "Total = ", str_t, " Mapped = ", str_mp,
    " (Other/Missing ZIP = ", str_ms, ")\n",
    # "Data shown on a continuous color scale from 0% to 80%\n",
    "Rates calculated with ACS 2019 5 Year population estimates\n",
    "Highest ZIP: ", zip_max, " - ", "Rate: ", str_rt_max, ", N: ", n_max
  )

  pal_n <- 9L
  pal <- RColorBrewer::brewer.pal(pal_n, "OrRd")

  bbox <- sf::st_bbox(gg_data[["geometry"]])

  breaks <- scale_breaks(rate_min, rate_max)[[1L]]

  label <- paste0(
    "Shelby Co. Total Rate: ", str_rt_t, "\n",
    "Lowest ZIP: ", str_rt_min, "\n",
    "Highest ZIP: ", str_rt_max
  )

  zip_plt <- ggplot2::ggplot(
    gg_data,
    ggplot2::aes(geometry = .data[["geometry"]], fill = .data[["rate"]])
  ) +
    ggplot2::geom_sf() +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = .data[["zip"]]),
      color = "grey30",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_gradientn(
      name = "Active Cases per 100k",
      breaks = breaks,
      oob = scales::oob_squish,
      colors = pal,
      guide = ggplot2::guide_colorbar(
        barheight = grid::unit(0.4, units = "npc")
      ),
      values = 0:(pal_n-1L)/(pal_n-1L),
      na.value = NA_character_
    ) +
    ggplot2::annotate(
      "label",
      x = bbox[["xmin"]],
      y = bbox[["ymax"]],
      label = label,
      vjust = 1,
      hjust = 0,
      color = "grey30",
      size = 14 / ggplot2::.pt,
      fill = NA_character_
    )

  theme_mods <- ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = c(0.11, 0.5),
    legend.direction = "vertical",
    legend.box.background = ggplot2::element_blank()
  )

  set_covid_theme(zip_plt) %>%
    add_title_caption(
      title = "Active Cases by ZIP Code",
      subtitle = format(date, "%m/%d/%Y"),
      caption = caption
    ) %>%
    {. + theme_mods}

}
