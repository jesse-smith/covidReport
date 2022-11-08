#' Plot % Fully Vaccinated by ZIP Code
#'
#' @param data Prepped vaccination data from TennIIS
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @param zip_path Path to Shelby ZIP shapefile
#'
#' @return A `ggplot` object
#'
#' @export
vac_bivalent_map_pct <- function(
  data = vac_prep(date = date, distinct = TRUE),
  date = NULL,
  zip_path = coviData::path_create(
    "O:/EPI/Shapefiles 07.2014/MergedZips",
    "AllZipswithMergedZips_clippedbySCBoundary.shp"
  )
) {

  date <- date_vac(date)

  if (!rlang::is_installed("RColorBrewer") || !rlang::is_installed("sf")) {
    rlang::abort(paste(
      "The {RColorBrewer} and {sf} packages must be installed",
      "to use `vac_map_pct()`"
    ))
  }


  counts <- data %>%
    dplyr::filter(.data[["status"]] == "Bivalent Booster") %>%
    dplyr::transmute(
      zip = vac_parse_zip(.data[["address_zip"]]),
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
      vac_pct = 100 * .data[["n"]] / .data[["pop_2019"]],
      zip = dplyr::if_else(is.na(.data[["vac_pct"]]), NA_character_, .data[["zip"]]),
      zip_pct_lbl = dplyr::if_else(
        !is.na(.data[["zip"]]) & !is.na(.data[["vac_pct"]]),
        paste0(
          .data[["zip"]], "\n",
          round(.data[["vac_pct"]]), "%"
        ),
        NA_character_
      )
    )

  data_out <- gg_data %>%
    dplyr::select(zip, n, vac_pct)

  #output daily zip vaccine counts to v drive for google sheets
  write.csv(data_out, file = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/COVID-19 Vaccine Reporting/daily vac counts/zip_count_BivalentVac_", date, ".csv"))


  n_mapped_all <-  gg_data %>%
    dplyr::filter(.data[["zip"]] != "Other", !is.na(.data[["zip"]])) %>%
    dplyr::pull("n")

  n_mapped <- sum(n_mapped_all, na.rm = TRUE)
  n_missing <- n_total - n_mapped

  str_t <- format(n_total, big.mark = ",")
  str_mp <- format(n_mapped, big.mark = ",")
  str_ms <- format(n_missing, big.mark = ",")


  pct_total <- 100 * n_total / 937166L
  pct_min <- min(gg_data[["vac_pct"]], na.rm = TRUE)
  pct_max <- max(gg_data[["vac_pct"]], na.rm = TRUE)

  str_pct_t <- paste0(round(pct_total, digits = 1L), "%")
  str_pct_min <- paste0(round(pct_min, digits = 1L), "%")
  str_pct_max <- paste0(round(pct_max, digits = 1L), "%")



  pal_n <- 9L
  pal <- RColorBrewer::brewer.pal(pal_n, "Purples")

  bbox <- sf::st_bbox(gg_data[["geometry"]])


  break_max <- dplyr::case_when(
    ceiling(max(gg_data$vac_pct, na.rm = TRUE)) < 25 ~ 25,
    ceiling(max(gg_data$vac_pct, na.rm = TRUE)) < 50 ~ 50,
    ceiling(max(gg_data$vac_pct, na.rm = TRUE)) < 75 ~ 75,
    TRUE ~ 100
  )

  break_by <- dplyr::case_when(
    break_max == 25 ~ 5,
    break_max == 50 ~ 10,
    break_max == 75 ~ 15,
    TRUE ~ 20
  )


  caption <- paste0(
    "Data Source: Tennessee Immunization Information System (TennIIS)\n",
    "Total = ", str_t, " Mapped = ", str_mp,
    " (Other/Missing ZIP = ", str_ms, ")\n",
     "Data shown on a continuous color scale from 0% to ", break_max, "%\n",
    "Rates calculated with ACS 2019 5 Year population estimates"
  )


  breaks <- seq(0, break_max, by = break_by)

  label <- paste0(
    "Shelby Co. Total: ", str_pct_t, "\n",
    "Lowest ZIP: ", str_pct_min, "\n",
    "Highest ZIP: ", str_pct_max
  )

  zip_plt <- ggplot2::ggplot(
    gg_data,
    ggplot2::aes(geometry = .data[["geometry"]], fill = .data[["vac_pct"]])
  ) +
    ggplot2::geom_sf() +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = .data[["zip"]]),
      color = "grey30",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_gradientn(
      name = "% Vaccinated",
      breaks = breaks,
      labels = function(x) paste0(round(x, 1L), "%"),
      limits = c(0, break_max),
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
    legend.position = c(0.1, 0.525),
    legend.direction = "vertical",
    legend.box.background = ggplot2::element_blank()
  )

  set_covid_theme(zip_plt) %>%
    add_title_caption(
      title = "Population Vaccinated by ZIP Code\n(Bivalent Booster)",
      subtitle = paste("Through", format(date_vac(date), "%m/%d/%Y")),
      caption = caption
    ) %>%
    {. + theme_mods}

}

