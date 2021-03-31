# devtools::install_github("UrbanInstitute/urbnmapr")

# urbnmapr has `sf` data for all US counties - use this for polygon
shelby_poly <- urbnmapr::counties %>%
  # We're only interested in Shelby Co.
  dplyr::filter(county_fips == "47157") %>%
  dplyr::arrange(order) %>%
  dplyr::select(long, lat) %>%
  # Using for count visualization, so bottom needs to be flat
  dplyr::mutate(
    lat = dplyr::if_else(
      min(lat) < lat & lat < 35,
      min(lat),
      lat
    )
  ) %>%
  # Standardize distances
  dplyr::transmute(
    x = (long - min(long)) / (max(long) - min(long)),
    y = (lat - min(lat)) / (max(lat) - min(lat))
  ) %>%
  # Interpolate and smooth - no more than 1/1000th between points
  as.matrix() %>%
  smoothr::smooth_ksmooth(wrap = TRUE, max_distance = 1e-3) %>%
  set_colnames(c("x", "y")) %>%
  dplyr::as_tibble() %>%
  # Re-standardize distances
  dplyr::mutate(
    x = (x - min(x)) / (max(x) - min(x)),
    y = (y - min(y)) / (max(y) - min(y))
  )

usethis::use_data(shelby_poly, overwrite = TRUE)
