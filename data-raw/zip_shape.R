## code to prepare `zip_shape` dataset goes here
zip_path <- coviData::path_create(
  "O:/EPI/Shapefiles 07.2014/MergedZips",
  "AllZipswithMergedZips_clippedbySCBoundary.shp"
)
zip <- sf::read_sf(zip_path) %>%
  dplyr::select(ZIP, zip = ZIPCODEMRG, geometry)

denoms <- coviData::path_create(
  "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA",
  "Confirmed COVID-19 SC cases 03312020_10AM_Zipcode.xlsx"
) %>%
  readxl::read_excel(
    range = readxl::cell_cols("A:D"),
    col_types = rep("text", 4L)
  ) %>%
  dplyr::select(1L, pop = 4L) %>%
  dplyr::transmute(
    pop = as.integer(.data[["pop"]]),
    ZIP = stringr::str_remove_all(.data[["ZipCode"]], "[^0-9]")
  )

zip_shape <- dplyr::left_join(zip, denoms, by = "ZIP") %>%
  dplyr::select(zip, pop_2019 = pop, geometry)

usethis::use_data(zip_shape, overwrite = TRUE)
