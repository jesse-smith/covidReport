library(magrittr)
zips <- sf::read_sf("O:/EPI/Shapefiles 07.2014/MergedZips/AllZipswithMergedZips_clippedbySCBoundary.shp")

rates <- readxl::read_excel("V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Confirmed COVID-19 SC cases 03312020_10AM_Zipcode.xlsx") %>%
  dplyr::select("ZipCode", "PopEstACS18_5YR", "VacPerPop3_31") %>%
  dplyr::mutate(ZIP = stringr::str_remove_all(ZipCode, "[^0-9]"))

plt <- dplyr::left_join(zips, rates, by = "ZIP") %>%
  ggplot2::ggplot(ggplot2::aes(fill = VacPerPop3_31)) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf_text(ggplot2::aes(label = ZipCode)) +
  ggthemes::scale_fill_continuous_tableau(palette = "Orange-Gold")

coviData::set_covid_theme(plt) %>%
  coviData::add_title_caption(title = "UK Variant by ZIP") +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), axis.text = ggplot2::element_blank())

