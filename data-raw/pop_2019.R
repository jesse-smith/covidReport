age_grp <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
n <- c(195372L, 121794L, 141059L, 116569L, 114610L, 116609L, 80989L, 50164L)
pop_age_2019 <- tibble::tibble(age_grp = age_grp, n = n)

path <- fs::path(
  "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Population Estimates",
  "NCHS Bridged Intercensal Population Estimates/pcen_v2019_y1019.sas7bdat"
)

data <- haven::read_sas(path)

pop_2019 <- data %>%
  dplyr::filter(ST_FIPS == 47, CO_FIPS == 157) %>%
  dplyr::select(-"VINTAGE", -(6:15), -dplyr::ends_with("FIPS")) %>%
  dplyr::mutate(
    ethnicity = dplyr::if_else(
      hisp == 1,
      "Not Hispanic/Latino",
      "Hispanic/Latino"
    ) %>%
      factor() %>%
      forcats::fct_relevel("Hispanic/Latino", "Not Hispanic/Latino"),
    race = dplyr::case_when(
      RACESEX <= 2 ~ "White",
      RACESEX <= 4 ~ "Black/African American",
      RACESEX <= 6 ~ "American Indian/Alaskan Native",
      RACESEX <= 8 ~ "Asian/Pacific Islander"
    ) %>%
      forcats::as_factor() %>%
      forcats::fct_relevel("Black/African American"),
    sex = dplyr::if_else(RACESEX %% 2 == 0, "Female", "Male") %>% factor()
  ) %>%
  dplyr::select(age, sex, race, ethnicity, population = POP2019) %>%
  dplyr::arrange(race)

usethis::use_data(pop_2019, overwrite = TRUE)
