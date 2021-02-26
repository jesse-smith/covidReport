age_grp <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
n <- c(195372L, 121794L, 141059L, 116569L, 114610L, 116609L, 80989L, 50164L)
pop_age_2019 <- tibble::tibble(age_grp = age_grp, n = n)

usethis::use_data(pop_age_2019, overwrite = TRUE)
