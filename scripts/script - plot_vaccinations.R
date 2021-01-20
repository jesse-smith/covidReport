devtools::load_all()

p <- plot_vaccinations(
  n_partial = 26688,
  n_full = 6999,
  date_updated = as.Date("2020-01-18")
) %T>% show()

ggplot2::ggsave(
  paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/figs/vaccine_slide/vaccination_goal_",
    Sys.Date(),
    ".png"
  ),
  p,
  width = 12,
  height = 9
)
