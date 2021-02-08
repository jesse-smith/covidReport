devtools::load_all()

p <- plot_vaccinations2(date_updated = "2021-02-06") %T>% show()

fname <- paste0(
  "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
  "COVID-19 Vaccine Reporting/figs/vaccination_goal/vaccination_goal_",
  "2021-02-06",
  ".png"
)

ggplot2::ggsave(
  fname,
  p,
  width = 12,
  height = 9
)
