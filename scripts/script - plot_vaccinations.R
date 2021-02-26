devtools::load_all()

p <- plot_vaccinations() %T>% show()

fname <- paste0(
  "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
  "COVID-19 Vaccine Reporting/figs/vaccination_goal/vaccination_goal_",
  vac_date(),
  ".png"
)

ggplot2::ggsave(
  fname,
  p,
  width = 12,
  height = 9
)
