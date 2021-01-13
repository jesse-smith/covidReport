devtools::load_all()

p <- plot_vaccinations(
  n_partial = 15261,
  n_full = 108,
  date_updated = as.Date("2020-01-07")
) %T>% show()

ggplot2::ggsave(
  paste0("figs/vaccination_fig_", Sys.Date(), ".png"),
  p,
  width = 12,
  height = 9
)
