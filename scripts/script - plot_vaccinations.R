devtools::load_all()

p <- plot_vaccinations(
  n_partial = 5893,
  n_full = 0
) %T>% show()

ggplot2::ggsave(
  paste0("figs/vaccination_fig_", Sys.Date(), ".png"),
  p,
  width = 12,
  height = 9
)
