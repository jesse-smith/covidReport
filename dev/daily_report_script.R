ppl_pos <- coviData::load_sas(dataset = "positive_ppl")
ppl_neg <- coviData::load_sas(dataset = "negative_ppl")
pcr_pos <- coviData::load_sas(dataset = "positive_pcr")
pcr_neg <- coviData::load_sas(dataset = "negative_pcr")

ppl_pos %>% generate_epicurve() %>% plot_epicurve()

save_epicurve(epicurve)

load_epicurve() %>%
  coviData::fill_dates(date_col = date, start = as.Date("2020-03-05"), end = Sys.Date() - 1)
previous_epicurve

ggsave("curve.png", width = 16, height = 9)
