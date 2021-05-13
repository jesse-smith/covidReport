fmt_covid_table <- function(flextable, total = FALSE) {

  inner_border <- officer::fp_border("grey60")
  outer_border <- officer::fp_border("grey30", width = 2)

  covid_flextable <- flextable %>%
    # Background
    flextable::bg(bg = "#f0f0f0", part = "all") %>%
    flextable::bg(bg = "midnightblue", part = "header") %>%
    # Font color
    flextable::color(color = "#f0f0f0", part = "header") %>%
    flextable::color(color = "grey30", part = "body") %>%
    flextable::color(color = "grey60", part = "footer") %>%
    # Font boldness
    flextable::bold(part = "header") %>%
    flextable::bold(j = 1L, part = "body") %>%
    # Borders
    flextable::border_remove() %>%
    flextable::border_inner_h(inner_border) %>%
    flextable::hline_bottom(border = outer_border)

  if (!total) return(covid_flextable)

  covid_flextable %>%
    flextable::bold(i = flextable::nrow_part(.)) %>%
    flextable::hline_top(i = flextable::nrow_part(.), border = outer_border) %>%
    flextable::hline_bottom(i = flextable::nrow_part(.), border = outer_border)
}
