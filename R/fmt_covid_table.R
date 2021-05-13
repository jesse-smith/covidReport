fmt_covid_table <- function(
  flextable,
  total = FALSE,
  align_label = c("left", "center", "right")
) {

  align_label <- rlang::arg_match(align_label)[[1L]]

  inner_border <- officer::fp_border("grey60")
  outer_border <- officer::fp_border("grey30", width = 2)

  covid_flextable <- flextable %>%
    # Background
    flextable::bg(bg = "#f0f0f0", part = "all") %>%
    flextable::bg(bg = "midnightblue", part = "header") %>%
    # Font
    flextable::font(fontname = "Arial", part = "all") %>%
    # Font size
    flextable::fontsize(size = 20, part = "all") %>%
    flextable::fontsize(size = 14, part = "footer") %>%
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
    flextable::hline_bottom(border = outer_border) %>%
    # Alignment label column
    flextable::align(j = 1L, align = align_label, part = "all") %>%
    # Format total row
    purrr::when(
      total ~ flextable::bold(., i = flextable::nrow_part(.)) %>%
        flextable::hline(
          i = c(flextable::nrow_part(.), flextable::nrow_part(.)-1L),
          border = outer_border
        ),
      ~ .
    )
}
