fmt_covid_table <- function(gt_tbl) {
  gt_tbl %>%
    gt::tab_style(
      style = list(
        gt::cell_text(color = "white"),
        gt::cell_fill(color = "midnightblue")
      ),
      locations = gt::cells_column_labels(gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(),
      locations = list(
        gt::cells_column_labels(gt::everything()),
        gt::cells_body()
      )
    ) %>%
    gt::opt_table_lines("none")
}
