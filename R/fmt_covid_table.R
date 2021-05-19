#' Standardize Table Styling for COVID-19 Reports
#'
#' `fmt_covid_table()` standardizes styling for `flextable` and `gt_tbl` objects
#'
#' @param table A `flextable` or `gt_tbl` object
#'
#' @param total Does the table contain a "Total" row at the bottom?
#'
#' @param align_label Alignment for left-most column (labels, by convention)
#'
#' @return A `flextable` or `gt_tbl` with modified styling
#'
#' @keywords internal
fmt_covid_table <- function(
  table,
  total = FALSE,
  align_label = c("left", "center", "right")
) {
  UseMethod("fmt_covid_table")
}

#' @rdname fmt_covid_table
#'
#' @export
fmt_covid_table.flextable <- function(
  table,
  total = FALSE,
  align_label = c("left", "center", "right")
) {

  total <- coviData::assert_bool(total)
  align_label <- rlang::arg_match(align_label)[[1L]]

  inner_border <- officer::fp_border("grey60")
  outer_border <- officer::fp_border("grey30", width = 2)

  covid_table <- table %>%
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

#' @rdname fmt_covid_table
#'
#' @export
fmt_covid_table.gt_tbl <- function(
  table,
  total = FALSE,
  align_label = c("left", "center", "right")
) {

  total <- coviData::assert_bool(total)
  align_label <- rlang::arg_match(align_label)[[1L]]

  header <- gt::cells_column_labels(gt::everything())
  label_col <- gt::cells_body(columns = 1L)

  table %>%
    # Background
    gt::tab_options(
      table.background.color = "#f0f0f0",
      column_labels.background.color = "midnightblue"
    ) %>%
    # Font
    gt::tab_options(table.font.names = c("Arial", "Helvetica")) %>%
    # Font color
    gt::tab_options(table.font.color = "grey30") %>%
    gt::tab_style(gt::cell_text(color = "#f0f0f0"), location = header) %>%
    # Font boldness
    gt::tab_style(
      gt::cell_text(weight = "bold"),
      locations = list(header, label_col)
    ) %>%
    # Borders
    gt::tab_options(
      # Table top border (delete)
      table.border.top.color = "#00000000",
      table.border.top.width = gt::px(0L),
      # Table bottom border
      table.border.bottom.color = "grey30",
      # Column labels top/bottom borders (delete)
      column_labels.border.top.color = "#00000000",
      column_labels.border.top.width = gt::px(0L),
      column_labels.border.bottom.color = "#00000000",
      column_labels.border.bottom.width = gt::px(0L),
      # Inner horizontal borders
      table_body.hlines.color = "grey60",
      # Footer bottom border (delete)
      source_notes.border.bottom.color = "#00000000",
      source_notes.border.bottom.width = gt::px(0L)
    ) %>%
    # Align label column
    gt::tab_style(
      gt::cell_text(align = align_label),
      locations = list(
        label_col,
        gt::cells_column_labels(1L)
      )
    ) %>%
    # Format total row
    purrr::when(
      total ~ gt::tab_style(
        .,
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_borders(
            c("top", "bottom"),
            color = "grey30",
            weight = gt::px(2L)
          )
        ),
        locations = gt::cells_body(rows = NROW(extract2(., "_data")))
      ),
      ~ .
    )
}
