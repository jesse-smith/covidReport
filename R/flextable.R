#' Standardize `flextable` Styling for COVID-19 Reports
#'
#' @param flextable A `flextable` object
#'
#' @param total Does the table contain a "Total" row at the bottom?
#'
#' @param align_label Alignment for left-most column (labels, by convention)
#'
#' @return A `flextable` with modified styling
#'
#' @keywords internal
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


#' Create a Location for a Flextable into a Powerpoint
#'
#' @param flextable The flextable to add
#'
#' @param pptx The powerpoint object
#'
#' @param layout The layout of the slide being added to
#'
#' @param master The slide master to use, if any
#'
#' @param type The placeholder type to use
#'
#' @param pos_h If multiple of the same placeholder `type` exist, how should
#'   the placeholder be chosen? `TRUE` means the horizontal axis will be
#'   used; `FALSE` means vertical. See `pos_first` for more information.
#'
#' @param pos_first If multiple of the same placeholder `type` exist, should
#'   the first (or last) be chosen (along the axis given by `pos_h`)?
#'
#' @param halign Horizontal table alignment with placeholder; `0` aligns left
#'   borders, `1` aligns right borders. Defaults to centered (`0.5`).
#'
#' @param valign Vertical table alignment with placeholder; `0` aligns bottom
#'   borders, `1` aligns top borders. Defaults to centered (`0.5`).
#'
#' @inherit officer::ph_location_template params return
#'
#' @keywords internal
ph_location_table <- function(
  flextable,
  pptx,
  layout,
  master = NULL,
  type = "tbl",
  pos_h = TRUE,
  pos_first = TRUE,
  halign = 0.5,
  valign = 0.5,
  newlabel = "",
  id = 1
) {
  coords <- pptx %>%
    officer::layout_properties(layout = layout, master = master) %>%
    dplyr::filter(.data[["type"]] == {{ type }}) %>%
    dplyr::arrange(
      if ({{ pos_h }}) .data[["offx"]] else .data[["offy"]]
    ) %>%
    dplyr::slice(if ({{ pos_first }}) 1L else dplyr::n()) %>%
    dplyr::select("offx", "offy", "cx", "cy")

  dims <- flextable %>%
    flextable::flextable_dim() %>%
    purrr::flatten_dbl() %>%
    set_names(c("width", "height", "aspect_ratio"))

  left <- coords[["offx"]] + halign * (coords[["cx"]] - dims[["width"]])
  top  <- coords[["offy"]] + (1 - valign) * (coords[["cy"]] - dims[["height"]])

  officer::ph_location_template(
    left = left,
    top = top,
    width = dims[["width"]],
    height = dims[["height"]],
    type = type,
    newlabel = newlabel,
    id = id
  )
}

#' Convert a `flextable` to an HTML String
#'
#' @param flextable A `flextable` object
#'
#' @return A `character` vector of length 1 containing HTML content from a
#'   `flextable`
#'
#' @keywords internal
ft_as_html <- function(flextable) {
  capture.output(
    {ft_html <- flextable::flextable_to_rmd(flextable)},
    file = "NUL"
  )

  ft_html
}
