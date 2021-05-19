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
