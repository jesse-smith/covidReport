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
