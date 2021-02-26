#' Save a `ggplot` Object
#'
#' `save_plot()` saves a `ggplot` object to the specified path. It is a wrapper
#' around \code{\link[ggplot2:ggsave]{ggsave()}} that takes the plot as the
#' first argument and provides alternate defaults for use in SCHD slides. Unlike
#' `ggsave()` the `ggplot` object must always be supplied explicitly, and it
#' will not overwrite existing files by default.
#'
#' @param gg_obj A `ggplot` object to save
#'
#' @param path The location for saving
#'
#' @param width The width of the saved figure, in inches by default
#'
#' @param height The height of the saved figure, in inches by default
#'
#' @param force Should existing files be overwritten?
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[ggplot2:ggsave]{ggsave()}}
#'
#' @return The `gg_obj`, invisibly
#'
#' @export
save_plot <- function(
  gg_obj,
  path,
  width = 16,
  height = 9,
  force = FALSE,
  ...
) {
  path <- coviData::path_create(path)
  if (fs::file_exists(path) && !force) {
    rlang::abort(
      paste(
        "A file already exists at this location.",
        "To overwrite, set `force = TRUE`."
      )
    )
  }
  ggplot2::ggsave(path, plot = gg_obj, width = width, height = height, ...)

  invisible(gg_obj)
}
