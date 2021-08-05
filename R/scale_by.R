#' Calculate Scale Guides for Plots
#'
#' @param range Either a scalar numeric indicating the range of the scale,
#'   or a length 2 numeric indicating the min and max of the scale
#'
#' @return A `double`
#'
#' @keywords internal
scale_by <- function(range) {
  if (vec_is(range, size = 2L)) {
    range <- diff(range)
  } else if (!vec_is(range, size = 1L)) {
    rlang::abort("`range` must be a length 1 or 2 numeric vector")
  }

  magnitude <- 10^floor(log10(range))
  location  <- range / magnitude
  by <- purrr::when(
    location,
    . <= 2.5 ~ 0.10,
    . <= 5.0 ~ 0.25,
    . <= 7.5 ~ 0.50,
             ~ 1.00
  )

  as.double(by * magnitude)
}
