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
    . <=  5/3 ~ 0.1,
    . <=  7/3 ~ 0.2,
    . <= 12/3 ~ 0.5,
              ~ 1.0
  )

  as.double(by * magnitude)
}
