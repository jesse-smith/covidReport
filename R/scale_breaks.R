#' Create breaks for scales
#'
#' @param min Minimum value in scale range
#'
#' @param max Maximum value in scale range
#'
#' @return A numeric sequence of appropriately spaced break points
#'
#' @keywords internal
scale_breaks <- function(lower, upper, lim_zero = TRUE) {
  # `by` for sequence creation
  by <- purrr::map_dbl(upper - lower, scale_by)

  # Get limits...
  lim_lower_tmp <- floor(lower / by) * by
  lim_upper_tmp <- ceiling(upper / by) * by

  # ... but make sure they are strictly outside the data range
  lim_lower <- dplyr::if_else(
    lim_lower_tmp == lower & !(dplyr::near(lim_lower_tmp, 0) & lim_zero),
    lim_lower_tmp - by,
    lim_lower_tmp
  )
  lim_upper <- dplyr::if_else(
    lim_upper_tmp == upper & !(dplyr::near(lim_upper_tmp, 0) & lim_zero),
    lim_upper_tmp + by,
    lim_upper_tmp
  )

  purrr::pmap(
    list(lim_lower, lim_upper, by),
    ~ seq(..1, ..2, by = ..3)
  )
}

#' Calculate Scale Guides for Plots
#'
#' @param range A scalar numeric indicating the range of the scale
#'
#' @return A `double`
#'
#' @keywords internal
scale_by <- function(range) {
  invalid_range <- {
    vec_size(range) != 1L ||
      !is.numeric(range) ||
      vec_is_empty(range) ||
      dplyr::near(range, 0L)
  }
  if (invalid_range) {
    rlang::abort("`range` must be a non-empty, positive, numeric scalar")
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
