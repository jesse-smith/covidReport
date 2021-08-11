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
  list(lim_lower, lim_upper, by) %>%
    purrr::pmap(~ seq(..1, ..2, by = ..3)) %>%
    as_list_of(.ptype = double())
}

#' Calculate Scale Guides for Plots
#'
#' @param range A scalar numeric indicating the range of the scale
#'
#' @return A `double`
#'
#' @keywords internal
scale_by <- function(range) {
  range_scalar <- rlang::is_true(vec_size(range) == 1L)
  range_num  <- rlang::is_true(is.numeric(range))
  range_not_empty <- rlang::is_true(!vec_is_empty(range))
  range_positive  <- rlang::is_true(range >= 0)
  valid_range <- c(range_scalar, range_num, range_not_empty, range_positive)

  if (!all(valid_range)) {
    nms <- dplyr::if_else(valid_range, "", "x")
    checks <- c(
      "scalar (length 1)",
      "numeric (integer or double)",
      "non-empty (not `NULL` or length 0)",
      "positive (not 0, negative, or missing)"
    )
    valid_range <-
    rlang::abort(
      paste0(
        "`range` must be:\n",
        rlang::format_error_bullets(vec_set_names(checks, nms))
      )
    )
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
