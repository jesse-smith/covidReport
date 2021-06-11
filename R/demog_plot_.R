#' Plot Demographic Summary
#'
#' @param data Data from an `active_calc_*()` function
#'
#' @param grp The grouping variable
#'
#' @return A `ggplot`
#'
#' @keywords internal
demog_plot_ <- function(
  data,
  unit,
  grp = c("age", "sex", "race", "ethnicity"),
  color = "midnightblue",
  vjust = c("top", "bottom"),
  date = NULL
) {
  grp <- rlang::arg_match(grp)[[1L]]

  data %>%
    dplyr::select("grp", "rate") %>%
    dplyr::filter(as.character(.data[["grp"]]) != "Missing") %>%
    dplyr::mutate(grp = forcats::fct_drop(.data[["grp"]], "Missing")) %>%
    demog_ggplot_() %>%
    set_covid_theme() %>%
    add_demog_axis_labels_(grp = grp) %>%
    add_demog_col_(fill = color) %>%
    add_demog_col_labels_(color = color, vjust = vjust) %>%
    remove_x_grid() %>%
    add_demog_scale_() %>%
    add_demog_title_caption_(unit = unit, grp = grp, date = date)
}


demog_ggplot_ <- function(data) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[["grp"]], y = 1e5 * .data[["rate"]])
  )
}

add_demog_axis_labels_ <- function(gg_obj, grp) {
  Grp <- stringr::str_to_title(grp)
  add_axis_labels(gg_obj, xlab = Grp, ylab = "Rate per 100,000 Population")
}

add_demog_col_ <- function(gg_obj, fill = "midnightblue") {
  assert(rlang::is_string(fill))

  gg_obj + ggplot2::geom_col(fill = fill)
}

add_demog_col_labels_ <- function(
  gg_obj,
  color = "midnightblue",
  vjust = c("top", "bottom")
) {

  assert(rlang::is_string(color))
  vjust <- rlang::arg_match(vjust)[[1L]]

  y <- gg_obj[["mapping"]][["y"]]

  gg_obj_lbl <- gg_obj +
    ggplot2::geom_label(
      ggplot2::aes(
        label = purrr::map_chr(
          !!y,
          ~ formatC(
            .x,
            format = "f",
            digits = if (.x < 1e3) 1L else 0L,
            big.mark = ","
          )
        )
      ),
      color = if (vjust == "top") "#f0f0f0" else color,
      fill  = if (vjust == "top") color     else "#f0f0f0",
      size  = 4.5,
      vjust = vjust,
      label.size = 0
    )

  if (vjust == "bottom") {
    y_max <- max(rlang::eval_tidy(y, data = gg_obj_lbl[["data"]]), na.rm = TRUE)
    gg_obj_lbl + ggplot2::coord_cartesian(ylim = c(0, y_max*1.05))
  } else {
    gg_obj_lbl
  }
}

add_demog_scale_ <- function(gg_obj) {

  y_max <- gg_obj[["mapping"]][["y"]] %>%
    rlang::eval_tidy(data = gg_obj[["data"]]) %>%
    max(na.rm = TRUE)

  magnitude <- floor(log10(y_max))

  scale_decision <- y_max / 10^magnitude

  if (scale_decision < 1.4) {
    scale_by <- 10^(magnitude-1)
  } else if (scale_decision < 3.5) {
    scale_by <- 2*10^(magnitude-1)
  } else if (scale_decision < 7) {
    scale_by <- 5*10^(magnitude-1)
  } else {
    scale_by <- 10^magnitude
  }

  breaks <- seq(0, 2 * y_max, by = scale_by)

  labels <- format(breaks, big.mark = ",")

  gg_obj +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = labels
    )
}

add_demog_title_caption_ <- function(gg_obj, unit, grp, date) {
  Grp <- stringr::str_to_title(grp)
  Unit <- stringr::str_to_title(unit)
  cap <- "Data Source: National Electronic Disease Surveillance System (NEDSS)"
  add_title_caption(
    gg_obj,
    title = paste(Unit, "by", Grp, "(per 100k)"),
    subtitle = format(lubridate::as_date(date), "%B %d, %Y"),
    caption = cap
  )
}
