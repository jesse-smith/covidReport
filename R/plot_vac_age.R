#' Plot Vaccinations by Age Group
#'
#' `plot_vac_age()` plots vaccinations by age group in a bar chart. It can
#' produce 4 distinct figures, depending on the values of `pct` and `by_pop`.
#'
#' When `by_pop = TRUE`,
#' the resulting figure displays vaccinations with reference to the population
#' of each age group; when `by_pop = FALSE`, the figure does not display
#' population-related values.
#'
#' When `pct = TRUE`, values are displayed as percentages; when `pct = FALSE`,
#' raw totals are displayed instead.
#'
#' The combination `pct = FALSE, by_pop = TRUE` results in the most informative
#' (but most complex) display. It shows vaccination counts as a fraction of
#' population totals. Any of the other 3 charts can be build from the
#' information in this one; however, this also means that the "take-home
#' message" from this figure is more open-ended than the rest.
#'
#' @param .data Vaccination data, as created by
#'   \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param pct Should the resulting graphic display percentages
#'   (`TRUE`, the default) or totals (`FALSE`)?
#'
#' @param by_pop Should the resulting graphic be calculated using age group
#'   population statistics (`TRUE`, the default) or not (`FALSE`)?
#'
#' @param incl_under_15 Should the age group `"0-15"` be included? The default
#'   is `FALSE`.
#'
#' @return A `ggplot` object
#'
#' @export
plot_vac_age <- function(
  .data = coviData::vac_load() %>% coviData::vac_prep(),
  pct = TRUE,
  by_pop = TRUE,
  incl_under_15 = FALSE
) {

  pct <- coviData::assert_bool(pct)
  by_pop <- coviData::assert_bool(by_pop)
  incl_under_15 <- coviData::assert_bool(incl_under_15)

  date <- .data %>%
    dplyr::pull("vacc_date") %>%
    coviData::std_dates(orders = "mdY", force = "dt") %>%
    max(na.rm = TRUE)

  gg_data <- .data %>%
    vac_count_age_grp() %>%
    vac_join_age_pop(incl_under_15 = incl_under_15) %>%
    vac_age_fct()

  gg_data %>%
    vac_age_ggplot(pct = pct, by_pop = by_pop) %>%
    set_covid_theme() %>%
    set_vac_age_axis_limits() %>%
    add_vac_age_axis_labels(by_pop = by_pop) %>%
    add_vac_age_col(by_pop = by_pop) %>%
    add_vac_age_col_labels() %>%
    remove_x_grid() %>%
    add_vac_age_scale(by_pop = by_pop) %>%
    add_vac_age_title_caption(by_pop = by_pop, date = date)
}

vac_age_ggplot <- function(.data, pct, by_pop) {

  ggplot2::ggplot(
    .data,
    ggplot2::aes(
      x = .data[["age_grp"]],
      y = !!vac_age_choose_y(pct = pct, by_pop = by_pop)
    )
  )
}

vac_age_test_pct <- function(gg_obj) {
  y <- rlang::eval_tidy(
    gg_obj[["mapping"]][["y"]],
    data = gg_obj[["data"]]
  )

  max(y, na.rm = TRUE) <= 1L
}

vac_age_choose_y <- function(pct, by_pop) {
  pct <- coviData::assert_bool(pct)
  by_pop <- coviData::assert_bool(by_pop)

  if (by_pop && pct) {
    rlang::expr(.data[["n_vac"]] / .data[["n_pop"]])
  } else if (!by_pop && pct) {
    rlang::expr(.data[["n_vac"]] / sum(.data[["n_vac"]], na.rm = TRUE))
  } else {
    rlang::expr(.data[["n_vac"]])
  }
}

set_vac_age_axis_limits <- function(gg_obj) {
  pct <- vac_age_test_pct(gg_obj)
  set_axis_limits(gg_obj, ylim = if (pct) c(0, 1) else NULL)
}

remove_x_grid <- function(gg_obj) {
  gg_obj +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}

add_vac_age_col <- function(gg_obj, by_pop) {
  pct <- vac_age_test_pct(gg_obj)
  by_pop <- coviData::assert_bool(by_pop)

  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)
  color <- pal_indigo(2L)[[2L]]
  width <- if (by_pop) 0.95 else 0.99

  if (by_pop && !pct) {
    gg_obj +
      ggplot2::geom_col(
        ggplot2::aes(y = .data[["n_pop"]]),
        color = color,
        fill  = NA,
        width = width
      ) +
      ggplot2::geom_col(fill = color, width = width)
  } else {
    gg_obj + ggplot2::geom_col(fill = color, width = width)
  }
}

add_vac_age_scale <- function(gg_obj, by_pop) {
  pct <- vac_age_test_pct(gg_obj)
  by_pop <- coviData::assert_bool(by_pop)

  label_fn <- purrr::partial(vac_age_label_fn, pct = !!pct)

  if (pct) {
    breaks <- seq(0, 1, by = 0.1)
  } else if (by_pop) {
    breaks <- seq.int(0L, 1e6L, by = 20000L)
  } else {
    breaks <- ggplot2::waiver()
  }

  gg_obj + ggplot2::scale_y_continuous(
    breaks = breaks,
    labels = label_fn,
    minor_breaks = NULL
  )
}

add_vac_age_axis_labels <- function(gg_obj, by_pop) {
  pct <- vac_age_test_pct(gg_obj)
  by_pop <- coviData::assert_bool(by_pop)

  ylab <- dplyr::case_when(
    pct &&  by_pop ~ "% Population",
    pct && !by_pop ~ "% Vaccinations",
     by_pop ~ "People",
    !by_pop ~ "People"
  )
  add_axis_labels(gg_obj, xlab = "Age", ylab = ylab)
}

add_vac_age_col_labels <- function(gg_obj) {
  pct <- vac_age_test_pct(gg_obj)

  y <- gg_obj[["mapping"]][["y"]]

  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)
  color <- pal_indigo(2L)[[2L]]

  gg_obj_lbl1 <- gg_obj +
    ggplot2::geom_label(
      ggplot2::aes(
        label = vac_age_label_fn(!!y, pct = max(!!y, na.rm = TRUE) <= 1)
      ),
      color = color,
      fill = "#f0f0f0",
      size = 4.5,
      vjust = 0
    )

  if (pct) {

    gg_obj_lbl1 + ggplot2::geom_label(
      ggplot2::aes(
        label = dplyr::if_else(
          !!y >= 0.05,
          vac_age_label_fn(.data[["n_vac"]], pct = FALSE),
          NA_character_
        )
      ),
      color = "#f0f0f0",
      fill  = color,
      size  = 4.5,
      vjust = 1,
      label.size = 0
    )
  }
}

vac_age_label_fn <- function(x, pct, suffix = "") {
  pct <- coviData::assert_bool(pct)
  vctrs::vec_assert(suffix, ptype = character(), size = 1L)
  add_space <- all(
    stringr::str_length(suffix) > 0L,
    stringr::str_starts(suffix, "\\s")
  )
  purrr::map_chr(
    x * if (pct) 100 else 1e-3,
    ~ paste0(
      round(.x, digits = if (rlang::is_integerish(.x)) 0L else 1L),
      if (pct) "%" else "k",
      if (add_space) " ",
      suffix
    )
  )
}

add_vac_age_title_caption <- function(gg_obj, by_pop, date) {

  title <- dplyr::case_when(
     by_pop ~ "Population Vaccinated by Age",
    !by_pop ~ "People Vaccinated by Age"
  )
  subtitle <- format(as.Date(date), "%B %d, %Y")

  caption <- paste0(
    "Totals, percentages, and ages are calculated using first doses only\n",
    "Data Source: Tennessee Immunization Information System (TennIIS)"
  )
  add_title_caption(
    gg_obj,
    title = title,
    subtitle = subtitle,
    caption = caption
  )
}

vac_count_age_grp <- function(.data) {
  .data %>%
    dplyr::filter(.data[["resident"]] | is.na(.data[["resident"]])) %>%
    dplyr::filter(as.integer(.data[["dose_count"]]) == 1L) %>%
    dplyr::transmute(
      age_grp = .data[["age_at_admin"]] %>% std_age() %>% vac_age_grp()
    ) %>%
    dplyr::count(.data[["age_grp"]])
}

std_age <- function(x) {
  x_dbl <- as.double(x)
  dplyr::if_else(dplyr::between(x_dbl, 0, 120), x_dbl, NA_real_)
}

vac_age_grp <- function(dbl) {

  vctrs::vec_assert(dbl, ptype = double())

  breaks <- c(0, seq(15, 75, by = 10), 120)
  lbls <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")

  cut(
    dbl,
    breaks = breaks,
    labels = lbls,
    right = FALSE,
    ordered_result = TRUE
  ) %>% as.character()
}

vac_join_age_pop <- function(.data, incl_under_15) {
  .data %>%
    dplyr::left_join(
      covidReport::pop_age_2019,
      by = "age_grp",
      suffix = c("_vac", "_pop")
    ) %>%
    purrr::when(
      incl_under_15 ~ .,
      ~ dplyr::filter(., .data[["age_grp"]] != "0-14")
    )
}

vac_age_fct <- function(.data) {
  dplyr::mutate(
    .data,
    age_grp = factor(.data[["age_grp"]], ordered = TRUE)
  )
}
