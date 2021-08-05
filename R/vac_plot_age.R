#' Plot Vaccinations by Age Group
#'
#' `vac_plot_age()` plots vaccinations by age group in a bar chart.
#'
#' When `by_pop = TRUE`,
#' the resulting figure displays vaccinations with reference to the population
#' of each age group; when `by_pop = FALSE`, the figure does not display
#' population-related values.
#'
#' @param .data Vaccination data, as created by
#'   \code{\link[coviData:vac_prep]{vac_prep()}}
#'
#' @param by_pop Should the resulting graphic be calculated using age group
#'   population statistics (`TRUE`, the default) or not (`FALSE`)?
#'
#' @param incl_under_12 Should the age group `"0-11"` be included? The default
#'   is `FALSE`.
#'
#' @return A `ggplot` object
#'
#' @export
vac_plot_age <- function(
  .data = coviData::vac_prep(coviData::read_vac()),
  by_pop = TRUE,
  incl_under_12 = FALSE
) {
  by_pop <- coviData::assert_bool(by_pop)
  incl_under_12 <- coviData::assert_bool(incl_under_12)

  date <- .data %>%
    dplyr::pull("vacc_date") %>%
    coviData::std_dates(orders = "mdY", force = "dt") %>%
    max(na.rm = TRUE)

  gg_data <- .data %>%
    vac_count_grp() %>%
    vac_join_age_pop(incl_under_12 = incl_under_12) %>%
    vac_age_fct()

  gg_data %>%
    vac_age_ggplot(by_pop = by_pop) %>%
    set_covid_theme() %>%
    vac_age_axis_limits(by_pop = by_pop) %>%
    add_vac_age_axis_labels(by_pop = by_pop) %>%
    add_vac_age_col(by_pop = by_pop) %>%
    add_vac_age_col_labels() %>%
    add_vac_age_scale(by_pop = by_pop) %>%
    remove_x_grid() %>%
    add_vac_age_title_caption(by_pop = by_pop, date = date)
}

vac_age_ggplot <- function(data, by_pop) {

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[["age_grp"]],
      y = !!vac_age_choose_y(by_pop = by_pop),
      color = .data[["full"]],
      fill = .data[["full"]]
    )
  )
}

vac_age_axis_limits <- function(gg_obj, by_pop) {
  assert_bool(by_pop)
  if (by_pop) set_axis_limits(gg_obj, ylim = c(0, 1)) else gg_obj
}

vac_age_choose_y <- function(by_pop) {
  by_pop <- coviData::assert_bool(by_pop)

  if (by_pop) {
    rlang::expr(.data[["n_vac"]] / .data[["n_pop"]])
  } else {
    rlang::expr(.data[["n_vac"]] / sum(.data[["n_vac"]], na.rm = TRUE))
  }
}

remove_x_grid <- function(gg_obj) {
  gg_obj +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}

add_vac_age_col <- function(gg_obj, by_pop) {

  by_pop <- coviData::assert_bool(by_pop)

  width <- if (by_pop) 0.95 else 0.99

  gg_obj + ggplot2::geom_col(position = "identity", width = width)
}

add_vac_age_scale <- function(gg_obj, by_pop) {
  by_pop <- coviData::assert_bool(by_pop)

  if (by_pop) {
    breaks <- seq(0, 1, by = 0.1)
  } else {
    max_pct <- max(rlang::eval_tidy(
      gg_obj[["mapping"]][["y"]],
      data = gg_obj[["data"]]
    ))
    magnitude <- 10^floor(log10(max_pct))
    location <- max_pct / magnitude
    seq_by <- purrr::when(
      location,
      . <= 10/4 ~ magnitude * 0.1,
      . <= 20/4 ~ magnitude * 0.25,
      . <= 33/4 ~ magnitude * 0.5,
      ~ magnitude
    )
    breaks <- seq(0, 10*max_pct, by = seq_by)
  }

  label_fn <- purrr::partial(vac_age_label_fn, n = NULL)

  pal_indigo <- ggsci::pal_material("indigo", n = 10L, reverse = TRUE)
  pal_purple <- ggsci::pal_material("deep-purple", n = 10L, reverse = TRUE)
  indigo <- pal_indigo(6L)[[6L]]
  purple <- pal_purple(2L)[[2L]]

  gg_obj +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = scales::label_percent(1),
      minor_breaks = NULL
    ) +
    ggplot2::scale_color_manual(
      name = "Vaccination Status",
      values = c(`FALSE` = indigo, `TRUE` = purple, " " = "#f0f0f0"),
      labels = c(`FALSE` = "At least 1 dose", `TRUE` = "Fully Vaccinated"),
      aesthetics = c("color", "fill")
    )
}

add_vac_age_axis_labels <- function(gg_obj, by_pop) {
  by_pop <- coviData::assert_bool(by_pop)

  ylab <- dplyr::if_else(by_pop, "% Population", "% Vaccinations")
  add_axis_labels(gg_obj, xlab = "Age", ylab = ylab)
}

add_vac_age_col_labels <- function(gg_obj) {

  y <- gg_obj[["mapping"]][["y"]]

  gg_obj + ggplot2::geom_label(
    ggplot2::aes(
      label = vac_age_label_fn(n = .data[["n_vac"]], pct = !!y),
      color = dplyr::if_else(
        .data[["full"]],
        " ",
        as.character(.data[["full"]])
      ),
      fill  = dplyr::if_else(
        .data[["full"]],
        as.character(.data[["full"]]),
        " "
      ),
      vjust = purrr::when(.data[["full"]], . ~ . + 0.1, ~ .)
    ),
    size = 4.5,
    label.size = 0,
    show.legend = FALSE
  )
}

vac_age_label_fn <- function(
  n = NULL,
  pct = NULL,
  pct_first = TRUE
) {

  assert_bool(pct_first)

  n_is_empty <- vec_is_empty(n)
  pct_is_empty <- vec_is_empty(pct)

  if (n_is_empty && pct_is_empty) {
    rlang::abort("Either `n` or `pct` must not be empty")
  }

  lbl_k <- purrr::as_mapper(
    ~ scales::number(
      .x,
      accuracy = 1,
      scale = 1e-3,
      suffix = "k",
      big.mark = ","
    )
  )

  lbl_pct <- purrr::as_mapper(
    ~ scales::percent(
      .x,
      accuracy = if(rlang::is_integerish(.x)) 1 else 0.1,
      big.mark = ",",
      trim = TRUE
    )
  )

  empty_lbl <- vec_rep("", times = vec_size_common(n, pct))

  n_lbl   <- if (!n_is_empty)   purrr::map_chr(n, lbl_k)     else empty_lbl
  pct_lbl <- if (!pct_is_empty) purrr::map_chr(pct, lbl_pct) else empty_lbl

  if (n_is_empty) {
    pct_lbl
  } else if (pct_is_empty) {
    n_lbl
  } else if (pct_first) {
    paste0(pct_lbl, " (", n_lbl, ")")
  } else {
    paste0(n_lbl, " (", pct_lbl, ")")
  }
}

add_vac_age_title_caption <- function(gg_obj, by_pop, date) {

  title <- dplyr::case_when(
     by_pop ~ "Population Vaccinated by Age",
    !by_pop ~ "People Vaccinated by Age"
  )
  subtitle <- format(as.Date(date), "%B %d, %Y")

  caption <- "Data Source: Tennessee Immunization Information System (TennIIS)"
  add_title_caption(
    gg_obj,
    title = title,
    subtitle = subtitle,
    caption = caption
  )
}

vac_count_grp <- function(.data) {
  .data %>%
    vac_filter_residents() %>%
    dplyr::transmute(

      full = .data[["recip_fully_vacc"]] %in% TRUE,
      age_grp = .data[["age_at_admin"]] %>% std_age() %>% vac_age_grp()
    ) %>%
    dplyr::count(.data[["full"]], .data[["age_grp"]])
}

std_age <- function(x) {
  x_dbl <- as.double(x)
  dplyr::if_else(0 <= x_dbl & x_dbl < 115, x_dbl, NA_real_)
}

vac_age_grp <- function(dbl) {

  vctrs::vec_assert(dbl, ptype = double())

  breaks <- c(0, 12, 16, seq(25, 75, by = 10), 115)
  lbls <- c(
     "0-11",
    "12-15",
    "16-24",
    "25-34",
    "35-44",
    "45-54",
    "55-64",
    "65-74",
    "75+"
  )

  cut(
    dbl,
    breaks = breaks,
    labels = lbls,
    right = FALSE,
    ordered_result = TRUE
  ) %>% as.character()
}

vac_join_age_pop <- function(.data, incl_under_12 = FALSE) {
  pop_age <- covidReport::pop_2019 %>%
    dplyr::mutate(
      age_grp = .data[["age"]] %>%
        as.double() %>%
        vac_age_grp()
    ) %>%
    dplyr::group_by(.data[["age_grp"]]) %>%
    dplyr::summarize(n = sum(.data[["population"]]))
  .data %>%
    dplyr::left_join(
      pop_age,
      by = "age_grp",
      suffix = c("_vac", "_pop")
    ) %>%
    purrr::when(
      incl_under_12 ~ .,
      ~ dplyr::filter(., .data[["age_grp"]] != "0-11")
    )
}

vac_age_fct <- function(.data) {
  dplyr::mutate(
    .data,
    age_grp = factor(.data[["age_grp"]], ordered = TRUE)
  )
}
