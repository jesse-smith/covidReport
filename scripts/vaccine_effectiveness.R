library(magrittr)
library(ggplot2)

# Convenience smoothing function
smth_stl <- function(x, freq = 7L, trend = 30L, periodic = FALSE, remainder = FALSE) {
  x %>%
    stats::ts(frequency = freq) %>%
    stats::stl(
      s.window = if (periodic) "periodic" else freq,
      t.window = trend,
      robust = TRUE,
      na.action = na.exclude
    ) %>%
    sweep::sw_tidy_decomp() %>%
    dplyr::select(if (remainder) c("trend", "remainder") else "trend") %>%
    rowSums()
}

# Load data
inv <- coviData::pos(coviData::process_inv())
v <- coviData::vac_prep(distinct = TRUE)

# Get total cases prior to Jan 11
n_prior <- inv %>%
  dplyr::transmute(dt = specimen_coll_dt) %>%
  tidytable::filter.(
    tidytable::between.(dt, as.Date("2020-03-05"), as.Date("2021-01-10"))
  ) %>%
  NROW()

# All cases (by breakthrough status - aka fully vaccinated status)
inv_dt <- inv %>%
  # Don't convert to `tidytable` (aka `data.table`) until excess variables are removed
  # Otherwise all columns will be read for copy
  dplyr::transmute(
    # Assume that all cases not marked as breakthrough are not breakthrough
    vac = breakthrough_case %in% "Yes",
    # Use specimen collection date as end date
    dt = specimen_coll_dt
  ) %>%
  # Filter out impossible/irrelevant times and dates
  tidylog::filter(
    !is.na(dt),
    tidytable::between.(dt, as.Date("2021-01-11"), coviData::date_inv())
  ) %>%
  # Convert to data.table by reference
  data.table::setDT() %>%
  # Set key as vac & date
  data.table::setindex(vac, dt) %>%
  # Subset variables to needed
  tidytable::transmute.(dt, vac, case = TRUE) %>%
  # Summarize # of persons (*_case_n) and person-days (*_case_days) converted
  # to a case on each date (by fully vaccinated status- aka breakthrough)
  tidytable::summarize.(
    u_case_n = sum(!vac),
    v_case_n = sum(vac),
    .by = "dt"
  ) %>%
  # Fill in missing dates with zeros
  tidytable::complete.(
    dt = seq(min(.[["dt"]]), max(.[["dt"]]), by = 1),
    fill = list(
      u_case_n = 0,
      v_case_n = 0,
      u_case_days = 0,
      v_case_days = 0
    )
  ) %>%
  # Set key as date
  data.table::setkey(dt)

# All fully vaccinated
v_dt <- v %>%
  # Don't convert to `tidytable` (aka `data.table`) until excess variables are removed
  # Otherwise all columns will be read for copy
  # Only interested in those who have all doses
  dplyr::filter(recip_fully_vacc) %>%
  # Need 14 days post-all-doses to get date "fully vaccinated"
  dplyr::transmute(dt = lubridate::mdy(vacc_date) + 14) %>%
  # Convert to data.table by reference
  data.table::setDT() %>%
  # Set date as key
  data.table::setkey(dt) %>%
  # Really only interested in fully vaccinated (with valid dates)
  tidytable::filter.(
    tidytable::between.(dt, as.Date("2021-01-11"), coviData::date_vac())
  ) %>%
  # Count up new fully vaccinated on each date (not cumulative)
  tidytable::count.(dt, name = "v_all_n") %>%
  # Fill missing dates with zeros
  tidytable::complete.(
    dt = seq(min(.[["dt"]]), max(.[["dt"]]), by = 1),
    fill = list(v_all_n = 0L)
  ) %>%
  tidytable::mutate.(
    # Make people vaccinated cumulative for consistency
    v_all_n = cumsum(v_all_n)
  ) %>%
  # Ensure date is still key
  data.table::setkey(dt)

gg_data <- v_dt %>%
  # Join all vaccinated (cumulative) and all cases (incident) by date
  tidytable::left_join.(inv_dt, by = "dt") %>%
  # Ensure date is still key
  data.table::setkey(dt) %>%
  # Calculate unvaccinated susceptible population (assuming full testing)
  # This includes the people who will become cases on that date (i.e. this is unvaccinated risk set)
  # CUMULATIVE OVER TIME
  tidytable::mutate.(
    # Shelby pop - total cases before period - all vaccinations up to each date (i.e. censored)
    u_all_n = 937166 - {{ n_prior }} - data.table::shift(v_all_n, fill = 0L),
    .before = "v_all_n"
  ) %>%
  # Transform to needed data
  tidytable::transmute.(
    dt,
    # Final un-fully-vaccinated risk set at each time point (never case)
    # Current cumulative unvaccinated - previous cumulative unvaccinated cases
    u_n = u_all_n - dplyr::lag(cumsum(u_case_n), default = 0),
    # Just rename cases
    u_case = u_case_n,
    # Final fully vaccinated risk set at each time point
    # Current cumulative vaccinated - previous cumulative vaccinated cases
    v_n = v_all_n - dplyr::lag(cumsum(v_case_n), default = 0),
    # Just rename cases
    v_case = v_case_n
  ) %>%
  # Filter to 1/12/21 - the first date someone could be fully vaccinated
  tidytable::filter.(
    tidytable::between.(dt, as.Date("2021-01-12"), coviData::date_inv()-5)
  ) %>%
  # Subset to final variables (date & rates)
  tidytable::transmute.(
    dt,
    # Case rate in unvaccinated susceptibles
    u_rate = 1e5*u_case/u_n,
    # Case rate in vaccinated susceptibles
    v_rate = 1e5*v_case/v_n
  ) %>%
  tidytable::mutate.(
    # Smooth case rates
    u_rate_smth = smth_stl(sqrt(u_rate))^2,
    v_rate_smth = smth_stl(sqrt(v_rate))^2,
    v_rate = smth_stl(sqrt(v_rate), remainder = TRUE)^2,
    u_rate = smth_stl(sqrt(u_rate), remainder = TRUE)^2,
    # Ratio (unsmoothed and smoothed)
    u_ratio = v_rate / u_rate,
    v_ratio = u_ratio,
    u_ratio_smth = v_rate_smth / u_rate_smth,
    v_ratio_smth = u_ratio_smth
  ) %>%
  # Pivot rates by vaccination status
  tidytable::pivot_longer.(
    -"dt",
    names_to = c("status", ".value"),
    names_pattern = "(.)_(.*)"
  ) %>%
  # Reset key to status/dt
  data.table::setkey(status, dt) %>%
  # Remove unstable dates
  tidytable::filter.(dt > as.Date("2021-02-28")) %>%
  # Reset status/dt as key (unsure how data.table handles change to key column)
  # Also arranges by keys
  data.table::setkey(status, dt)

plt_rt <- gg_data %>%
  {ggplot2::ggplot(., ggplot2::aes(x = dt, y = rate, color = status, fill = status)) +
    ggplot2::geom_point(size = 1, alpha = 0.2, show.legend = FALSE) +
    ggplot2::geom_line(ggplot2::aes(y = rate_smth), size = 1, show.legend = FALSE) +
    ggplot2::scale_color_manual(
      values = c(v = "cornflowerblue", u = "firebrick")
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    ggplot2::annotate(
      "label",
      x = as.Date("2021-04-22"),
      y = gg_data %>%
        dplyr::filter(dt == as.Date("2021-04-22")) %>%
        dplyr::pull("rate_smth") %>%
        add(max(gg_data$rate)/120),
      label = c("Not Fully Vaccinated", "Fully Vaccinated"),
      color = c("firebrick", "cornflowerblue"),
      fill = "#f0f0f0",
      size = 14/ggplot2::.pt,
      vjust = 0
    )
  } %>%
  coviData::set_covid_theme() %>%
  coviData::add_title_caption(
    "Case Rates in Population at Risk",
    subtitle = paste0("By Vaccination Status (", format(min(coviData::date_inv(), coviData::date_vac()), "%m/%d/%Y"), ")"),
    caption = paste0(
      "Population at risk: Number of p'eople in group on a given day,",
      " excluding known cases in the group up to that day"
    )
  ) %>%
  coviData::add_axis_labels(ylab = "Cases per 100k") %>%
  coviData::add_scale_month()

plt_ef <- gg_data %>%
  {ggplot2::ggplot(., ggplot2::aes(x = dt, y = 1 - ratio, group = status)) +
    ggplot2::geom_point(color = "darkorchid", size = 1, alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = 1 - ratio_smth), size = 1, color = "darkorchid") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::label_percent(accuracy = 1))
  } %>%
  coviData::set_covid_theme() %>%
  coviData::add_axis_labels(ylab = "% Case Rate Reduction") %>%
  coviData::set_axis_limits(ylim = c(0.75, 1)) %>%
  coviData::add_title_caption(
    title = "Vaccine Effectiveness Over Time"
  )

