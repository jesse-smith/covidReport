#' Filter Case Data to Active Cases
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process-nbs]{pos(process_inv())}}
#'
#' @param days `numeric`. The number of days a case is considered active
#'
#' @param date The download date of the data; defaults to most recent
#'
#' @return The input `data` filtered to rows containing deaths
#'
#' @keywords internal
filter_active <- function(
  data,
  days = 14L,
  date = NULL
) {
  date <- date_inv(date)
  # Onset date of first known case
  start_dt <- lubridate::as_date("2020-02-29")
  a_cols <- c("illness_onset_dt", "specimen_coll_dt", "inv_start_dt")

  data %>%
    dplyr::mutate(
      .id_d_tmp_ = dplyr::row_number(),
      .active_dt_tmp_ = dplyr::across(
        {{ a_cols }},
        ~ coviData::std_dates(.x, orders = "ymdT", train = FALSE, force = "dt")
      ) %>%
        dplyr::transmute(
          dplyr::across(
            {{ a_cols }},
            ~ dplyr::if_else(
              {{ start_dt }} <= .x & .x <= {{ date }},
              .x,
              lubridate::NA_Date_
            )
          ),
          dt = coviData::coalesce_across({{ a_cols }})
        ) %>%
        dplyr::pull("dt"),
      .active_days_tmp_ = as.integer({{ date }} - .data[[".active_dt_tmp_"]]),
      .a_tmp_ = dplyr::between(.data[[".active_days_tmp_"]], 0L, {{ days }})
    ) %>%
    dplyr::filter(
      .data[[".a_tmp_"]],
      !.data[[".id_d_tmp_"]] %in% filter_deaths(.)[[".id_d_tmp_"]]
    ) %>%
    dplyr::select(
      -c(".id_d_tmp_", ".active_dt_tmp_", ".active_days_tmp_", ".a_tmp_")
    )
}
