#' Filter Case Data to COVID-19 Deaths
#'
#' @param data Case data, as output by
#'   \code{\link[coviData:process_positive_people]{process_positive_people()}}
#'
#' @param col `<tidy-select>` A single, `character` indicator column for
#'   COVID-19 deaths. Deaths should be labeled `"Y"`.
#'
#' @param cols_check `<tidy-select>` Columns to check for missing data. Rows
#'   with missing data are assumed to be coded incorrectly and will not be
#'   counted as a death, even if `col` is labeled `"Y"`.
#'
#' @return The input `data` filtered to rows containing deaths
#'
#' @keywords internal
filter_deaths <- function(
  data,
  col = "die_from_illness_ind",
  cols_check = "inv_death_dt"
) {

  coviData::assert_dataframe(data)

  col <- coviData::select_colnames(data, {{ col }})
  cols_check <- coviData::select_colnames(data, {{ cols_check }})

  coviData::assert_cols(data, {{ col }}, n = 1L)
  coviData::assert_cols(data, {{ cols_check }})

  data %>%
    dplyr::mutate(
      .cols_check_tmp_ = dplyr::across({{ cols_check }}, is.na) %>%
        rowSums(na.rm = TRUE) %>%
        as.logical()
    ) %>%
    dplyr::filter(
      dplyr::if_else(
        .data[[".cols_check_tmp_"]],
        FALSE,
        toupper(.data[[col]]) %in% "Y"
      )
    ) %>%
    dplyr::select(-".cols_check_tmp_")
}
