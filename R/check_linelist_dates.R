check_linelist_dates <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date(),
  quiet = FALSE
) {

  collect_nm <- select_colnames(.data, .collection_date)
  report_nm <- select_colnames(.data, .report_date)

  assert_cols(.data, collect_nm, ptype = lubridate::Date(), n = 1L)
  assert_cols(.data, report_nm, ptype = lubridate::Date(), n = 1L)

  .data %T>%
    {if (!quiet) rlang::inform("Removing invalid dates")} %>%
    tidylog::filter(
      .data[[collect_nm]] >= as.Date("2020-03-05"),
      .data[[collect_nm]] <= today,
      .data[[report_nm]] >= as.Date("2020-03-05"),
      .data[[report_nm]] <= today
    ) %T>%
    {if (!quiet) rlang::inform("Removing illogical dates combinations")} %>%
    tidylog::filter(.data[[collect_nm]] <= .data[[report_nm]])
}
