#' Load Inspections Data
#'
#' `insp_load_data()` loads the data from an excel workbook into the R
#' session. standardizes column names, and optionally checks that the column
#' names match expected values.
#'
#' If `check = TRUE`, `insp_load_data()` expects at least five columns
#' giving information on the (last?) inspection visit date, the business name,
#' the business address, the number of total violations by the business, and the
#' closure date (if it exists). The expected values of the column names are
#' formulated as generally as possible to allow for human error. The columns are
#' matched as follows:
#'
#' \itemize{
#'   \item \strong{inspection visit date} must contain the words \emph{"date"}
#'     and \emph{"visit"}
#'   \item \strong{business name} must contain the words \emph{"name"} and
#'     \emph{"business"}
#'   \item \strong{business address} must contain the word \emph{"address"}
#'   \item \strong{number of violations} must contain the word
#'     \emph{"violations"}
#'   \item \strong{closure date} must contain the string \emph{"clos"} and the
#'     word \emph{"date"}
#' }
#'
#' @param path The path to the excel workbook containing inspections data
#'
#' @param check Should column names be checked? See \strong{Details} for more
#'   information on name checking.
#'
#' @return A `tibble` containing the inspections data
#'
#' @seealso Other functions in the inspections data pipeline (
#'   \code{\link[covidReport:insp_prep_data]{insp_prep_data()}},
#'   \code{
#'   \link[covidReport:insp_create_table]{insp_create_table()}
#'   },
#'   \code{\link[covidReport:insp_save_table]{insp_save_table()}},
#'   \code{
#'   \link[covidReport:insp_archive_table]{insp_archive_table()}
#'   }) and the wrapper for the full workflow
#'   (\code{
#'   \link[covidReport:insp_table_pipeline]{insp_table_pipeline()}
#'   })
#'
#' @export
insp_load_data <- function(
  path = coviData::path_create(
    "V:/Compliance/Inspection Data for Publishing/",
    "Grand List of Inspections",
    ext = "xlsx"
  ),
  check = TRUE
) {

  path <- coviData::path_create(path)
  guess_max <- .Machine$integer.max %/% 100L

  readxl::read_excel(path, guess_max = guess_max, sheet = "Sheet1") %>%
    janitor::clean_names() %T>%
    {if (check) insp_cols_exist(.) else .}

}

#' Prepare Inspections Data for Displaying in HTML Table
#'
#' `insp_prep_data()` prepares data from the inspections team for display
#' as an HTML table. It selects, transforms, orders, and sorts the five columns
#' needed for display and outputs a visualization-ready dataset.
#'
#' `insp_prep_data()` chains together a number of operations on the input
#' data.
#'
#' First, columns are selected as defined in the \strong{Details} of
#' \code{\link[covidReport:insp_cols_exist]{insp_cols_exist()}};
#' if multiple columns match this selection, they are
#' \code{\link[coviData:coalesce_across]{coalesced}}.
#'
#' Second, columns are transformed according to type and assigned a standard
#' name. Visit date and closure date are assigned to `dt_visit` and `dt_closed`,
#' respecively, and converted to the ISO 8601 standard (see
#' \code{\link[janitor:convert_to_date]{convert_to_date()}},
#' \code{
#' \link[covidReport:insp_cast_chr_date]{insp_cast_chr_date()}
#' }, and
#' \code{
#' \link[covidReport:insp_replace_year_long]{
#' insp_replace_year_long()
#' }}). Name and address columns are assigned to `nm_business` and
#' `addr_business`, respectively, and converted to character with
#' extra whitespace removed. The number of violations is assigned to
#' `n_violations` and converted to integer.
#'
#' Third, rows missing a business name or number of violations (`nm_business`
#' or `n_violations`) are removed from the dataset. Entries without names are
#' not easily identifiable, and the primary purpose of the table is to display
#' violations.
#'
#' Fourth, columns are reordered for ease of viewing, and unnecessary columns
#' (any not listed above) are dropped. The ordering is `n_violations`,
#' `nm_business`, `addr_business`, `dt_visit`, and `dt_closed`.
#'
#' Last, rows are arranged by column from left to right (first by
#' `n_violations`, then by `nm_business`, and so on). `n_violations`,
#' `dt_visit`, and `dt_closed` are arranged in descending order (most violations
#' and most recent dates first); `nm_business` and `addr_business` are arranged
#' in ascending order (aka alphabetically).
#'
#' @param .data A data frame containing the inspections data
#'
#' @return The processed data frame
#'
#' @seealso Other functions in the inspections data pipeline (
#'   \code{
#'   \link[covidReport:insp_load_data]{insp_load_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_create_table]{insp_create_table()}
#'   },
#'   \code{
#'   \link[covidReport:insp_save_table]{insp_save_table()}
#'   },
#'   \code{
#'   \link[covidReport:insp_archive_table]{insp_archive_table()}
#'   }) and the wrapper for the full workflow
#'   (\code{
#'   \link[covidReport:insp_table_pipeline]{insp_table_pipeline()}
#'   })
#'
#' @export
insp_prep_data <- function(.data) {
  .data %>%
    tidylog::mutate(
      dt_visit = coviData::coalesce_across(
        dplyr::contains("date") & dplyr::contains("visit")
      ) %>%
        janitor::convert_to_date(character_fun = insp_cast_chr_date),
      nm_business = coviData::coalesce_across(
        dplyr::contains("name") & dplyr::contains("business")
      ) %>%
        as.character() %>%
        stringr::str_squish(),
      addr_business = coviData::coalesce_across(
        dplyr::contains("address")
      ) %>%
        as.character() %>%
        stringr::str_squish(),
      n_violations = coviData::coalesce_across(
        dplyr::contains("violations")
      ) %>%
        stringr::str_squish() %>%
        as.integer(),
      dt_closed = coviData::coalesce_across(
        dplyr::contains("clos") & dplyr::contains("date")
      ) %>%
        coviData::std_dates() %>%
        format("%m/%d/%Y") %>%
        stringr::str_replace_na("")
    ) %>%
    # Entries with no name or number of violations aren't useful to show
    tidylog::filter(
      !is.na(.data[["n_violations"]]),
      !is.na(.data[["nm_business"]])
    ) %>%
    # Only want to display these columns, in this order
    dplyr::select(
      .data[["n_violations"]],
      .data[["nm_business"]],
      .data[["addr_business"]],
      .data[["dt_visit"]],
      .data[["dt_closed"]]
    ) %>%
    # Arrange for viewing
    dplyr::arrange(
      dplyr::desc(.data[["n_violations"]]),
      .data[["nm_business"]],
      .data[["addr_business"]],
      dplyr::desc(.data[["dt_visit"]]),
      dplyr::desc(.data[["dt_closed"]])
    )
}

#' Create HTML Table of COVID-19 Business Inspections Results
#'
#' `insp_create_table()` takes prepared inspections data and creates an
#' HTML table for publishing on the web (using the \strong{DT} package). Data
#' should first be prepared using
#' \code{\link[covidReport:insp_prep_data]{insp_prep_data()}}.
#'
#' @param .data The prepared inspections data
#'
#' @return A `reactable` object
#'
#' @seealso Other functions in the inspections data pipeline (
#'   \code{
#'   \link[covidReport:insp_load_data]{insp_load_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_prep_data]{insp_prep_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_save_table]{insp_save_table()}
#'   },
#'   \code{
#'   \link[covidReport:insp_archive_table]{insp_archive_table()}
#'   }) and the wrapper for the full workflow
#'   (\code{
#'   \link[covidReport:insp_table_pipeline]{insp_table_pipeline()}
#'   })
#'
#' @export
insp_create_table <- function(.data) {
  # Create column definitions
  def_n_violations <- reactable::colDef(
    name = "Violations",
    style = violation_style
  )
  def_nm_business <- reactable::colDef(name = "Business")
  def_addr_business <- reactable::colDef(name = "Address")
  def_dt_visit <- reactable::colDef(
    name = "Date Visited",
    format = reactable::colFormat(date = TRUE)
  )
  def_dt_closed <- reactable::colDef(name = "Date Closed")

  cols <- list(
    n_violations = def_n_violations,
    nm_business = def_nm_business,
    addr_business = def_addr_business,
    dt_visit = def_dt_visit,
    dt_closed = def_dt_closed
  )

  reactable::reactable(
    .data,
    columns = cols,
    resizable = TRUE,
    filterable = TRUE,
    showPageSizeOptions = TRUE,
    highlight = TRUE,
    striped = TRUE,
    showSortable = TRUE,
    showPageInfo = FALSE
  )
}

#' Save an HTML Table Produced by `insp_create_table()`
#'
#' `insp_save_table()` saves a `reactable` object to the specified
#' `path`. The defaults are intended for saving the inspections data table.
#'
#' This function replies on \code{\link[htmlwidgets:saveWidget]{saveWidget()}},
#' but replaces the base64 encoded dependencies in the self-contained file with
#' the human-readable equivalent.
#'
#' @param .table A `datatables` object
#'
#' @param path The location to save the table
#'
#' @param force Should an existing file at `path` be overwritten if a
#'   conflict occurs?
#'
#' @return The input `.table` (invisibly)
#'
#' @seealso Other functions in the inspections data pipeline (
#'   \code{
#'   \link[covidReport:insp_load_data]{insp_load_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_prep_data]{insp_prep_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_create_table]{insp_create_table()}
#'   },
#'   \code{
#'   \link[covidReport:insp_archive_table]{insp_archive_table()}
#'   }) and the wrapper for the full workflow
#'   (\code{
#'   \link[covidReport:insp_table_pipeline]{insp_table_pipeline()}
#'   })
#'
#' @export
insp_save_table <- function(
  .table,
  path = coviData::path_create(
    "V:/Compliance/Inspection Data for Publishing/Table/",
    paste0("insp_table_", Sys.Date()),
    ext = "html"
  ),
  force = FALSE
) {

  path <- coviData::path_create(path)

  if (fs::file_exists(path) && !force) {
    rlang::abort("File already exists. To overwrite, set `force = TRUE`.")
  }

  temp_file <- fs::file_temp("insp_table", ext = "html")
  on.exit(fs::file_delete(temp_file), add = TRUE)

  htmlwidgets::saveWidget(
    .table,
    file = temp_file,
    selfcontained = TRUE,
    title = "COVID-19 Business Inspections"
  )

  readr::read_lines(temp_file) %>%
    insp_convert_base64() %>%
    readr::write_lines(path)

  invisible(.table)
}

#' Create and Manage Inspection Table Archive Files
#'
#' `insp_archive_table()` creates a backup of the file at `table_path`
#' and ensures that only one file matching `table_pattern` is in the directory
#' specified in `table_path`. It also ensures that backups are kept to a
#' reasonable number in `archive_dir` (at least seven at all times, possibly
#' more if multiple backups per day were made in the last seven days).
#'
#' @param table_path The path to the table to archive
#'
#' @param archive_dir The path to the archive directory
#'
#' @param table_pattern The pattern to search for when removing old tables from
#'   the directory in `table_path`
#'
#' @param force Should any existing file in `archive_dir` be overwritten if
#'   there is a conflict?
#'
#' @return `table_path` (invisibly)
#'
#' @seealso Other functions in the inspections data pipeline (
#'   \code{
#'   \link[covidReport:insp_load_data]{insp_load_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_prep_data]{insp_prep_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_create_table]{insp_create_table()}
#'   },
#'   \code{
#'   \link[covidReport:insp_save_table]{insp_save_table()}
#'   }) and the wrapper for the full workflow
#'   (\code{
#'   \link[covidReport:insp_table_pipeline]{insp_table_pipeline()}
#'   })
#'
#' @export
insp_archive_table <- function(
  table_path = coviData::path_create(
    "V:/Compliance/Inspection Data for Publishing/Table/",
    paste0("insp_table_", Sys.Date()),
    ext = "html"
  ),
  archive_dir = "V:/Compliance/Inspection Data for Publishing/Table/Archive/",
  table_pattern = "insp_table_.*html",
  force = FALSE
) {

  # Split and combine paths in various ways to prep for archiving
  table_path <- coviData::path_create(table_path)
  archive_dir <- coviData::path_create(archive_dir)
  table_file <- fs::path_file(table_path)
  table_dir <- fs::path_dir(table_path)
  archive_path <- coviData::path_create(archive_dir, table_file)

  # Create copy of table in archive
  fs::file_copy(path = table_path, new_path = archive_path, overwrite = force)

  # Make sure only the most recent table is in the "Table" directory
  insp_trim_tables(
    table_dir = table_dir,
    table_pattern = table_pattern,
    min_tables = 1L
  )

  # Clean up backups as well
  insp_trim_archive(
    archive_dir = archive_dir,
    min_backups = 7L
  )
}

# Wrapper Function #############################################################

#' Run the Full Inspections Table Pipeline
#'
#' This is a simplified wrapper for the inspections table pipeline that makes
#' it easier to run. This takes inspections data from load to finished product
#' and cleans up after itself. See the component functions for details on the
#' pipeline.
#'
#' @param data_path The path to the excel workbook containing inspections data
#'
#' @param table_path The location to save the inspections table
#'
#' @param archive_dir The path to the archive directory for inspections tables
#'
#' @param force Should existing files at `table_path` or `archive_dir` be
#'   overwritten if a conflict occurs?
#'
#' @return The inspections table as a `datatables` object (invisibly)
#'
#' @seealso The component functions of the inspections table pipeline:
#'   \code{
#'   \link[covidReport:insp_load_data]{insp_load_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_prep_data]{insp_prep_data()}
#'   },
#'   \code{
#'   \link[covidReport:insp_create_table]{insp_create_table()}
#'   },
#'   \code{
#'   \link[covidReport:insp_save_table]{insp_save_table()}
#'   },
#'   \code{
#'   \link[covidReport:insp_archive_table]{insp_archive_table()}
#'   }
#'
#' @export
insp_table_pipeline <- function(
  data_path = coviData::path_create(
    "V:/Compliance/Inspection Data for Publishing/",
    "Grand List of Inspections",
    ext = "xlsx"
  ),
  table_path = coviData::path_create(
    "V:/Compliance/Inspection Data for Publishing/Table/",
    paste0("insp_table_", Sys.Date()),
    ext = "html"
  ),
  archive_dir = "V:/Compliance/Inspection Data for Publishing/Table/Archive/",
  force = FALSE
) {

  rlang::inform("Loading inspections data...")
  table_data_raw <- insp_load_data(path = data_path)

  rlang::inform("Preparing inspections data...")
  table_data_prepped <- insp_prep_data(table_data_raw)

  rlang::inform("Creating inspections table...")
  table <- insp_create_table(table_data_prepped)

  rlang::inform("Saving inspections table...")
  insp_save_table(table, path = table_path, force = force)

  rlang::inform("Managing inspections archive...")
  insp_archive_table(
    table_path = table_path,
    archive_dir = archive_dir,
    force = force
  )

  rlang::inform("Done.")
  invisible(table)
}

# Helpers ----------------------------------------------------------------------

#' Inspections Violations Color Helper
#'
#' Map number of violations to a color (green, orange, or red). Invalid values
#' default to black (`"#000000"`).
#'
#' @param value A scalar or vector numeric. This function *can* accept vectors;
#'   however, \code{\link[reactable:colDef]{colDef()}} passes scalars.
#'
#' @return A hex color code
violation_style <- function(value) {
  color <- dplyr::case_when(
    !is.numeric(value) ~ "#000000",
    value < 0L ~ "#000000",
    value < 2L ~ "#008000",
    value < 4L ~ "#ff8c00",
    value < Inf ~ "#e00000",
    TRUE ~ "#000000"
  )

  list(color = color, fontWeight = "bold")
}

decode_base64 <- function(string) {
  string %>%
    jsonlite::base64_dec() %>%
    rawToChar()
}

insp_convert_base64 <- function(html) {

  is_b64_script <- stringr::str_detect(html, "<script.+base64,")

  b64 <- html[is_b64_script] %>%
    stringr::str_remove("<script.+base64,") %>%
    stringr::str_extract("[^\">]+")

  chr <- purrr::map_chr(b64, decode_base64)

  vctrs::vec_assign(
    html,
    i = is_b64_script,
    value = paste0("<script>", chr, "</script>")
  ) %>% insp_convert_null()
}

insp_convert_null <- function(html) {
  is_data <- stringr::str_detect(
    html,
    "<script type=\"application/json\" data-for"
  )

  data_converted <- stringr::str_replace_all(
    html[is_data],
    '(?<=,)(null|["]{2})|(null|["]{2})(?=,)',
    replacement = '" "'
  )

  vctrs::vec_assign(html, i = is_data, value = data_converted)
}


#' Replace Malformed Years in Inspections Data
#'
#' `insp_replace_year_long()` replaces numeric years which only have
#' three digits, rather than the expected four (i.e. `202` instead of `2020`).
#' The replacement value is the current year. This function is intended for
#' use inside
#' \code{
#' \link[covidReport:insp_cast_chr_date]{insp_cast_chr_date()}
#' }.
#'
#' @param x A vector of dates
#'
#' @param quiet Should a warning be issued when malformed dates are detected?
#'
#' @return The corrected date vector
#'
#' @seealso The calling function
#'   \code{
#'   \link[covidReport:insp_cast_chr_date]{insp_cast_chr_date()}
#'   }, as well as higher-level functions
#'   \code{\link[janitor:convert_to_date]{convert_to_date()}} and
#'   \code{
#'   \link[covidReport:insp_prep_data]{insp_prep_data()}
#'   }
#'
#' @keywords internal
insp_replace_year_long <- function(x, quiet = FALSE) {

  year_malformed <- "[0-9]{3,}"

  current_year <- Sys.Date() %>% lubridate::year() %>% as.character()

  malformed_values <- x %>%
    stringr::str_detect(year_malformed) %>%
    magrittr::extract(x = x)

  n_malformed <- vctrs::vec_size(malformed_values)

  if (n_malformed > 0 & !quiet) {
    msg <- paste0(paste0(malformed_values, collapse = ", "), " was malformed")
    rlang::warn(message = msg)
  }

  stringr::str_replace(
    x,
    pattern = year_malformed,
    replacement = current_year
  )
}

#' Cast Character Dates to a Standard Format
#'
#' `insp_cast_chr_date()` converts dates in a variety of character
#' formats to the ISO 8601 standard. It is intended for use as the
#' `character_fun` in \code{\link[janitor:convert_to_date]{convert_to_date()}}.
#'
#' @param x A vector of dates
#'
#' @return The standardized date vector
#'
#' @seealso The function that this is used in,
#'   \code{\link[janitor:convert_to_date]{convert_to_date()}}, as well as the
#'   higher-level function
#'   \code{
#'   \link[covidReport:insp_prep_data]{insp_prep_data()}
#'   }
#'
#' @keywords internal
insp_cast_chr_date <- function(x) {

  orders <- c("mdy", "dmy", "ymd", "dmyT", "mdyT", "ymdT")

  x %>%
    insp_replace_year_long() %>%
    lubridate::parse_date_time(orders = orders) %>%
    lubridate::as_date()
}

#' Check that the Expected Columns are Present in Inspections Data
#'
#' `insp_cols_exist()` is a wrapper around
#' \code{\link[coviData:cols_exist]{cols_exist()}} that checks for the existence
#' of columns expected in the inspections data. It is intended for use in
#' \code{\link[covidReport:insp_load_data]{insp_load_data()}}.
#'
#' @param .data A data frame containing the inspections data
#'
#' @return The input data frame
#'
#' @seealso \code{
#'   \link[covidReport:insp_load_data]{insp_load_data()}
#'   }
#'
#' @keywords internal
insp_cols_exist <- function(.data) {
  coviData::cols_exist(
    .data,
    dplyr::contains("date") & dplyr::contains("visit"),
    dplyr::contains("name") & dplyr::contains("business"),
    dplyr::contains("address"),
    dplyr::contains("violations"),
    dplyr::contains("clos") & dplyr::contains("date")
  )
}

#' Trim Table Directory for Inspections Tables
#'
#' `insp_trim_tables()` ensures that at least `min_tables` tables are
#' in the tables directory at a time, but that at most `min_tables` days worth
#' of tables are kept if there are more than `min_tables` tables. All tables
#' should be archived before removal. This function is for internal use in
#' \code{
#' \link[covidReport:insp_archive_table]{insp_archive_table()}
#' }.
#'
#' @param table_dir The directory containing the most recent inspections
#'   table(s)
#'
#' @param table_pattern The inspections table naming pattern, in regex. This
#'   prevents deletion of files which aren't inspection tables.
#'
#' @param min_tables The minimum number of tables to keep in `table_dir`
#'
#' @return `table_dir` (invisibly)
#'
#' @seealso \code{
#'   \link[covidReport:insp_archive_table]{insp_archive_table()}
#'   }, \code{\link[coviData:trim_backups]{trim_backups()}}
#'
#' @keywords internal
insp_trim_tables <- function(
  table_dir = "V:/Compliance/Inspection Data for Publishing/Table/",
  table_pattern = "insp_table_.*html",
  min_tables = 1L
) {

  table_dir <- coviData::path_create(table_dir)

  if (!fs::is_dir(table_dir)) {
    rlang::abort("`table_dir` must be an existing directory")
  }

  coviData::trim_backups(
    table_dir,
    pattern = table_pattern,
    min_backups = min_tables
  )

  invisible(table_dir)
}

#' Trim Archive Directory for Inspections Tables
#'
#' `insp_trim_archive()` ensures that at least `min_backups` backups are
#' always kept of the inspections table, but that at most `min_backups` days
#' worth of backups are kept if there are more than `min_backups` backups. This
#' function is for internal use in
#' \code{
#' \link[covidReport:insp_archive_table]{insp_archive_table()}
#' }.
#'
#' @param archive_dir The directory containing archived inspections tables
#'
#' @param min_backups The minimum number of backups to retain
#'
#' @return `archive_dir` (invisibly)
#'
#' @seealso \code{
#'   \link[covidReport:insp_archive_table]{insp_archive_table()}
#'   }, \code{\link[coviData:trim_backups]{trim_backups()}}
#'
#' @keywords internal
insp_trim_archive <- function(
  archive_dir = "V:/Compliance/Inspection Data for Publishing/Table/Archive/",
  min_backups = 7L
) {

  archive_dir <- coviData::path_create(archive_dir)

  if (!fs::is_dir(archive_dir)) {
    rlang::abort("`archive_dir` must be an existing directory")
  }

  coviData::trim_backups(
    archive_dir,
    min_backups = 7L
  )

  invisible(archive_dir)
}
