#' Convert `flextable`, `gt_tbl`, and HTML `character` Objects to a `tibble`
#'
#' `as_tbl()` first converts objects to HTML, then extracts the HTML table from
#' the result
#'
#' @param x A `flextable`, `gt_tbl`, or `character` vector defining an HTML
#'   table
#'
#' @return A `tibble`
#'
#' @export
#'
#' @keywords internal
as_tbl <- function(x) {
  UseMethod("as_tbl")
}

#' @rdname as_tbl
#'
#' @export
as_tbl.gt_tbl <- function(x) {
  x %>%
    gt::as_raw_html() %>%
    as_tbl()
}

#' @rdname as_tbl
#'
#' @export
as_tbl.flextable <- function(x) {
  x %>%
    flextable::flextable_to_rmd() %>%
    xml2::read_html()
}

#' @rdname as_tbl
#'
#' @export
as_tbl.character <- function(x) {
  suppressMessages({
    x %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      dplyr::as_tibble(.name_repair = "unique") %>%
      dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(.x, ""))) %>%
      janitor::remove_empty(c("rows", "cols")) %>%
      purrr::when(
        !any(stringr::str_detect(colnames(.), "[.]{3}")) ~ .,
        ~ set_colnames(., .[1L,]) %>% dplyr::filter(dplyr::row_number() != 1L)
      )
  })
}
