#' print chronogram skeleton
#'
#' @param x a chronogram skeleton object (class cg_skl_tbl)
#' @param ... additional arguments passed to tibble::print S3 generic
#'
#' @return print to console
#'
#' @export
#' @examples
#' \dontrun{
#' small.study <- chronogram_skeleton(
#'   ids = c(1, 2, 3),
#'   start_date = "01012020",
#'   end_date = "10102021"
#' )
#'
#' ## print, with default tibble options ##
#' small.study
#'
#' ## print, with eg 3 rows ##
#' print(small.study, n = 3)
#' }
print.cg_skl_tbl <- function(
    x, ...) {
  calendar_date <- attributes(x)$col_calendar_date
  ids_column_name <- attributes(x)$col_ids
  version_number <- attributes(x)$cg_pkg_version

  cat(
    ## use paste to prevent cat re-formatting dates
    paste(
      "A chronogram skeleton:\n",
      "Dates column: ", calendar_date, "\n",
      "IDs column:   ", ids_column_name, "\n",
      "Starts on:    ",
      min(x %>% dplyr::pull(calendar_date)), "\n",
      "Ends on:      ",
      max(x %>% dplyr::pull(calendar_date)), "\n",
      "Spanning:     ",
      max(x %>%
        dplyr::pull(calendar_date)) -
        min(x %>% dplyr::pull(calendar_date)) + 1,
      " days\n",
      "Contains:     ",
      x %>%
        dplyr::pull(ids_column_name) %>%
        factor(.) %>% nlevels(.),
      " unique participant IDs\n",
      "Size:         ",
      rlang::parse_bytes(
        as.character(
          lobstr::obj_size(x)
        )
      ), "\n",
      "Pkg_version:  ",
      version_number, "[used to build this cg]\n\n"
    )
  )

  NextMethod(x, ...)
}
