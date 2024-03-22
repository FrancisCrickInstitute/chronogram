#' validate chronogram skeleton
#'
#' @param x a chronogram skeleton object (class cg_skl_tbl)
#'
#' @return Errors, or TRUE if valid cg_skl_tbl
#'
validate_chronogram_skeleton <- function(x) {
  stopifnot(
    "Invalid cg_skeleton: Wrong class. Use 'chronogram_skeleton()'" =
      inherits(x, "cg_skl_tbl") == TRUE
  )

  stopifnot(
    "Invalid cg_skeleton: col_id attribute not a column name" =
      attributes(x)$col_ids %in% attributes(x)$names
  )

  stopifnot(
    "Invalid cg_skeleton: col_calendar_date attribute not a column name" =
      attributes(x)$col_calendar_date %in% attributes(x)$names
  )

  stopifnot(
    "Invalid cg_skeleton: cg_pkg_version missing.
    Use 'chronogram_skeleton()'" =
      !is.na(attributes(x)$cg_pkg_version)
  )


  stopifnot(
    "Invalid chronogram_skeleton: it must contain two columns only" =
      ncol(x) == 2
  )

  stopifnot(
    "Invalid chronogram_skeleton:
    one (and only 1) column must contain dates,
    which are date class (not other date/time classes eg dttm)" = {
      variable <- NULL

      x %>%
        ## class of all columns ##
        dplyr::summarise_all(class) %>%
        ## count for each type of class ##
        tidyr::gather(variable, class) %>%
        ## filter to exact match "Date" ##
        dplyr::filter(class == "Date") %>%
        ## check that tally is 1
        dplyr::tally() == 1
    }
  )

  stopifnot(
    "Invalid chronogram_skeleton:
    col_calendar_date column must contain dates,
    which are date class (not other date/time classes eg dttm)" = {
      y <- x %>%
        ## class of all columns ##
        dplyr::pull(attributes(x)$col_calendar_date)
      inherits(y, "Date")
    }
  )

  return(TRUE)
}
