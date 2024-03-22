#' validate chronogram
#'
#' @param x a chronogram object (class cg_tbl)
#' @return TRUE if valid
#'
validate_chronogram <- function(x) {
  stopifnot(
    "Invalid chronogram: Wrong class. Create with 'chronogram()'" =
      inherits(x, "cg_tbl") == TRUE
  )


  stopifnot(
    "Invalid chronogram: it must contain a date column" = {
      (!is.na(attributes(x)$col_calendar_date)) &
        attributes(x)$col_calendar_date %in% attributes(x)$names
    }
  )

  stopifnot(
    "Invalid chronogram: it must contain an ID column" = {
      (!is.na(attributes(x)$col_ids)) &
        attributes(x)$col_ids %in% attributes(x)$names
    }
  )

  stopifnot("Invalid chronogram: Dates are duplicated for IDs" = {
    ids <- attributes(x)$col_ids
    cal_dates <- attributes(x)$col_calendar_date

    z <- x %>%
      tibble::as_tibble() %>%
      dplyr::group_by(
        dplyr::pick(c(attributes(x)$col_ids, attributes(x)$col_calendar_date))
      ) %>%
      dplyr::tally()

    max(z$n, na.rm = TRUE) == 1
  })
  return(TRUE)
}
