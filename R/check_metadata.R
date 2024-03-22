#' check metadata
#'
#' @param x a tibble containing metadata
#' @param cg_skeleton chronogram skeleton
#'
#' @return TRUE or stop with message
#'
check_metadata <- function(x, cg_skeleton = NULL) {
  ### metadata must be a tibble
  stopifnot(
    "Invalid metadata: not a tibble" =
      inherits(x, "tbl_df") == TRUE
  )

  calendar_date <- attributes(cg_skeleton)$col_calendar_date
  ids_column_name <- attributes(cg_skeleton)$col_ids

  ### does NOT have a calendar_date column
  stopifnot(
    "Invalid metadata: a 'calendar_date' column cannot exist in metadata" =
      calendar_date %in% colnames(x) == FALSE
  )

  ### has a column that matches ids_column_name
  if (!is.null(ids_column_name)) {
    stopifnot(
      "Invalid metadata: provided ids_column_name is not present" =
        ids_column_name %in% colnames(x) == TRUE
    )
  }

  return(TRUE)
}
