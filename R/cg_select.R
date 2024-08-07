#' Adapted dplyr::select() to include calendar dates and ID columns
#'
#' @param cg a chronogram
#' @param ... passed to `dplyr::select()`
#'
#' @return a chronogram with selected columns, and the two columns
#'   containing calendar dates and participant IDs.
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' data("built_smallstudy")
#' 
#' cg <- built_smallstudy$chronogram
#' 
#' ## keep or drop columns using selection helpers ##-----------------
#' ## here, keep column names containing "dose"
#' cg_select(cg, contains("dose"))
#' 
#' ## Or, drop column names containing "dose"
#' cg_select(cg, ! contains("dose"))
#' 
#' ## keep or drop columns using exact names ##-----------------------
#' cg_select(cg, "dose_1")
#' # or equivalently:
#' cg_select(cg, dose_1)
#' 
#' # or several selectors together ##---------------------------------
#' cg_select(cg, dose_1, dose_2)
#' 
#' cg_select(cg, contains("dose") & contains("date"))
#' 
cg_select <- function(cg, ...) {
  
  calendar_date <- attributes(cg)$col_calendar_date
  ids_column_name <- attributes(cg)$col_ids
  meta_col_ids <- attributes(cg)$cols_metadata
  
  y <-  cg %>%
    dplyr::select(calendar_date, ids_column_name, ...)
  
  
  ## reset column names for metadata attribute ##
  meta_col_ids <- meta_col_ids[meta_col_ids %in% colnames(y)]
  attributes(y)$cols_metadata <- meta_col_ids
  
  
  stopifnot(
    "Failed to return a valid cg_tbl" =
    validate_chronogram(y))
  
  return(y)
}