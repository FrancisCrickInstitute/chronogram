#' Select a visit within a windowed chronogram
#'
#' @param cg a windowed chronogram
#' @param visit_col a character vector to use to label the column with
#'   either dates of visits, or results. This is usually an
#'   experimental data column. Default is "NULL".
#' @param visit either "earliest" or "latest"
#'
#' @return A subsetted chronogram
#' @export
#'
#' @examples
#' \dontrun{
#' SevenDaysPrePostDose3 <- cg_window_by_metadata(
#'   annotatedChronogram, "date_dose_3", 7, 7
#' )
#' SevenDaysPrePostDose3 <- cg_select_visit(
#'   SevenDaysPrePostDose3, "earliest"
#' )
#' }
#'
cg_select_visit <- function(cg,
                            visit_col = NULL,
                            visit = c("earliest", "latest")) {
  stopifnot(
    "cg is not a valid chronogram" =
      validate_chronogram(cg)
  )

  calendar_date <- attributes(cg)$col_calendar_date
  ids_column_name <- attributes(cg)$col_ids

  stopifnot(
    "cg not windowed: use a cg_window family function first" =
      (attributes(cg)$windowed == TRUE)
  )

  if (visit == "earliest") {
    y <- cg %>%
      dplyr::group_by(.data[[{{ ids_column_name }}]]) %>%
      dplyr::filter(!is.na({{ visit_col }})) %>%
      dplyr::arrange(calendar_date) %>%
      dplyr::slice_head() %>%
      dplyr::ungroup()
  }

  if (visit == "latest") {
    y <- cg %>%
      dplyr::group_by(.data[[{{ ids_column_name }}]]) %>%
      dplyr::filter(!is.na({{ visit_col }})) %>%
      dplyr::arrange(calendar_date) %>%
      dplyr::slice_tail() %>%
      dplyr::ungroup()
  }

  # ## set attributes, including class ##
  attributes(y)$class <- attributes(cg)$class
  attributes(y)$col_calendar_date <- attributes(cg)$col_calendar_date
  attributes(y)$col_ids <- attributes(cg)$col_ids
  attributes(y)$cg_pkg_version <- attributes(cg)$cg_pkg_version
  attributes(y)$windowed <- attributes(cg)$windowed
  attributes(y)$cols_metadata <- attributes(cg)$cols_metadata

  stopifnot(validate_chronogram(y))

  return(y)
}
