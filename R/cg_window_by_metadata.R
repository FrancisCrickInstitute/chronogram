#' Take a window of dates from a chronogram
#'
#' Works with respect to a column (eg date of dose 3).
#'
#' @param cg a chronogram
#' @param windowing_date_col the column containing reference date (must repeat
#'  the date for each row of that individual's record).
#' @param preceding_days used as filter( date > (windowing_date_col -
#'  preceding_days) )
#' @param following_days used as filter( date < (windowing_date_col +
#'  following_days) )
#'
#'
#' @return A windowed chronogram
#' @seealso [chronogram::cg_window_by_episode()],
#'  [chronogram::cg_window_by_visit()]
#' @export
#'
#' @examples
#' \dontrun{
#' SevenDaysPrePostDose3 <- cg_window_by_metadata(
#'   annotatedChronogram, "date_dose_3", 7, 7
#' )
#' }
#'
cg_window_by_metadata <- function(cg,
                                  windowing_date_col,
                                  preceding_days,
                                  following_days) {
  calendar_date <- attributes(cg)$col_calendar_date
  calendar_date <- rlang::enquo(calendar_date)
  ids_column_name <- attributes(cg)$col_ids


  y <- cg %>%
    dplyr::filter(
      .data[[{{ calendar_date }}]] >
        {{ windowing_date_col }} - preceding_days
    ) %>%
    dplyr::filter(
      .data[[{{ calendar_date }}]] <
        {{ windowing_date_col }} + (following_days + 1)
    )

  # ## set attributes ##
  # attributes(y)$col_calendar_date <- attributes(cg)$col_calendar_date
  # attributes(y)$col_ids <- attributes(cg)$col_ids
  # attributes(y)$cg_pkg_version <- attributes(cg)$cg_pkg_version
  # attributes(y)$cols_metadata <- attributes(cg)$cols_metadata

  ## set windowed attribute to TRUE ##
  attributes(y)$windowed <- TRUE

  attributes(y) <- attributes(y)

  validate_chronogram(y)

  return(y)
}
