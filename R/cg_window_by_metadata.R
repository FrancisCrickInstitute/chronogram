#' Pick a window before and after a metadata date column
#'
#' @param cg a chronogram
#' @param windowing_date_col the column containing reference date (must repeat
#'  the date for each row of that individual's record).
#' @param preceding_days used as `filter( date > (windowing_date_col -
#'  preceding_days) )`
#' @param following_days used as `filter( date < (windowing_date_col +
#'  following_days) )`
#'
#'
#' @return A windowed chronogram
#' @seealso [chronogram::cg_window_by_episode()],
#'  [chronogram::cg_window_by_visit()]
#' @export
#'
#' @examples
#' data(pitch_chronogram)
#' 
#' pitch_chronogram
#' 
#' SevenDaysPrePostDose2 <- cg_window_by_metadata(
#'   pitch_chronogram, dose_2_date, 7, 7
#' )
#' 
#' SevenDaysPrePostDose2
#'
cg_window_by_metadata <- function(cg,
                                  windowing_date_col,
                                  preceding_days,
                                  following_days) {
  calendar_date <- attributes(cg)$col_calendar_date
  calendar_date <- rlang::enquo(calendar_date)
  ids_column_name <- attributes(cg)$col_ids


  y <- cg %>%
    dplyr::group_by(
      .data[[{{ ids_column_name }}]]) %>%
    dplyr::filter(
      .data[[{{ calendar_date }}]] >
        {{ windowing_date_col }} - preceding_days
    ) %>%
    dplyr::filter(
      .data[[{{ calendar_date }}]] <
        {{ windowing_date_col }} + (following_days + 1)
    ) %>%
    dplyr::ungroup() %>%
    
    ## if the anchor dates are very close (or windows very large),
    ## the above can duplicate rows, when called by
    ## cg_window_by_episode(episode_handling == "all")
    ## ^ this performs a left_join ahead of sending to cg_window_by_metadata
    dplyr::group_by(
      .data[[{{ ids_column_name }}]],
      .data[[{{ calendar_date }}]]
      ) %>%
    dplyr::slice_head() %>%
    dplyr::ungroup()

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
