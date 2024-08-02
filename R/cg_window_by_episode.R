#' Pick a window of dates from a chronogram: with respect to episode
#' start.
#'
#' @param cg a chronogram
#' @param episode_numbers_col The column name to use for episode
#'   numbers. Default is `episode_number`.
#' @param episode_handling which episode to reference. Must be one of
#'   "first", "last", or "all".
#' @param preceding_days used as `filter( date > (date_col -
#'   preceding_days) )`
#' @param following_days used as `filter( date < (date_col +
#'   following_days) )`
#'
#' @return A windowed chronogram
#' @seealso [chronogram::cg_window_by_visit()],
#'   [chronogram::cg_window_by_metadata()]
#' @export
#'
#' @examples
#' \dontrun{
#'
#' SevenDaysPrePostFirstEpisode <- cg_window_by_episode(
#'   annotatedChronogram, "first", 7, 7
#' )
#' }
#'
cg_window_by_episode <- function(
    cg,
    episode_numbers_col = "episode_number",
    episode_handling = c("first", "last", "all"),
    preceding_days, following_days) {
  stopifnot(
    "episode_handling must be one of first, last, or all" =
      episode_handling %in% c("first", "last", "all")
  )

  date_ref <- NULL

  calendar_date <- attributes(cg)$col_calendar_date
  ids_column_name <- attributes(cg)$col_ids

  stopifnot(
    "Please provide preceding days as a numerical value" =
      is.numeric(preceding_days)
  )

  stopifnot(
    "Please provide following days as a numerical value" =
      is.numeric(following_days)
  )



  if (episode_handling == "first") {
    refs <- cg %>%
      dplyr::group_by(.data[[{{ ids_column_name }}]]) %>%
      dplyr::filter(!is.na(.data[[{{ episode_numbers_col }}]])) %>%
      dplyr::arrange(
        {{ calendar_date }},
        {{ episode_numbers_col }},
        .by_group = TRUE
      ) %>%
      dplyr::slice_head() %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(ids_column_name, calendar_date))) %>%
      dplyr::rename(date_ref = {{ calendar_date }})
  }

  if (episode_handling == "last") {
    refs <- cg %>%
      dplyr::group_by(.data[[{{ ids_column_name }}]]) %>%
      dplyr::filter(!is.na(.data[[{{ episode_numbers_col }}]])) %>%
      dplyr::arrange(
        {{ calendar_date }},
        {{ episode_numbers_col }},
        .by_group = TRUE
      ) %>%
      dplyr::slice_tail() %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(ids_column_name, calendar_date))) %>%
      dplyr::rename(date_ref = {{ calendar_date }})
  }

  if (episode_handling == "all") {
    refs <- cg %>%
      dplyr::group_by(
        .data[[{{ ids_column_name }}]],
        .data[[{{ episode_numbers_col }}]]
      ) %>%
      dplyr::filter(!is.na(.data[[{{ episode_numbers_col }}]])) %>%
      dplyr::arrange({{ calendar_date }},
        .by_group = TRUE
      ) %>%
      dplyr::slice_head() %>%
      dplyr::ungroup() %>%
      dplyr::select(
        dplyr::all_of(c(ids_column_name, calendar_date))
      ) %>%
      dplyr::rename(date_ref = {{ calendar_date }})
  }

  cg <- dplyr::left_join(cg, refs, by = ids_column_name)

  y <- cg_window_by_metadata(cg,
    windowing_date_col = date_ref,
    preceding_days = preceding_days,
    following_days = following_days
  )

  y <- y %>% dplyr::select(!date_ref)

  ## set windowed attribute to TRUE ##
  attributes(y)$windowed <- TRUE

  validate_chronogram(y)

  return(y)
}
