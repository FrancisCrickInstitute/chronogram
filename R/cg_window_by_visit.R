#' Pick a window of dates from a chronogram: with respect to visits.
#'
#' This is a special case of `cg_window_by_episode()`. It treats the
#' visit column as an episode, with the argument `episode_handling
#' = "all"`.
#'
#' @param cg a chronogram
#' @param visit_col a column with either dates of visits, or results.
#'   This is usually an experimental data column. Default is NULL.
#'   Provide unquoted.
#' @param preceding_days used as filter( date > (date_col -
#'   preceding_days) )
#' @param following_days used as filter( date < (date_col +
#'   following_days) )
#'
#' @return A subsetted chronogram
#' @seealso [chronogram::cg_window_by_episode()],
#'   [chronogram::cg_window_by_metadata()]
#' @export
#'
#' @examples
#' 
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#' 
#' SevenDaysAfterEachStudyVisit <- cg_window_by_visit(
#'   cg,
#'   visit_col = serum_Ab_S, 7, 7
#' )
#' 
cg_window_by_visit <- function(cg,
                               visit_col = NULL,
                               preceding_days, following_days) {
  
  y <- cg_window_by_episode(cg,
    episode_numbers_col = {{ visit_col }},
    preceding_days = preceding_days,
    following_days = following_days,
    episode_handling = "all"
  )

  return(y)
}
