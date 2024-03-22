#' A helper function to add treatment data to a chronogram
#'
#' @param cg a chronogram object (class cg_tbl)
#' @param treatment a tibble containing treatment dates (with columns:
#'   the specified calendar_date, and ID columns, defined with
#'   [chronogram::chronogram_skeleton()])
#'
#' This code wraps the`cg_add_experiment` function.
#'
#' @return A chronogram
#' @seealso [chronogram::cg_add_experiment()]
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## Create a tibble of exemplar treatment data ##
#' # Here, we have treated ID=1 with B cell depletion
#' # therapy, rituximab (RTX).
#' #
#' small_study_treatment <-
#'   tibble::tribble(
#'     ~elig_study_id, ~calendar_date, ~treatment,
#'     1, "01/02/2021", "RTX"
#'   )
#' small_study_treatment <- small_study_treatment %>%
#'   mutate(across(contains("date"), ~ lubridate::dmy(.x)))
#'
#' ## Add to chronogram ##
#' small_study_chronogram <- cg_add_treatment(
#'   small_study_chronogram,
#'   small_study_treatment
#' )
#' }
cg_add_treatment <- function(
    cg,
    treatment) {
  xxx <- cg_add_experiment(
    cg = cg,
    experiment = treatment
  )

  return(xxx)
}
