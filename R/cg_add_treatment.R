#' Add treatment data to a chronogram
#'
#' @param cg a chronogram object (class cg_tbl)
#' @param treatment a tibble containing treatment data. The dates
#'   and IDs columns in cg must be present in `experiment`.
#'
#' @return A chronogram
#' @seealso [chronogram::cg_add_experiment()]
#' @export
#'
#' @examples
#' ## Example 1: A small study ##-------------------------------------
#' library(dplyr)
#' data(built_smallstudy)
#' 
#' ## Setup ##
#' cg <- built_smallstudy$chronogram
#' 
#' # Create a tibble of exemplar treatment data
#' # Here, we have treated ID=1 with B cell depletion
#' # therapy, rituximab (RTX).
#' 
#' treatments_to_add <-
#'   tibble::tribble(
#'     ~elig_study_id, ~calendar_date, ~treatment,
#'     1, "01/02/2021", "RTX"
#'   )
#'   
#' ## Make calendar_date a date class ##
#' treatments_to_add <- treatments_to_add %>%
#'   mutate(across(contains("date"), ~ lubridate::dmy(.x)))
#'   
#' ## Add this new treatment data ##
#' cg_added <- cg_add_experiment(cg, treatments_to_add)
#' 
#' ## Example 2: Incorrect column names ##----------------------------
#' 
#' ## Setup as for Example 1 ##
#' treatments_to_add_renamed <- treatments_to_add %>%
#' dplyr::rename(ID = elig_study_id)
#' 
#' ## this fails ##
#' try(
#' cg_added_2 <- cg_add_experiment(cg, 
#' treatments_to_add_renamed)
#' )
#' 
#'##------------------------------------------------------------------

cg_add_treatment <- function(
    cg,
    treatment) {
  xxx <- cg_add_experiment(
    cg = cg,
    experiment = treatment
  )

  return(xxx)
}
