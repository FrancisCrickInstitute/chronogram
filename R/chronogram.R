#' Build a chronogram, from a chronogram_skeleton and metadata
#'
#' @param cg_skeleton a cg_skeleton object
#' @param metadata a tibble containing metadata and ID column name (and
#'  IDs) that are present in cg_skeleton
#'
#' @return An object of chronogram class, with metadata spread across
#'  all calendar_dates.
#'
#' @seealso [chronogram::chronogram_skeleton()],
#'  [chronogram::cg_add_experiment()]
#' @export
#'
#' @examples
#' \dontrun{
#' ## a 3-person chronogram_skeleton ##
#' small_study <- chronogram_skeleton(
#'   col_ids = elig_study_id,
#'   ids = c(1, 2, 3),
#'   start_date = "01012020",
#'   end_date = "10102021",
#'   col_calendar_date = calendar_date
#' )
#'
#' ## Create a tibble containing some metadata for our 3 individuals ##
#' small_study_metadata <- tibble::tribble(
#'   ~elig_study_id, ~age, ~sex, ~dose_1, ~date_dose_1, ~dose_2, ~date_dose_2,
#'   1, 40, "F", "AZD1222", "05/01/2021", "AZD1222", "05/02/2021",
#'   2, 45, "F", "BNT162b2", "05/01/2021", "BNT162b2", "05/02/2021",
#'   3, 35, "M", "BNT162b2", "10/01/2021", "BNT162b2", "10/03/2021"
#' )
#'
#' ## Set appropriate metadata column classes ##
#' library(dplyr)
#' small_study_metadata <- small_study_metadata %>%
#'   mutate(across(c(sex, dose_1, dose_2), ~ as.factor(.x)))
#'
#' small_study_metadata <- small_study_metadata %>%
#'   mutate(across(contains("date"), ~ lubridate::dmy(.x)))
#'
#' ## Make a chronogram ##
#' small_study_chronogram <- chronogram(
#'   small_study,
#'   small_study_metadata
#' )
#'
#'
#'
#' #'  ## Add Experimental data ##
#'
#' small_study_Ab <-
#'   tibble::tribble(
#'     ~elig_study_id, ~calendar_date, ~S, ~N,
#'     1, "05/01/2021", 500, 100,
#'     1, "15 / 01 / 2021", 4000, 100,
#'     1, "03 / 02 / 2021", 3750, 100,
#'     1, "15 / 02 / 2021", 10000, 100,
#'     2, "05 / 01 / 2021", 0, 0,
#'     2, "15 / 01 / 2021", 4000 / 2, 0,
#'     2, "03 / 02 / 2021", 3750 / 2, 0,
#'     2, "15 / 02 / 2021", 10000 / 2, 0,
#'     3, "05 / 01 / 2021", 0, 0,
#'     3, "25 / 01 / 2021", 4000 / 2, 0,
#'     3, "03 / 02 / 2021", 3750 / 2, 0,
#'     3, "20 / 03 / 2021", 10000 / 2, 0
#'   )
#'
#' small_study_Ab <- small_study_Ab %>%
#'   mutate(across(contains("date"), ~ lubridate::dmy(.x)))
#'
#' small_study_chronogram <- cg_add_experiment(
#'   small_study_chronogram,
#'   small_study_Ab
#' )
#' }
chronogram <- function(cg_skeleton, metadata) {
  ## retrieve ids column name from cg_skeleton
  ids_column_name <- attributes(cg_skeleton)$col_ids

  ## Check chronogram_skeleton is valid ##
  stopifnot(validate_chronogram_skeleton(cg_skeleton))

  ## Check metadata is valid ##
  check_metadata(
    x = metadata,
    cg_skeleton = cg_skeleton
  )

  ##

  ## stash levels of cg_skeleton::ids ##
  lvls <- cg_skeleton %>%
    dplyr::pull(.data[[{{ ids_column_name }}]])
  lvls <- levels(lvls)

  ## left_join based on ids_column_name
  x <- dplyr::left_join(
    cg_skeleton %>%
      dplyr::mutate({{ ids_column_name }} :=
        as.character(
          .data[[{{ ids_column_name }}]]
        )),
    metadata %>%
      dplyr::mutate({{ ids_column_name }} :=
        as.character(
          .data[[{{ ids_column_name }}]]
        )),
    by = {{ ids_column_name }}
  )

  ## re-apply the levels from cg_skeleton::ids ##
  x <- x %>%
    dplyr::mutate({{ ids_column_name }} :=
      factor(
        .data[[{{ ids_column_name }}]],
        levels = lvls
      ))

  metadata_cols <- colnames(metadata)

  x <- new_chronogram(x, metadata_cols)

  stopifnot(validate_chronogram(x))


  return(x)
}
