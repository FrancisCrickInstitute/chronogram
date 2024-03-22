#' A helper function to save a chronogram
#'
#' @param cg a chronogram object (class cg_tbl)
#' @param file quoted filename. Include an extension.
#' @param verbose logical. Default TRUE. Display messages?
#' @param ... passed to [base::save()]. For example, `compress`
#'
#' @return INVISIBLE, a saved file
#'
#' @seealso [chronogram::cg_load()]
#' @export
#'
#' @examples
#' \dontrun{
#' data("built_smallstudy")
#' cg_orig <- built_smallstudy$chronogram
#' ff <- tempfile()
#' cg_save(cg_orig, file = ff)
#' cg_new <- cg_load(ff)
#'
#' summary(cg_new == cg_orig)
#' identical(cg_new, cg_orig)
#' }
cg_save <- function(cg,
                    file = NULL, ...,
                    verbose = TRUE) {
  stopifnot(
    "Supplied object, x, is not a valid chronogram." =
      validate_chronogram(cg)
  )

  stopifnot(
    "No filename supplied" =
      is.null(file) == FALSE
  )

  # stopifnot(
  #   "Compression not recognised" =
  #     is.null(file) == F)

  ## calendar date column finding ##
  calendar_date <- attributes(cg)$col_calendar_date

  ## ids column finding ##
  ids_column_name <- attributes(cg)$col_ids

  ## ----------##
  ## split cg into three:
  # i. a single row/participant of metadata
  # ii. experimental data (etc) stays as 1 row/{participant x date}
  # iii. a list of attributes


  ## iii. ##
  attributesToSave <- attributes(cg)
  ## ----------##
  ## i. ##
  if (verbose == TRUE) {
    message("Finding metadata columns ...")
  }

  ## metadata_columns slot of attributes of chronogram object ##
  metadata_cols <- attributes(cg)$metadata_columns

  if (verbose == TRUE) {
    message("Found: ", paste(metadata_cols, collapse = ", "))
  }

  metadataToSave <- cg %>%
    dplyr::select(dplyr::all_of(c(ids_column_name, metadata_cols)))

  class(metadataToSave) <-
    class(metadataToSave)[-1]

  metadataToSave <- metadataToSave %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(ids_column_name)
      )
    ) %>%
    dplyr::slice_head() %>%
    dplyr::ungroup()
  ## ----------##
  ## ii. ##
  not_metadata_cols <- colnames(cg)[!colnames(cg) %in% metadata_cols]

  expdataToSave <- cg %>%
    dplyr::select(
      dplyr::all_of(
        c(
          {{ calendar_date }},
          {{ ids_column_name }},
          not_metadata_cols
        )
      )
    )

  class(expdataToSave) <-
    class(expdataToSave)[-1]

  if (verbose == TRUE) {
    message("Writing file ... ", file)
  }
  ## save the three objects as a single file, compressed as described ##
  save(metadataToSave, expdataToSave, attributesToSave,
    ids_column_name,
    file = file, ...
  )
}
