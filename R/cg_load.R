#' Load a chronogram
#' 
#' Load into memory a chronogram previously written to disk by `cg_save()`.
#'
#' @param file quoted filename. Include an extension.
#'
#' @return re-loaded chronogram
#'
#' @seealso [chronogram::cg_save()]
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
cg_load <- function(file = NULL) {
  ## suppress global binding note ##
  attributesToSave <-
    expdataToSave <-
    ids_column_name <-
    metadataToSave <- NULL

  load(file)

  ids_column_name <- attributesToSave$col_ids

  message(
    paste(
      "Suggest save and load with the same package version:\n",
      "Loading into :",
      paste(utils::packageVersion("chronogram"), sep = "."), "\n",
      "Saved with   :",
      attributesToSave$cg_pkg_version, "\n",
      "If version numbers are different:
      re-generate chronogram, or update your library."
    )
  )

  ## join the metadata and experimental data ##
  ## note left_join rather than seqDate() rebuilding
  ## (seqDate rebuild would not support a windowed chronogram)
  cg_new <- dplyr::left_join(expdataToSave,
    metadataToSave,
    by = {{ ids_column_name }}
  )

  ## re-order columns to match object send to disk ##
  cg_new <- cg_new %>%
    dplyr::select(dplyr::all_of(attributesToSave$names))

  ## reset attributes (and class) ##
  attributes(cg_new) <- attributesToSave

  return(cg_new)
}
