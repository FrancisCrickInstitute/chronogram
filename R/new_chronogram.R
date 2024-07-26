#' new chronogram
#'
#' @param x a tbl, a daughter of a join between a chronogram skeleton and
#'   one (or more) objects coerced to a tbl.
#' @param metadata_cols the column names from the metadata tibble
#' @param ... passed to `new_tibble()`. Adds grouping support.
#'
#' @return x an extended tibble with class cg_tbl
#'
#' @seealso [chronogram::chronogram_skeleton()]
#'
#' @examples
#' \dontrun{
#' x <- new_chronogram(x)
#' }
new_chronogram <- function(x,
                           metadata_cols,
                           ...) {
  stopifnot(
    "provided object does not inherit tbl_df class" =
      inherits(x, "tbl_df")
  )

  stopifnot(
    "metadata_cols not provided as a character vector" =
      inherits(metadata_cols, "character")
  )

  ## Set first class to cg_tbl ##
  class(x) <- c("cg_tbl", class(x))

  ## remove the col_ids from metadata column names ##
  col_ids <- attributes(x)$col_ids
  cols_metadata <- metadata_cols[
    !metadata_cols %in% col_ids
  ]

  ## add metadata column names to attributes ##
  attributes_x <- attributes(x)
  attributes_x <- c(attributes_x,
    windowed = FALSE,
    cols_metadata = list(cols_metadata)
  )
  attributes(x) <- attributes_x

  return(x)
}
