#' new_chronogram_skeleton
#'
#' @description A low level constructor function. Use the helper
#' chronogram_skeleton() instead.
#'
#'
#' @param x a tibble
#' @param calendar_date_col provided as
#'  rlang::enquo(an_unquoted_string)
#' @param ids_col provided as rlang::enquo(an_unquoted_string)
#'
#' @return x a tibble with class cg_skl_tbl
#'
#' @seealso [chronogram::chronogram_skeleton()]
#'
new_chronogram_skeleton <- function(x,
                                    calendar_date_col = NULL,
                                    ids_col = NULL) {
  ## calendar_date_col and ids_col are provided as
  ## rlang::enquo(an_unquoted_string)
  ## use rlang::as_name() to convert to a quoted string

  calendar_date_col <- rlang::as_name(calendar_date_col)

  ids_col <- rlang::as_name(ids_col)

  attributes_x <- attributes(x)

  stopifnot(
    "x not a tbl_df.
      Use helper chronogram_skeleton() instead of
      low level constructor new_chronogram_skeleton()" =
      inherits(x, "tbl_df")
  )

  stopifnot(
    "Supplied calendar date col not present in x" =
      calendar_date_col %in% colnames(x)
  )

  stopifnot(
    "Supplied ids_column_name not present in x" =
      ids_col %in% colnames(x)
  )


  attributes_x <- c(
    attributes_x,
    col_calendar_date = calendar_date_col,
    col_ids = ids_col,
    ## also include the version of chronogram ##
    cg_pkg_version =
      paste(utils::packageVersion("chronogram"), sep = ".")
  )

  attributes(x) <- attributes_x

  class(x) <- c("cg_skl_tbl", class(x))

  return(x)
}
