#' New grouped chronogram
#'
#' A constructor for a grouped chronogram. This is called automatically
#' by the S3 method: `group_by.cg_tbl()`.
#'
#' @param x a chronogram
#' @param groups groups
#' @param class character vector of class
#' @param ... parsed to `dplyr::group_by()`
#'
#' @return x an extended grouped tibble with class `grouped_cg_df`
#'
#' @seealso [chronogram::new_chronogram()]
#'
#' @examples
#' \dontrun{
#' x <- new_chronogram(x)
#' }
#'
#' @export
new_grouped_chronogram <- function(x, groups, ..., class = character()) {
  class <- c(class, c("grouped_cg_df", "grouped_df"))
  y <- new_tbl2chronogram(x, groups = groups, ..., class = class)


  ## we want to set all attributes BUT names & row.names (as these change with filter etc) ##
  attributes(y)$col_calendar_date <- attributes(x)$col_calendar_date
  attributes(y)$col_ids <- attributes(x)$col_ids
  attributes(y)$cg_pkg_version <- attributes(x)$cg_pkg_version
  attributes(y)$windowed <- attributes(x)$windowed
  attributes(y)$cols_metadata <- attributes(x)$cols_metadata

  return(y)
}


#' new_tbl2chronogram
#'
#' Coerce a tibble to a chronogram. This
#' function handles grouped chronograms. See
#' `chronogram::chronogram()` for a constructor.
#'
#' @param x tibble to coerce
#' @param class a character vector of classes
#' @param ... passed to `tibble::new_tibble()`
#' @export
new_tbl2chronogram <- function(x, ..., class = character()) {
  xx <- tibble::as_tibble(x)
  y <- tibble::new_tibble(
    x = xx,
    ## we want to set all attributes BUT names & row.names (as these change with filter etc) ##
    col_calendar_date = attributes(x)$col_calendar_date,
    col_ids = attributes(x)$col_ids,
    cg_pkg_version = attributes(x)$cg_pkg_version,
    windowed = attributes(x)$windowed,
    cols_metadata <- attributes(x)$cols_metadata,
    ...,
    nrow = nrow(x),
    class = c(class, "cg_tbl")
  )

  return(y)
}


#' group-by & ungroup-by chronogram
#'
#' S3 implementation of `dplyr::group_by()` that accepts a chronogram
#' and returns an object of `grouped_cg_df` class & the reverse
#' `dplyr::ungroup_by()`.
#' 
#'
#'
#' @param .data input chronogram, cg_tbl
#' @param ...,.add,.drop arguments expected by group_by S3
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr group_data
#'
#' @exportS3Method
group_by.cg_tbl <- function(.data,
                            ...,
                            .add = FALSE,
                            .drop = dplyr::group_by_drop_default(.data)) {
  .data <- NextMethod()
  groups <- dplyr::group_data(.data)
  # groups <- dplyr::group_by_prepare(.data, ..., .add = .add, .drop = .drop, error_call = rlang::current_env())
  y <- new_grouped_chronogram(.data, groups)
  #
  #   attributes_x <- attributes(.data)
  #
  #   ## set attributes ##
  #   attributes(y)$col_calendar_date <- attributes_x$col_calendar_date
  #   attributes(y)$col_ids <- attributes_x$col_ids
  #   attributes(y)$cg_pkg_version <- attributes_x$cg_pkg_version
  #   attributes(y)$windowed <- attributes_x$windowed
  #   attributes(y)$cols_metadata <- attributes_x$cols_metadata

  return(y)
}

#' ungroup-by chronogram
#'
#' An S3 implementation of `dplyr::ungroup()` that accepts a
#' `grouped_cg_df` and returns a chronogram. This
#' retains the attributes from the orginal chronogram.
#'
#'
#' @param x input chronogram, grouped_cg_df
#'
#' @importFrom dplyr ungroup
#' @seealso [chronogram::group_by()]
#'
#' @rdname group_by.cg_tbl
#' @exportS3Method
ungroup.grouped_cg_df <- function(x, ...) {
  out <- NextMethod()
  out <- ungroup(out, ...)

  # ## set attributes ##
  attributes(out)$col_calendar_date <- attributes(x)$col_calendar_date
  attributes(out)$col_ids <- attributes(x)$col_ids
  attributes(out)$cg_pkg_version <- attributes(x)$cg_pkg_version
  attributes(out)$windowed <- attributes(x)$windowed
  attributes(out)$cols_metadata <- attributes(x)$cols_metadata

  y <- new_tbl2chronogram(out)

  # attributes_x <- attributes(.data)
  #


  return(y)
}
