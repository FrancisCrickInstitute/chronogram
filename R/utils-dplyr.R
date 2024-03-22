#' Dplyr row slice for chronogram
#'
#' As recommended here:
#' https://dplyr.tidyverse.org/reference/dplyr_extending.html
#'
#' Ensures dplyr verbs return a cg_tbl, rather than tbl.
#' User can ignore.
#'
#' @param data data
#' @param i rows to eg slice
#' @param ... additional arguments
#'
#' @return a cg_tbl after a dplyr verb action
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom dplyr dplyr_row_slice
#' @exportS3Method

dplyr_row_slice.cg_tbl <- function(data, i, ...) {
  out <- tibble::as_tibble(data)
  out <- vctrs::vec_slice(out, i)
  # note this is not dplyr::dplyr_
  ## S3 needs to find the correct method to dispatch
  dplyr_reconstruct(out, data)
}
#' @importFrom dplyr dplyr_col_modify
#' @exportS3Method
dplyr_col_modify.cg_tbl <- function(data, cols) {
  out <- dplyr_col_modify(tibble::as_tibble(data), cols)
  dplyr_reconstruct(out, data)
}




#' @export
`[.cg_tbl` <- function(x, i, j, drop = FALSE, ...) {
  res <- NextMethod()
  return(res)
}


#' @export
`names<-.cg_tbl` <- function (x, value)  {
  res <- NextMethod()
  return(res)
}

#' @export
`names<-.grouped_cg_df` <- function (x, value)  {
  res <- NextMethod()
  return(res)
}



#' @export
`[.grouped_cg_df` <- function(x, i, j, drop = FALSE, ...) {
  res <- NextMethod()
  return(res)
}

#' Dplyr reconstruct for chronogram
#'
#' As recommended here:
#' https://dplyr.tidyverse.org/reference/dplyr_extending.html
#'
#' Ensures dplyr verbs return a cg_tbl, rather than tbl.
#' User can ignore.
#'
#' @param data data
#' @param template example class
#'
#' @return a cg_tbl after a dplyr verb action
#' @exportS3Method
dplyr_reconstruct.cg_tbl <- function(data, template) {
  class(data) <- class(template)

  ## we want to set all attributes BUT names & row.names (as these change with filter etc) ##
  attributes(data)$col_calendar_date <- attributes(template)$col_calendar_date
  attributes(data)$col_ids <- attributes(template)$col_ids
  attributes(data)$cg_pkg_version <- attributes(template)$cg_pkg_version
  attributes(data)$windowed <- attributes(template)$windowed
  attributes(data)$cols_metadata <- attributes(template)$cols_metadata

  data
}


#' Dplyr row slice for grouped chronogram
#'
#' As recommended here:
#' https://dplyr.tidyverse.org/reference/dplyr_extending.html
#'
#' Ensures dplyr verbs return a grouped_cg_df, rather than grouped_df.
#' User can ignore.
#'
#' @param data data
#' @param i rows to eg slice
#' @param ... additional arguments
#' @param preserve FALSE
#'
#' @return a grouped_cg_df after a dplyr verb action
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom dplyr dplyr_row_slice
#' @exportS3Method

dplyr_row_slice.grouped_cg_df <- function(data, i, ..., preserve = FALSE) {
  ## derived from : dplyr:::dplyr_row_slice.grouped_df ##
  ## (as we want same in-data behaviour - grouped_cg_df has extra attributes)
  out <- vctrs::vec_slice(as.data.frame(data), i)
  groups <- dplyr::group_data(data)
  new_id <- vctrs::vec_slice(dplyr::group_indices(data), i)
  new_grps <- vctrs::vec_group_loc(new_id)
  rows <- rep(vctrs::list_of(integer()), length.out = nrow(groups))
  rows[new_grps$key] <- new_grps$loc
  groups$.rows <- rows
  if (!preserve && isTRUE(attr(groups, ".drop"))) {
    groups <- group_data_trim(groups)
  }
  out <- dplyr::new_grouped_df(out, groups)
  ## note this is not dplyr::dplyr_
  ## S3 needs to find the correct method to dispatch
  dplyr_reconstruct(out, data)

  # ## we want to set all attributes BUT names & row.names (as these change with filter etc) ##
  # attributes(data)$col_calendar_date <- attributes(template)$col_calendar_date
  # attributes(data)$col_ids <- attributes(template)$col_ids
  # attributes(data)$cg_pkg_version <- attributes(template)$cg_pkg_version
  # attributes(data)$windowed <- attributes(template)$windowed
  # attributes(data)$cols_metadata <- attributes(template)$cols_metadata
}

#' dplyr:::group_data_trim
#'
#' @param group_data grouped dataset
#'
#' @references dplyr:::group_data_trim
#' @export
group_data_trim <- function(group_data) {
  ## from dplyr:::group_data_trim ##
  non_empty <- lengths(group_data$.rows) > 0
  group_data[non_empty, , drop = FALSE]
}

#' @importFrom dplyr dplyr_col_modify
#' @exportS3Method
dplyr_col_modify.grouped_cg_df <- function(data, cols) {
  # out <- dplyr::grouped_df(data, vars = dplyr::group_vars(data))
  # out <- dplyr_col_modify(data, cols)
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' Dplyr reconstruct for grouped chronogram
#'
#' As recommended here:
#' https://dplyr.tidyverse.org/reference/dplyr_extending.html
#'
#' Ensures dplyr verbs return a grouped_cg_df, rather than grouped_df.
#' User can ignore.
#'
#' @param data data
#' @param template example class
#'
#' @return a grouped_cg_df after a dplyr verb action
#' @exportS3Method
dplyr_reconstruct.grouped_cg_df <- function(data, template) {
  ## leans on dplyr grouped_df reconstruct function
  group_vars_to_use <- intersect(dplyr::group_vars(template), names(data))
  data <- dplyr::grouped_df(data,
    group_vars_to_use,
    drop = dplyr::group_by_drop_default(template)
  )

  class(data) <- class(template)

  ## we want to set all attributes BUT names & row.names (as these change with filter etc) ##
  attributes(data)$col_calendar_date <- attributes(template)$col_calendar_date
  attributes(data)$col_ids <- attributes(template)$col_ids
  attributes(data)$cg_pkg_version <- attributes(template)$cg_pkg_version
  attributes(data)$windowed <- attributes(template)$windowed
  attributes(data)$cols_metadata <- attributes(template)$cols_metadata
  data
}
