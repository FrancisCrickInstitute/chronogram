#' @export
#' @rdname glimpse_metadata.cg_tbl
glimpse_metadata <- function(x, width = NULL, ...) {
  UseMethod("glimpse_metadata")
}

#' @export
#' @rdname glimpse_experiment_data.cg_tbl
glimpse_experiment_data <- function(x, width = NULL, ...) {
  UseMethod("glimpse_experiment_data")
}



#' Glimpse S3 method for metadata from chronogram objects (class cg_tbl)
#'
#' @param x a chrongram
#' @param width,... passed to pillar::glimpse
#'
#' @return print to console
#' @export
#'
#' @seealso [chronogram::glimpse()]
#'
#' @importFrom dplyr glimpse
#' @examples
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#' glimpse_metadata(cg)
#' 
glimpse_metadata.cg_tbl <- function(x, width = NULL, ...) {
  metadata_cols <- attributes(x)$cols_metadata

  metadata_to_glimpse <- x %>%
    tibble::as_tibble() %>%
    dplyr::select(
      dplyr::all_of(metadata_cols)
    )

  cat("Metadata\n")
  metadata_to_glimpse %>% dplyr::glimpse(., width = NULL, ...)
}




#' Glimpse S3 method for experiment data from chronogram objects (class cg_tbl)
#'
#' @param x a chrongram
#' @param width,... passed to pillar::glimpse
#'
#' @return print to console
#' @export
#'
#' @seealso [chronogram::glimpse()]
#' @importFrom dplyr glimpse
#' @examples
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#' glimpse_experiment_data(cg)
#' 
glimpse_experiment_data.cg_tbl <- function(x, width = NULL, ...) {
  metadata_cols <- attributes(x)$cols_metadata
  ids_col_name <- attributes(x)$col_ids
  calendar_date <- attributes(x)$col_calendar_date

  to_ignore <- c(metadata_cols, ids_col_name, calendar_date)

  experiment_data_to_glimpse <- x %>%
    tibble::as_tibble() %>%
    dplyr::select(
      -dplyr::all_of(metadata_cols)
    )

  cat("Experiment data & annotations\n")
  experiment_data_to_glimpse %>% dplyr::glimpse(., width = width, ...)
}

#' Glimpse S3 method for chronogram objects (class cg_tbl)
#'
#' @param x a chrongram
#' @param width,... passed to pillar::glimpse
#'
#' @return print to console
#' @export
#' @importFrom dplyr glimpse
#'
#' @examples
#' library(dplyr)
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#' glimpse(cg)
#' 
glimpse.cg_tbl <- function(x, width = NULL, ...) {
  cat(
    paste(
      "Glimpse: chronogram", "\n",
      "Dates column: ", attributes(x)$col_calendar_date, "\n",
      "IDs column:   ", attributes(x)$col_ids, "\n"
    )
  )
  cat("\n")
  glimpse_metadata.cg_tbl(x, width = width, ...)
  cat("\n")
  glimpse_experiment_data.cg_tbl(x, width = width, ...)
}


#' Glimpse S3 method for grouped chronogram objects (class grouped_cg_df)
#'
#' @param x a grouped chrongram
#' @param width,... passed to pillar::glimpse
#'
#' @return print to console
#' @export
#' @importFrom dplyr glimpse
#'
#' @examples
#' library(dplyr)
#' 
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#' cg <- cg %>% group_by(elig_study_id)
#' glimpse(cg)
#'
glimpse.grouped_cg_df <- function(x, width = NULL, ...) {
  groups <- dplyr::group_vars(x)

  x <- x %>% ungroup()

  cat(
    paste(
      "Glimpse: grouped chronogram", "\n",
      "Groups:       ", paste(groups, collapse = ", "), "\n",
      "Dates column: ", attributes(x)$col_calendar_date, "\n",
      "IDs column:   ", attributes(x)$col_ids, "\n"
    )
  )
  cat("\n")
  glimpse_metadata.cg_tbl(x, width = width, ...)
  cat("\n")
  glimpse_experiment_data.cg_tbl(x, width = width, ...)
}
