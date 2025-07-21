#' check ExperimentalData
#'
#' @param x a tibble containing ExperimentalData, with an ID column
#'   and a date of SAMPLE (not date of assay).
#' @param cg the chronogram to join
#'
#' @return "valid" or stop with message
#'
check_experiment <- function(x, cg = NULL) {
  ### ExperimentalData must be a tibble
  stopifnot(
    "Invalid experiment: not a tibble" =
      any(grepl(class(x), pattern = "tbl_df"))
  )

  calendar_date <- attributes(cg)$col_calendar_date
  ids_column_name <- attributes(cg)$col_ids

  ### and have a calendar_date column
  stopifnot(
    "Invalid experiment: a calendar date column required" =
      (calendar_date %in% colnames(x) == TRUE)
  )

  ### has a column that matches ids_column_name
  if (!is.null(ids_column_name)) {
    stopifnot(
      "Invalid experiment: provided ids_column_name is not present" =
        ids_column_name %in% colnames(x) == TRUE
    )
  }

  stopifnot(
    "Invalid experiment: date:ids are duplicated" = {
      n <- NULL # suppress global binding note

      xx <- x %>%
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(
              c(
                calendar_date,
                ids_column_name
              )
            )
          )
        ) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

      all(xx < 2) == TRUE
    }
  )


  return(TRUE)
}
