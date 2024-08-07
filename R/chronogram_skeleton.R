#' Build a chronogram skeleton, a two column index of IDs and calendar
#' dates, a scaffold for a chronogram
#' 
#' Assemble a chronogram skeleton from start and end dates, and a
#' vector of participant IDs. For most circumstances, `cg_assemble()`
#' is suggested (which calls `chronogram_skeleton()` and
#' `chronogram()`). See assembly vignette.
#' 
#' @param ids a vector of study, or participant IDs.
#' @param col_ids to label the ID
#'   column. Default is `column_name_for_ids` (unquoted). Studies may use
#'   StudyID, StudyId, PID, etc.
#' @param start_date the start date, in a format recognised by
#'   `lubridate::dmy()`
#' @param end_date the end date, in a format recognised by
#'   `lubridate::dmy()`
#' @param col_calendar_date to label the date
#'   column. Default is `column_name_for_calendar_date` (unquoted).
#'
#' @return A chronogram skeleton (class tbl_df, with extra attributes).
#' @export
#'
#' @examples
#' ##-----------------------------------------------------------------
#' small_study <- chronogram_skeleton(
#'   ids = c(1, 2, 3),
#'   start_date = "01012020",
#'   end_date = "10102021"
#' )
#'
#' ## These examples fail ##------------------------------------------
#' ## Duplicate dates, or dates lubridate::dmy() does not recognise ##
#' small_study <- try(
#' chronogram_skeleton(
#'   ids = c(1, 2, 3),
#'   start_date = c("01012020", "05012020"),
#'   end_date = "10102021")
#' )
#' small_study <- try(
#' chronogram_skeleton(
#'   ids = c(1, 2, 3),
#'   start_date = c("01012020"),
#'   end_date = c("10102021", "20102020"))
#' )
#' small_study <- try(
#' chronogram_skeleton(
#'   ids = c(1, 2, 3),
#'   start_date = "20200101",
#'   end_date = "10102021")
#' )
#' small_study <- try(
#' chronogram_skeleton(
#'   ids = c(1, 2, 3),
#'   start_date = "01012020",
#'   end_date = "20201010")
#' )
#'
#' ## Duplicate participant IDs are accepted, but give a warning ##
#' small_study <- chronogram_skeleton(
#'   ids = c(1, 2, 3, 3),
#'   start_date = "01012020",
#'   end_date = "10102021"
#' )
#' 
chronogram_skeleton <- function(
    ids = NULL,
    col_ids = column_name_for_ids,
    start_date = NULL,
    end_date = NULL,
    col_calendar_date = column_name_for_calendar_date) {
  ## check dates are valid
  start_date <-
    suppressWarnings(
      lubridate::dmy(start_date)
    )
  end_date <- suppressWarnings(
    lubridate::dmy(end_date)
  )

  stopifnot(
    "Start date failed to parse! Provide in dmy format." =
      any(is.na(start_date)) == FALSE
  )

  stopifnot(
    "More than 1 start dates provided!" =
      length(start_date) == 1
  )

  stopifnot(
    "End date failed to parse! Provide in dmy format." =
      any(is.na(end_date)) == FALSE
  )

  stopifnot(
    "Start date is later than end date. Use dmy format." =
      as.numeric(end_date) >
        as.numeric(start_date)
  )

  stopifnot(
    "More than 1 end dates provided!" =
      length(end_date) == 1
  )

  stopifnot(
    "Oops, please provide a col_ids" =
      is.null(rlang::enquo(col_ids)) == FALSE
  )

  ##  check for duplicate ids
  if (any(duplicated(ids))) {
    ##  print duplicates to screen, then use once
    warning(paste("Duplicate IDs found : ", ids[duplicated(ids)],
      "Check your input ID vector...
      provide a unique identifier for
      each individual\nReturning a de-duplicated chronogram_skeleton...",
      collapse = " "
    ))
    ids <- ids[!duplicated(ids)]
  }

  ## make ids a factor ##
  ids <- as.factor(as.character(ids))

  ##  do the cross multiplication of ids x dates
  cg_skeleton <-
    tibble::tibble(
      "{{col_calendar_date}}" := seq.Date(
        from = start_date,
        end_date, by = 1
      )
    )

  cg_skeleton <- tidyr::crossing(
    cg_skeleton, tibble::tibble("{{col_ids}}" := ids)
  )

  ##  return a tibble with two-columns
  cg_skeleton <- cg_skeleton %>%
    dplyr::arrange({{ col_ids }}, {{ col_calendar_date }})

  cg_skeleton <- new_chronogram_skeleton(
    x = cg_skeleton,
    calendar_date_col = rlang::enquo(col_calendar_date),
    ids_col = rlang::enquo(col_ids)
  )

  return(cg_skeleton)
}
