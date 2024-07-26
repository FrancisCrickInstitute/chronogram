#' summary chronogram
#'
#' @param x a chronogram object (class tbl_chronogram)
#'
#' @return print to console
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
#' ## print, with default tibble options ##
#' small_study_chronogram
#'
#' ## print, with eg 3 rows ##
#' print(small_study_chronogram, n = 3)
#' }
#' @export
#' @noRd
#'
summary.cg_tbl <- function(object, ...) {
  x <- object

  calendar_date <- attributes(x)$col_calendar_date
  ids_column_name <- attributes(x)$col_ids
  version_number <- attributes(x)$cg_pkg_version
  windowed <- attributes(x)$windowed
  meta <- attributes(x)$cols_metadata

  zz <- NULL # suppress no visible binding warning

  over_min <- x %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(ids_column_name)
      )
    ) %>%
    dplyr::summarise(zz = (dplyr::n())) %>%
    dplyr::pull(zz) %>%
    min(., na.rm = TRUE)

  over_max <- x %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(ids_column_name)
      )
    ) %>%
    dplyr::summarise(zz = (dplyr::n())) %>%
    dplyr::pull(zz) %>%
    max(., na.rm = TRUE)

  cat(
    ## use paste to prevent cat re-formating dates
    paste(
      "A chronogram:\n",
      "Dates column: ", calendar_date, "\n",
      "IDs column:   ", ids_column_name, "\n",
      "Starts on:    ",
      min(x %>% dplyr::pull(calendar_date)), "\n",
      "Ends on:      ",
      max(x %>% dplyr::pull(calendar_date)), "\n",
      # "Spanning:     ",
      # max(x %>% dplyr::pull( calendar_date )) -
      #   min(x %>% dplyr::pull( calendar_date )),
      # " days\n",
      "Contains:     ",
      x %>% dplyr::pull(ids_column_name) %>%
        factor(.) %>% nlevels(.),
      " unique participant IDs\n",
      "Windowed:     ", windowed, "\n",
      "Spanning:     ",
      over_min,
      "-",
      over_max, "days [min-max per participant]\n",
      "Metadata:     ",
      paste(meta, collapse = ", "), "\n",
      "Size:         ",
      rlang::parse_bytes(
        as.character(
          lobstr::obj_size(x)
        )
      ), "\n",
      "Pkg_version:  ",
      version_number, "[used to build this cg]\n\n"
    )
  )
}




#' @export
#' @noRd
summary.grouped_cg_df <- function(object, ...) {

  x <- object
  groups <- dplyr::group_vars(x)

  cat(
    paste(
      "A grouped chronogram", "\n",
      "Groups:       ", paste(groups, collapse = ", "), "\n",
      "... now summarising whole chronogram ...", "\n"
    )
  )

  NextMethod(x, ...)
}
