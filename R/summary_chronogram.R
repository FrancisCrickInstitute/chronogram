#' summary chronogram
#'
#' @param x a chronogram object (class tbl_chronogram)
#'
#' @return print to console
#' 
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
