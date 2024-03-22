#' Wrapper to assemble chronogram
#'
#' @description `cg_assemble()` wraps `chronogram_skeleton()`,
#'  `chronogram()` and `cg_add_experiment()`. A verbose messsaging
#'  option (on by default) is provided to help troubleshoot input. For
#'  further troubleshooting, running `chronogram_skeleton()`,
#'  `chronogram()` and `cg_add_experiment()` sequentially is
#'  suggested.
#'
#' @param metadata_ids_col the (unquoted) column name for IDs in
#'  metadata tibble. Studies may use StudyID, StudyId, PID, etc. No
#'  default provided.
#' @param calendar_date_col to label the date column  (unquoted).
#' @param experiment_data_list a list of tibbles of experiment data.
#'  See `cg_add_experiment()` for details. Ignored if not provided.
#' @param verbose Default TRUE. Show progress messages?
#'
#' @inheritParams chronogram_skeleton
#' @inheritParams chronogram
#'
#' @return a chronogram (class cg_tbl)
#'
#' @seealso [chronogram_skeleton()] [chronogram()]
#'  [cg_add_experiment()]
#' @export
#'
#' @examples
#' \dontrun{
#' data("smallstudy")
#'
#'
#' cg <- cg_assemble(
#'   start_date = "01012020",
#'   end_date = "10012020",
#'   metadata = smallstudy$small_study_metadata,
#'   metadata_ids_col = elig_study_id,
#'   calendar_date_col = calendar_date
#' )
#'
#' ## Now with experiment data ##
#' small_study_Ab <-
#'   tibble::tribble(
#'     ~elig_study_id, ~calendar_date, ~serum_Ab_S, ~serum_Ab_N,
#'     1, "05/01/2021", 500, 100,
#'     1, "15/01/2021", 4000, 100,
#'     1, "03/02/2021", 3750, 100,
#'     1, "15/02/2021", 10000, 100,
#'     2, "05/01/2021", 0, 0,
#'     2, "15/01/2021", 4000 / 2, 0,
#'     2, "03/02/2021", 3750 / 2, 0,
#'     2, "15/02/2021", 10000 / 2, 0,
#'     3, "05/01/2021", 0, 0,
#'     3, "25/01/2021", 4000 / 2, 0,
#'     3, "03/02/2021", 3750 / 2, 0,
#'     3, "20/03/2021", 10000 / 2, 0
#'   )
#'
#' small_study_Ab <- small_study_Ab %>%
#'   mutate(across(contains("date"), ~ lubridate::dmy(.x)))
#'
#' cg <- cg_assemble(
#'   start_date = "01012020",
#'   end_date = "10012020",
#'   metadata = smallstudy$small_study_metadata,
#'   metadata_ids_col = elig_study_id,
#'   calendar_date_col = calendar_date,
#'   experiment_data_list = list(small_study_Ab)
#' )
#'
#' ## Or two items of experiment data ##
#' infections_to_add <- tibble::tribble(
#'   ~calendar_date, ~elig_study_id, ~LFT, ~PCR, ~symptoms,
#'   "01102020", "1", "pos", NA, NA,
#'   "11102020", "1", "pos", NA, "severe"
#' )
#' ## Make calendar_date a date ##
#' infections_to_add$calendar_date <- lubridate::dmy(
#'   infections_to_add$calendar_date
#' )
#'
#' cg <- cg_assemble(
#'   start_date = "01012020",
#'   end_date = "10102021",
#'   metadata = smallstudy$small_study_metadata,
#'   metadata_ids_col = elig_study_id,
#'   calendar_date_col = calendar_date,
#'   experiment_data_list = list(small_study_Ab, infections_to_add)
#' )
#' }
#'
cg_assemble <- function(
    start_date,
    end_date,
    calendar_date_col,
    metadata,
    metadata_ids_col,
    experiment_data_list = NULL,
    verbose = TRUE) {
  ## quote NSE variables ##
  quoted_calendar_date_col <-
    rlang::as_label(rlang::enquo(calendar_date_col))

  quoted_metadata_ids_col <-
    rlang::as_label(rlang::enquo(metadata_ids_col))

  ## input checks ##
  if (verbose) {
    message("Checking input parameters...")
  }

  ## check start_date ##
  if (verbose) {
    message(paste("-- checking start date", start_date))
  }

  stopifnot(
    "Start date not provided as character" =
      is.character(start_date)
  )
  stopifnot(
    "Start date is not length == 1" =
      length(start_date) == 1
  )
  stopifnot("Start date not parsed to date by lubridate::dmy" = {
    inherits(x = lubridate::dmy(start_date), "Date") &
      !is.na(lubridate::dmy(start_date))
  })

  ## check end_date ##
  if (verbose) {
    message(paste("-- checking end date", end_date))
  }

  stopifnot(
    "End date not provided as character" =
      is.character(end_date)
  )
  stopifnot(
    "End date is not length == 1" =
      length(end_date) == 1
  )
  stopifnot("End date not parsed to date by lubridate::dmy" = {
    inherits(x = lubridate::dmy(end_date), "Date") &
      !is.na(lubridate::dmy(end_date))
  })

  if (verbose) {
    message(paste("-- checking end date later than start date"))
  }

  stopifnot(
    "Start date is later than end date" =
      lubridate::dmy(end_date) > lubridate::dmy(start_date)
  )


  ## check metadata ##
  if (verbose) {
    message(paste("-- checking metadata"))
  }

  stopifnot(
    "Metadata not a tibble" =
      inherits(metadata, "tbl_df")
  )
  ## metadata ID column present in metadata
  stopifnot(
    "ID column not in metadata" =
      quoted_metadata_ids_col %in% colnames(metadata)
  )

  ## calendar date column present in metadata
  stopifnot(
    "calendar date column also in metadata. Consult assembly vignette." = {
      (quoted_calendar_date_col %in%
        colnames(metadata)) == FALSE
    }
  )

  if (is.null(experiment_data_list)) {
    if (verbose) {
      message(
        "--no experiment data provided. Add later: cg_add_experiment()"
      )
    }
  } else {
    if (verbose) {
      message(
        paste("-- checking experiment data list")
      )
    }

    stopifnot(
      "Experiment data not provided as a list" =
        inherits(experiment_data_list, "list")
    )

    for (i in seq_len(length(experiment_data_list))) {
      message(paste("--- checking experiment data list slot", i))
      stopifnot(
        "Experiment data list slots does not a tibble
        Consider adding one-by-one with cg_add_experiment()" =
          inherits(experiment_data_list[[i]], "tbl_df")
      )

      stopifnot("calendar date column not present" = {
        (quoted_calendar_date_col %in%
          colnames(experiment_data_list[[i]]))
      })

      stopifnot("calendar date column is not class Date" = {
        inherits(
          experiment_data_list[[i]] %>%
            dplyr::pull(quoted_calendar_date_col),
          "Date"
        )
      })

      stopifnot("IDs column not present" = {
        (quoted_metadata_ids_col %in%
          colnames(experiment_data_list[[i]]))
      })
      ## overlap with cg colnames checked in cg_add_experiment()
    }
  }

  if (verbose) {
    message("Input checks completed\nChronogram assembling...")
  }
  ## END checks ###

  ## derived variables ##
  ids <- metadata %>% dplyr::pull(quoted_metadata_ids_col)



  ## make a cg_skeleton ##
  cg_skeleton <- chronogram_skeleton(
    start_date = start_date,
    end_date = end_date,
    col_calendar_date = {{ calendar_date_col }},
    ids = ids,
    col_ids = {{ metadata_ids_col }}
  )

  if (verbose) {
    message("-- chrongram_skeleton built")
  }

  ## make a cg ##
  cg <- chronogram(cg_skeleton = cg_skeleton, metadata = metadata)

  if (verbose) {
    message("-- chrongram built with metadata")
  }

  if (is.null(experiment_data_list)) {
    if (verbose) {
      message(
        "-- no experiment data provided\n\nAssembly finished"
      )
      return(cg)
    }
  } else {
    if (verbose) {
      message(
        paste("-- adding experiment data")
      )
    }

    for (i in seq_len(length(experiment_data_list))) {
      if (verbose) {
        message(
          paste(
            "--- adding experiment data slot", i,
            "cols...",
            paste(
              colnames(experiment_data_list[[i]])[1:3],
              collapse = " "
            ),
            "..."
          )
        )
      }

      cg <- cg_add_experiment(cg, experiment_data_list[[i]])
    }
  }

  return(cg)
}
