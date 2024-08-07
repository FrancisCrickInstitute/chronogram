#' Add experimental data to a chronogram
#'
#' @param cg a chronogram object (`class cg_tbl`)
#' @param experiment a `tibble::tibble()` containing experimental data. The dates
#'   and IDs columns in `cg` must be present in `experiment`.
#'
#' @return A chronogram
#' @seealso [chronogram::cg_assemble()],
#' [chronogram::cg_add_treatment()],
#' [chronogram::chronogram_skeleton()],
#'   [chronogram::chronogram()]
#' @export
#'
#' @examples
#' ## Example 1: A small study ##-------------------------------------
#' data(built_smallstudy)
#' 
#' ## Setup ##
#' cg <- built_smallstudy$chronogram
#' 
#' infections_to_add <- tibble::tribble(
#'   ~calendar_date, ~elig_study_id, ~LFT, ~PCR, ~symptoms,
#'   "01102020", "1", "pos", NA, NA,
#'   "11102020", "1", "pos", NA, "severe"
#' )
#' ## Make calendar_date a date ##
#' infections_to_add$calendar_date <- lubridate::dmy(
#'   infections_to_add$calendar_date
#' )
#' ## Add this new experiment data ##
#' cg_added <- cg_add_experiment(cg, infections_to_add)
#' 
#' ## Example 2: Incorrect column names ##----------------------------
#' 
#' ## Setup as for Example 1 ##
#' infections_to_add_renamed <- infections_to_add %>%
#' dplyr::rename(ID = elig_study_id)
#' 
#' ## this fails ##
#' try(
#' cg_added_2 <- cg_add_experiment(cg, 
#' infections_to_add_renamed)
#' )
#' 
#'##------------------------------------------------------------------
cg_add_experiment <- function(
    cg,
    experiment) {
  quoted_ids_column_name <- attributes(cg)$col_ids
  ids_column_name <- rlang::as_name(quoted_ids_column_name)

  quoted_calendar_date <- attributes(cg)$col_calendar_date
  name_calendar_date <- rlang::as_name(quoted_calendar_date)

  ## Check chronogram is valid ##
  stopifnot(
    validate_chronogram(cg)
  )

  ## Check experiment is valid ##
  stopifnot(
    check_experiment(
      x = experiment,
      cg = cg
    )
  )

  ## Enforce lubridate dmy formatting on experiment ##
  # use the first 10 rows (or number of rows in experiment if <10)
  num_rows <- nrow(experiment)
  to_check <- ifelse(num_rows > 10, 10, num_rows)

  dates_dmy <- lubridate::dmy(
    experiment %>%
      dplyr::pull(quoted_calendar_date) %>% .[1:to_check],
                              quiet = TRUE)
  dates_ymd <- lubridate::ymd(
    experiment %>%
      dplyr::pull(quoted_calendar_date) %>% .[1:to_check],
                              quiet = TRUE)

  stopifnot(
    "Experiment date format not recognised by lubridate::dmy() or ymd()" =
    ! all(is.na(dates_dmy), is.na(dates_ymd)) )

  if(all(is.na(dates_dmy))) {

    experiment <- experiment %>%
      dplyr::mutate(
        {{ name_calendar_date }} :=
          lubridate::ymd(experiment %>%
                           dplyr::pull(quoted_calendar_date),
                         quiet = TRUE)
      )

  }
  if(all(is.na(dates_ymd))) {

    experiment <- experiment %>%
      dplyr::mutate(
        {{ name_calendar_date }} :=
          lubridate::dmy(experiment %>%
                           dplyr::pull(quoted_calendar_date),
                         quiet = TRUE)
      )

  }


  ## check matching columns ##
  cols_to_check <- colnames(experiment)
  ## by design,
  ## quoted_calendar_date and quoted_ids_column_name should match
  ## these do not need warnings
  cols_to_check <-
    cols_to_check[
      !grepl(cols_to_check, pattern = quoted_calendar_date)
    ]
  cols_to_check <-
    cols_to_check[
      !grepl(cols_to_check, pattern = quoted_ids_column_name)
    ]

  pre_existing <- colnames(cg)[colnames(cg) %in% cols_to_check]
  # parse for message()
  pre_existing <- paste(pre_existing, collapse = ", ")

  if (nchar(pre_existing) > 1) {
    warning(paste(
      "WARNING: Experiment columns already present in cg:",
      pre_existing, "\n",
      "These are labelled `.x` and `.y`\n"
    ))
  }


  ## stash levels of chronogram::ids ##
  lvls <- cg %>% dplyr::select(
    dplyr::contains(
      quoted_ids_column_name
    )
  )
  lvls <- levels(lvls[[1]]) # this is always only 1 column

  ## left_join based on ids_column_name, and date
  xx <- dplyr::left_join(
    cg %>%
      tibble::as_tibble() %>%
      dplyr::mutate({{ ids_column_name }} :=
        as.character(
          .data[[{{ ids_column_name }}]]
        )),
    experiment %>%
      dplyr::mutate({{ ids_column_name }} :=
        as.character(
          .data[[{{ ids_column_name }}]]
        )),
    by = c(
      quoted_ids_column_name,
      quoted_calendar_date
    )
  )

  ## re-apply the levels from chronogram_skeleton::ids ##
  xx <- xx %>%
    dplyr::mutate({{ ids_column_name }} :=
      factor(
        .data[[{{ ids_column_name }}]],
        levels = lvls
      ))

  ## set attributes, including class ##
  ## (using dplyr_reconstruct() under the hood gives class cg_tbl,
  ## but the template doesn't feed forward well, and only
  ## tibble attributes preserved.)
  attributes(xx)$class <- attributes(cg)$class
  attributes(xx)$col_calendar_date <- attributes(cg)$col_calendar_date
  attributes(xx)$col_ids <- attributes(cg)$col_ids
  attributes(xx)$cg_pkg_version <- attributes(cg)$cg_pkg_version
  attributes(xx)$windowed <- attributes(cg)$windowed
  attributes(xx)$cols_metadata <- attributes(cg)$cols_metadata

  stopifnot(
    validate_chronogram(xx)
  )

  return(xx)
}
