#' A helper function to add experimental data to a chronogram
#'
#' @param cg a chronogram object (class tbl_chronogram)
#' @param experiment a tibble containing experimental data (with
#'   columns: calendar_date, and the specified ID)
#'
#' @return An object of chronogram class
#' @seealso [chronogram::chronogram_skeleton()], [chronogram::chronogram()]
#' @export
#'
#' @examples
#' \dontrun{
#' ## a 3-person chronogram_skeleton ##
#' small_study <- chronogram_skeleton(
#'   col_ids = elig_study_id,
#'   ids = c(1, 2, 3),
#'   start_date = c("01012020"),
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
#'
#' ## Create a tibble of exemplar experimental data ##
#' # suggest naming assays as {source}_{test}, eg serum_Ab
#' # as SARS-CoV-2 PCRs could be reasonably performed
#' # on stool, blood, sputum, BAL etc.
#' # chronogram package does not enforce any rules here.
#' #
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
#' ## Add to chronogram ##
#' small_study_chronogram <- cg_add_experiment(
#'   small_study_chronogram,
#'   small_study_Ab
#' )
#' }
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
