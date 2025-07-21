#' Find seroconversion episodes per individual
#'
#' @description `cg_annotate_episodes_find_seroconversion()` finds
#' episodes based on their seroconversion. Here, we use N (SARS-CoV-2
#' nucleocapsid). Most covid vaccines contain Spike
#' only, so seroconversion to N means exposure to SARS-CoV-2. For
#' symptoms, lateral flow tests and SARS-CoV-2 PCR positivity, the
#' experimental data matches exposure to within a few days (or hours).
#' Seroconversion occurs anytime between blood samples. Chronogram
#' therefore provides separate annotation functions
#' `cg_annotate_episodes_find()` and
#' `cg_annotate_episodes_find_seroconversion()`, and options to
#' combine the two results.
#'
#'
#' @param cg a chronogram
#' @param serum_N_titre the column containing seroconversion
#'   information.
#' @param serum_N_cutoff the numerical threshold for seropositivity.
#'   Greater than or equal to the threshold is taken as positive.
#'   Default is 1 - check with your assay documentation.
#' @param N_seroconversion_episode the column name for resulting
#'   episode flag. Default is "N_seroconversion_episode"
#' @param episode_number_append appended to
#'   `N_seroconversion_episode` to provide the column name for the
#'   episode number. Default is "number"
#' @param episode_start_col_append appended to
#'   `N_seroconversion_episode` to provide the column name for the
#'   episode start date. Default is "start"
#' @param episode_end_col_append appended to
#'   `N_seroconversion_episode` to provide the column name for the
#'   episode end date. Default is "_end"
#'
#' @return a chronogram, with episode numbers annotated
#' @seealso [chronogram::cg_annotate_episodes_find()]
#' @export
#'
#' @examples
#' ##Example 1: Small study##-----------------------------------------
#' library(dplyr) # for dplyr::filter()
#' 
#' data("built_smallstudy")
#'
#' cg <- built_smallstudy$chronogram
#' cg <- cg_annotate_episodes_find_seroconversion(cg,
#'   serum_N_titre =
#'     "serum_Ab_N"
#' )
#' 
#' ## ID==1 seroconverts to N ##
#' ## Their first N seroconversion has dates associated ##
#' ## Their later N positive tests are flagged without dates ##
#' cg %>% 
#' filter(N_seroconversion_episode_number == 1) %>% 
#' cg_select(contains("episode"))
#'##-------------------------------------------------------------------
cg_annotate_episodes_find_seroconversion <- function(
    cg,
    serum_N_titre = NULL,
    serum_N_cutoff = 1,
    N_seroconversion_episode = "N_seroconversion_episode",
    episode_number_append = "number",
    episode_start_col_append = "start",
    episode_end_col_append = "end") {
  calendar_date <- attributes(cg)$col_calendar_date
  ids_column_name <- attributes(cg)$col_ids

  x <- cg # UI consistency
  
  ## check serum_N_titre column exists ##
  stopifnot("User entered serum_N_titre not found" = {
    {{ serum_N_titre }} %in% colnames(x)
  })

  ## check serum_N_cutoff is a number ##
  stopifnot("User entered serum_N_cutoff not numeric" = {
    as.numeric(serum_N_cutoff) == TRUE
  })


  ## N_output column name parser ##
  N_seropos_episode_number <-
    paste(
      N_seroconversion_episode,
      episode_number_append,
      sep = "_"
    )
  N_seropos_episode_number <- rlang::enquo(N_seropos_episode_number)

  N_seropos_episode_start <-
    paste(
      N_seroconversion_episode,
      episode_start_col_append,
      sep = "_"
    )
  N_seropos_episode_start <- rlang::enquo(N_seropos_episode_start)


  N_seropos_episode_end <-
    paste(
      N_seroconversion_episode,
      episode_end_col_append,
      sep = "_"
    )
  N_seropos_episode_end <- rlang::enquo(N_seropos_episode_end)

  ## Infer start date ##
  start_date <- min(x %>% dplyr::pull({{ calendar_date }}))

  ## check new columns names are not already in chronogram object ##
  stopifnot(
    "User entered N_seroconversion_episode already present" = {
      !grepl(
        x = colnames(x),
        pattern = "N_seroconversion_episode"
      )
    }
  )

  ## Actual selection ##
  xx <- x %>%
    tibble::as_tibble() %>%
    dplyr::group_by(.data[[{{ ids_column_name }}]]) %>%
    dplyr::arrange({{ calendar_date }}, .by_group = TRUE) %>%
    dplyr::filter(!is.na(.data[[{{ serum_N_titre }}]])) %>%
    # Lag shifts a vector by n positions (n=1 by default),
    # this creates an empty value which is padded with NA.
    # This can be used to look at the previous observation
    # when the data is ordered by calendar date.

    # Seroconversion episodes should be flagged if
    # the N titre is >=1 and
    # the previous titre was <1 OR
    # NA (i.e. seroconversion before enrolment)
    # The seroconversion window starts between the +ve titre (>=1)
    # and the previous -ve titre (<1)
    # If seroconverted before enrolment, the window
    # begins at start of the pandemic (here, chronogram start_date)
    # The window ends at the date the +ve titre was observed.

    dplyr::mutate(
      Previous_N_titre =
        dplyr::lag(.data[[{{ serum_N_titre }}]],
          n = 1,
          default = NA
        )
    )

  xx <- xx %>%
    dplyr::mutate(
      {{ N_seroconversion_episode }} :=
        dplyr::case_when(
          (.data[[{{ serum_N_titre }}]] >= .env$serum_N_cutoff &
            .data[["Previous_N_titre"]] < .env$serum_N_cutoff) ~ "yes",
          (.data[[{{ serum_N_titre }}]] >= .env$serum_N_cutoff &
            is.na(.data[["Previous_N_titre"]])) ~ "yes"
        )
    )

  xx <- xx %>%
    dplyr::mutate(
      {{ N_seropos_episode_start }} :=
        dplyr::case_when(
          # seroconverted with prev == NA
          (
            .data[[{{ N_seroconversion_episode }}]] == "yes" &
              is.na(.data[["Previous_N_titre"]])
          ) ~
            as.Date(.env$start_date),
          # seroconverted with prev != NA
          (
            .data[[{{ N_seroconversion_episode }}]] == "yes" &
              !is.na(.data[["Previous_N_titre"]])
          ) ~
            as.Date(dplyr::lag(.data[[{{ calendar_date }}]], n = 1))
        )
    )


  xx <- xx %>%
    dplyr::mutate(
      {{ N_seropos_episode_end }} :=
        dplyr::case_when(
          (.data[[{{ N_seroconversion_episode }}]] == "yes") ~
            .data[[{{ calendar_date }}]]
        )
    )


  xx <- xx %>%
    dplyr::mutate(
      ## convert "yes" into 1 ##
      {{ N_seropos_episode_number }} :=
        dplyr::case_when(
          (.data[[{{ N_seroconversion_episode }}]] == "yes") ~ 1
        )
    ) %>%
    ## count, replacing NA with O ##
    dplyr::mutate(
      {{ N_seropos_episode_number }} :=
        base::cumsum(
          dplyr::coalesce(
            .data[[!!N_seropos_episode_number]], 0
          )
        )
    )


  xx <- xx %>%
    dplyr::ungroup() %>%
    dplyr::select(c(
      {{ calendar_date }},
      {{ ids_column_name }},
      {{ N_seroconversion_episode }},
      {{ N_seropos_episode_number }},
      {{ N_seropos_episode_start }},
      {{ N_seropos_episode_end }}
    ))

  xxx <- dplyr::left_join(
    x, xx,
    by = c({{ calendar_date }}, {{ ids_column_name }})
  )

  return(xxx)
}
