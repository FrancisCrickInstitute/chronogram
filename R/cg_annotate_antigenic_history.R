#' Calculate and assign flags based on infection history
#'
#' @param cg a chronogram
#'
#' @param episode_number a character vector to identify the
#'   episode_number column. Default is "episode_number".
#' @param dose_number a character vector to identify the column containing
#'   the number of doses received on that day. Default is "dose_number".
#' @param episode_variant_summarised a character vector to identify
#'   the column summarising the variant call. Default is
#'   "episode_variant_summarised".
#' @param ag_col column with infection flag
#'
#' @return A chronogram
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#' 
#' ## add infections to chronogram ##
#' cg <- cg_add_experiment(
#'   cg,
#'   built_smallstudy$infections_to_add
#' )
#' 
#' ## annotate infections ## 
#' cg <- cg_annotate_episodes_find(
#'    cg,
#'    infection_cols = c("LFT", "PCR", "symptoms"),
#'    infection_present = c("pos", "Post", "^severe")
#' )
#'
#' ## annotate vaccines ## 
#' cg <- cg %>% cg_annotate_vaccines_count(
#'  ## the prefix to the dose columns: ##
#'  dose = dose,
#'  ## the output column name: ##
#'  dose_counter = dose_number,
#'  ## the prefix to the date columns: ##
#'  vaccine_date_stem = date_dose,
#'  ## use 14d to 'star' after a dose ##
#'  intermediate_days = 14)
#' 
#' ## annotate exposures ##
#'cg <- cg %>% cg_annotate_exposures_count(
#'  episode_number = episode_number,
#'  dose_number = dose_number,
#'  ## we have not considered episodes of seroconversion
#'  N_seroconversion_episode_number = NULL
#'  )
#'  
#' ## assign variants ##
#' cg <- cg %>%
#' mutate(
#' episode_variant =
#' case_when(
#'     # "is an episode" & "PCR positive" -> Delta #
#'     (!is.na(episode_number)) & PCR == "Pos" ~ "Delta",
#'     # "is an episode" & "PCR unavailable" -> Anc/Delta #
#'     (!is.na(episode_number)) & PCR == "not tested" ~ "Anc/Alpha"
#' )
#' )
#' ## ^ this gives a variant call on a SINGLE row of each episode
#' 
#' ## fill the variant call ##
#'cg <- cg %>% cg_annotate_episodes_fill(
#'  col_to_fill = episode_variant,
#'  col_to_return = episode_variant_filled,
#'  .direction = "updown",
#'  episode_numbers_col = episode_number
#'  )
#'  
#'  cg <- cg %>% 
#'  mutate(
#'  episode_variant_summarised =    episode_variant_filled
#'  ) %>%
#'  cg_annotate_antigenic_history(
#'      episode_number = episode_number,
#'          dose_number = dose_number,
#'          episode_variant_summarised = episode_variant_summarised,
#'              ag_col = antigenic_history
#'              )
#'
#'  ## and finally:
#'  summary(factor(cg$antigenic_history))
#'
cg_annotate_antigenic_history <- function(
    cg,
    episode_number = episode_number,
    dose_number = dose_number,
    episode_variant_summarised = episode_variant_summarised,
    ag_col = antigenic_history) {
  attributes_x <- attributes(cg)

  ids_column_name <- attributes_x$col_ids
  calendar_date <- attributes_x$col_calendar_date

  quoted_episode_number <-
    rlang::as_name(
      rlang::enquo(episode_number)
    )

  stopifnot(
    "Chronogram must contain a \"dose_number\" column,
    showing number of doses at this calendar date.
    Consider using the cg_annotate_vaccines_count()
    function first." = "dose_number" %in% colnames(cg)
  )

  ## make a vector of with times of individuals ##
  y <- cg %>%
    tibble::as_tibble() %>%
    group_by({{ ids_column_name }}) %>%
    dplyr::filter(!is.na({{ episode_number }})) %>%
    group_by(
      dplyr::across(
        dplyr::all_of(
          c(
            ids_column_name,
            quoted_episode_number
          )
        )
      )
    ) %>%
    dplyr::slice_head() %>%
    dplyr::select(
      dplyr::all_of(ids_column_name),
      {{ episode_number }},
      {{ dose_number }},
      {{ episode_variant_summarised }}
    ) %>%
    dplyr::mutate(
      "{{ dose_number }}" :=
        paste0(
          "D",
          {{ dose_number }},
          "_",
          {{ episode_variant_summarised }}
        )
    ) %>%
    dplyr::summarise(
      "{{ ag_col }}" :=
        paste({{ dose_number }}, collapse = ",")
    ) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(ids_column_name)
        )
      )
    ) %>%
    dplyr::summarise("{{ ag_col }}" :=
      paste({{ ag_col }},
        collapse = ","
      )) %>%
    dplyr::select(
      dplyr::all_of(ids_column_name),
      {{ ag_col }}
    )

  ## if episode_number is always NA,
  ## that person does not have a combined
  ## reported ag_col.

  z <- cg %>%
    tibble::as_tibble() %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(ids_column_name)
        )
      )
    ) %>%
    dplyr::filter(all(is.na({{ episode_number }}))) %>%
    dplyr::summarise("{{ ag_col }}" :=
      paste("D",
        max(
          {{ dose_number }} %>%
            stringr::str_remove(., pattern = "star"),
          na.rm = TRUE
        ),
        "_not_infected",
        sep = ""
      )) %>%
    dplyr::select(
      dplyr::all_of(ids_column_name),
      {{ ag_col }}
    ) %>%
    dplyr::ungroup()

  stopifnot(
    "IDs present both in infected and uninfected sets..." =
      (!(z[[ids_column_name]] %in% y[[ids_column_name]]))
  )

  y <- dplyr::bind_rows(z, y)

  y <- dplyr::left_join(cg, y, by = ids_column_name)

  validate_chronogram(y)

  return(y)
}
