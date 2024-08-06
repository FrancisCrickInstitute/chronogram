#' Fill experimental, treatment or symptom information within an infection episode
#'
#' @param cg a chronogram
#' @param col_to_fill the name of the column to fill within each
#'   episode
#' @param col_to_return the name of the returned, filled column
#' @param .direction See `tidyr::fill()`, provide quoted.
#' @param episode_numbers_col The column name to use for episode
#'   numbers. Default is `episode_number`, unquoted.
#'
#' @return a chronogram, with episode data filled within each
#'   episode.
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
cg_annotate_episodes_fill <- function(cg,
                                      col_to_fill,
                                      col_to_return,
                                      .direction = "down",
                                      episode_numbers_col = episode_number) {
  attributes_x <- attributes(cg)

  quoted_episode_numbers_col <-
    rlang::as_name(
      rlang::enquo(episode_numbers_col)
    )

  ids_column_name <- attributes_x$col_ids
  calendar_date <- attributes_x$col_calendar_date

  x <- cg %>%
    dplyr::ungroup()

  xx <- x %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na({{ episode_numbers_col }})) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(
            ids_column_name,
            quoted_episode_numbers_col
          )
        )
      )
    ) %>%
    dplyr::arrange(calendar_date) %>%
    dplyr::mutate("{{ col_to_return }}" := {{ col_to_fill }}) %>%
    tidyr::fill({{ col_to_return }}, .direction = .direction) %>%
    dplyr::ungroup()

  xxx <- xx %>%
    dplyr::select(
      dplyr::all_of(ids_column_name),
      dplyr::all_of(quoted_episode_numbers_col),
      {{ col_to_return }}
    ) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(
            ids_column_name,
            quoted_episode_numbers_col
          )
        )
      )
    ) %>%
    dplyr::slice_head() %>%
    dplyr::ungroup()

  y <- dplyr::left_join(x, xxx)

  return(y)
}
