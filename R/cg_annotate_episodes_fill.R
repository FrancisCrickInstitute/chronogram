#' Find infections per individual
#'
#' @param x a chronogram
#' @param col_to_fill the name of the column to fill within each
#'   episode
#' @param col_to_return the name of the returned, filled column
#' @param .direction See `tidyr::fill()`, provide quoted.
#' @param episode_numbers_col The column name to use for episode
#'   numbers. Default is `episode_number`, unquoted.
#'
#' @return x a chronogram, with episode data filled within each
#'   episode.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## Simulate some infection data ##
#' infections_to_add <- tibble::tribble(
#'   ~calendar_date, ~elig_study_id, ~LFT, ~PCR, ~symptoms,
#'   "01102020", "1", "pos", NA, NA,
#'   "11102020", "1", "pos", NA, "severe"
#' )
#' ## Make calendar_date a date ##
#' infections_to_add$calendar_date <- lubridate::dmy(
#'   infections_to_add$calendar_date
#' )
#' ## add to chronogram
#' small.study <- cg_add_experiment(small.study, infections_to_add,
#'   ids_column_name = "elig_study_id",
#'   calendar_date = "calendar_date"
#' )
#'
#' ## now infection finding ##
#' small.study.inf <- cg_annotate_episodes_find(small.study,
#'   infection_cols = c("LFT", "PCR", "symptoms"),
#'   infection_present = c("pos", "Post", "^severe")
#' )
#' summary(small.study.inf$episode_number)
#'
#' ## exact text matching ##
#' test2 <- cg_annotate_episodes_find(small.study,
#'   infection_cols = c("LFT", "PCR", "symptoms"),
#'   infection_present = c("Pos", "Post", "^mild")
#' )
#' summary(test2$episode_number)
#'
#' ## empty strings will error (as they otherwise match everything) ##
#' test3a <- cg_annotate_episodes_find(small.study,
#'   infection_cols = c("LFT", "PCR", "symptoms"),
#'   infection_present = c("pos", "Post", "")
#' )
#' ## a 'random' string will not error ##
#' test3b <- cg_annotate_episodes_find(small.study,
#'   infection_cols = c("LFT", "PCR", "symptoms"),
#'   infection_present = c("pos", "Post", "a")
#' )
#'
#' summary(test2$episode_number)
#' }
cg_annotate_episodes_fill <- function(x,
                                      col_to_fill,
                                      col_to_return,
                                      .direction = "down",
                                      episode_numbers_col = episode_number) {
  attributes_x <- attributes(x)

  quoted_episode_numbers_col <-
    rlang::as_name(
      rlang::enquo(episode_numbers_col)
    )

  ids_column_name <- attributes_x$col_ids
  calendar_date <- attributes_x$col_calendar_date

  x <- x %>%
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
