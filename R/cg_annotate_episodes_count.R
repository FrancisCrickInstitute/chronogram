#' Calculate and assign flags based on infection history
#'
#' Count the total number of infections for each individual at the end
#' of the study (or at that data freeze), and make it available as a
#' fresh column. `cg_annotate_episodes_count()` should be run after
#' `cg_annotate_episodes_find()`. For the cumulative number of
#' infections use `cg_annotate_episodes_find()` only.
#'
#' @param cg a chronogram
#' @param episode_number a character vector to use to label the ID
#'   column. Default is "episode_number".
#' @param count_col column with infection count
#'
#'
#' @return A subsetted chronogram
#' @seealso [chronogram::cg_annotate_episodes_find()]
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#' 
#' ## add infections to chronogram
#' cg <- cg_add_experiment(
#'   cg,
#'   built_smallstudy$infections_to_add
#' )
#' 
#' ## annotate infections 
#' cg <- cg_annotate_episodes_find(
#'    cg,
#'    infection_cols = c("LFT", "PCR", "symptoms"),
#'    infection_present = c("pos", "Post", "^severe")
#' )
#' 
#' ## count infections
#' cg <- cg_annotate_episodes_count(
#' cg)
#'
#' ## pull number of infections each individual has during study ##
#' cg %>% 
#'   dplyr::group_by(elig_study_id) %>% 
#'   dplyr::slice_head() %>% 
#'   dplyr::select(elig_study_id, count_col)
#'
cg_annotate_episodes_count <- function(cg,
                                       episode_number = episode_number,
                                       count_col = count_col) {
  calendar_date <- attributes(cg)$col_calendar_date
  ids_column_name <- attributes(cg)$col_ids

  ## make a vector of with number of infections per individuals ##
  y <- cg %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(ids_column_name)
      )
    ) %>%
    dplyr::group_by({{ episode_number }}, .add = TRUE) %>%
    dplyr::slice_head() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(ids_column_name)
      )
    ) %>%
    dplyr::summarise(
      "{{ count_col }}" :=
        ## suppress warnings [individuals with 0 infections] ##
        suppressWarnings(
          max({{ episode_number }}, na.rm = TRUE)
        )
    ) %>%
    ## fix -Inf to be 0 infections ##
    dplyr::mutate("{{ count_col }}" := dplyr::case_when(
      {{ count_col }} == -Inf ~ 0,
      {{ count_col }} > 0 ~ suppressWarnings(
        as.numeric({{ count_col }})
      )
    ))


  y <- dplyr::left_join(cg, y, by = ids_column_name)
  return(y)
}
