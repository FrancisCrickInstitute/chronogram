#' Calculate and assign flags based on infection history
#'
#' This counts the total number of infections for each individual at
#' the end of the study (or at that data freeze), and makes it
#' available as a fresh column. this function should be run after
#' `cg_annotate_episodes_find()`. For the cumulative number of
#' infections use `cg_annotate_episodes_find()` only.
#'
#' @param x a chronogram
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
#' \dontrun{
#' cg <- cg_annotate_episodes_count(cg)
#' }
#'
cg_annotate_episodes_count <- function(x,
                                       episode_number = episode_number,
                                       count_col = count_col) {
  calendar_date <- attributes(x)$col_calendar_date
  ids_column_name <- attributes(x)$col_ids

  ## make a vector of with number of infections per individuals ##
  y <- x %>%
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


  y <- dplyr::left_join(x, y, by = ids_column_name)
  return(y)
}
