#' Calculate and assign flags based on infection history
#'
#' @param x a chronogram
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
#' \dontrun{
#' cg <- cg_annotate_label_episodes(cg)
#' }
#'
cg_annotate_antigenic_history <- function(
    x,
    episode_number = episode_number,
    dose_number = dose_number,
    episode_variant_summarised = episode_variant_summarised,
    ag_col = antigenic_history) {
  attributes_x <- attributes(x)

  ids_column_name <- attributes_x$col_ids
  calendar_date <- attributes_x$col_calendar_date

  quoted_episode_number <-
    rlang::as_name(
      rlang::enquo(episode_number)
    )

  stopifnot(
    "Chronogram x must contain a \"dose_number\" column,
    showing number of doses at this calendar date.
    Consider using the cg_annotate_vaccines_count()
    function first." = "dose_number" %in% colnames(x)
  )

  ## make a vector of with times of individuals ##
  y <- x %>%
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

  z <- x %>%
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

  y <- dplyr::left_join(x, y, by = ids_column_name)

  validate_chronogram(y)

  return(y)
}
