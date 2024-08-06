#' Find infections per individual
#'
#' @param cg a chronogram
#' @param infection_cols A vector of column names that contain
#'   information regarding infection status, such as symptoms, LFT
#'   results, PCR results, or sequencing results.
#' @param infection_present Strings used in each of `infection_cols`
#'   to indicate the presence of infection.
#' @param episode_days The number of days to scan forwards and
#'   backwards in time to consider events a single episode. For
#'   example, is symptom onset `10d` after a positive PCR test a
#'   single episode? The default is `14d`.
#' @param episode_numbers_col The column name to use for episode
#'   numbers. Default is `episode_number` (unquoted).
#' @param episode_start_col,episode_end_col The column names to store
#'   the episode start and end dates (defaults: `episode_start` and
#'   `episode_end`, both unquoted).
#'
#' @return x a chronogram, with episode numbers annotated
#' @seealso [chronogram::cg_annotate_episodes_find_seroconversion()]
#' @export
#'
#' @examples
#' ## Example 1: A small study ##-------------------------------------
#' data(built_smallstudy)
#' cg_small <- built_smallstudy$chronogram
#' infections_to_add <- built_smallstudy$infections_to_add
#'
#' ## add infection data to to chronogram ##
#' cg_small <- cg_add_experiment(cg_small, infections_to_add)
#'
#' ## now infection finding ##
#' cg_small_inf <- cg_annotate_episodes_find(cg_small,
#'   infection_cols = c("LFT", "PCR", "symptoms"),
#'   infection_present = c("pos", "Post", "^severe")
#' )
#' 
#' summary(cg_small_inf$episode_number)
#'
#' ## exact text matching ##------------------------------------------
#' test2 <- cg_annotate_episodes_find(cg_small_inf,
#'   infection_cols = c("LFT", "PCR", "symptoms"),
#'   infection_present = c("Pos", "Post", "^mild")
#' )
#' 
#' summary(test2$episode_number)
#'
#' ## empty strings will error (as they otherwise match everything) ##
#' test3a <- 
#' try(
#' cg_annotate_episodes_find(cg_small_inf,
#'   infection_cols = c("LFT", "PCR", "symptoms"),
#'   infection_present = c("pos", "Post", "")
#' )
#' )
#' ## a 'random' string will not error ##-----------------------------
#' test3b <- try(
#' cg_annotate_episodes_find(cg_small_inf,
#'   infection_cols = c("LFT", "PCR", "symptoms"),
#'   infection_present = c("pos", "Post", "a")
#' )
#' )
##--------------------------------------------------------------------
cg_annotate_episodes_find <- function(
    cg,
    infection_cols,
    infection_present,
    episode_days = 14,
    episode_numbers_col = episode_number,
    episode_start_col = episode_start,
    episode_end_col = episode_end) {
  attributes_x <- attributes(cg)

  ids_column_name <- attributes_x$col_ids
  calendar_date <- attributes_x$col_calendar_date

  x <- cg # cleaner UI with `cg` across functions #
  
  ## message to screen ##
  message('Parsed: infection_cols and infection_present
          \nSearching in the [[column]], for the "text": \n\n')
  conds <- .make_detects_exprs(
    infection_cols = infection_cols,
    infection_present = infection_present
  )
  message(paste(conds, collapse = "\n\n"), sep = "\n")

  stopifnot(
    "infection_present seems to have one more empty condition(s)" =
      any(grepl(conds, pattern = '""')) == FALSE
  )

  message("\n\n...detecting will be exact.
          Capitals, spelling etc must be precise\n\n")

  ## this stops a warning re no visible binding for variable.
  episode_present_flag <- NULL

  ## apply this case_when ##
  xx <- x %>%
    dplyr::mutate(episode_present_flag = dplyr::case_when(!!!conds))

  ## count episodes
  episode.counts <- xx %>%
    tibble::as_tibble() %>%
    dplyr::filter(episode_present_flag == "yes") %>%
    dplyr::group_by(.data[[{{ ids_column_name }}]]) %>%
    dplyr::mutate("{{episode_numbers_col}}" :=
      base::cumsum(
        c(
          TRUE,
          base::diff(
            calendar_date
          ) >
            episode_days
        )
      )) %>%
    dplyr::ungroup()



  quoted_episode_numbers_col <- rlang::enquo(episode_numbers_col)
  quoted_episode_numbers_col <- rlang::as_label(quoted_episode_numbers_col)

  columns_to_select <- c(
    calendar_date,
    ids_column_name, quoted_episode_numbers_col
  )

  cat(columns_to_select)

  episode.counts <- episode.counts %>%
    dplyr::select(dplyr::all_of(
      columns_to_select
    ))



  ## Now we bookend the earliest and latest dates
  ## Intervening calendar_dates are also labelled with episode number
  ## (this makes it much easier to do fill() to allow fuzzy matching
  ## of PCR + sequencing results done on different days)
  episode.counts <- episode.counts %>%
    dplyr::group_by(
      .data[[ids_column_name]],
      {{ episode_numbers_col }}
    ) %>%
    dplyr::summarise(
      "{{ episode_start_col }}" :=
        base::min(!!rlang::ensym(calendar_date)),
      "{{ episode_end_col }}" :=
        base::max(!!rlang::ensym(calendar_date))
    )

  ## Convert that vector to a nested list, then unnest ###
  episode.counts <- episode.counts %>%
    dplyr::rowwise() %>%
    dplyr::mutate(!!calendar_date :=
      base::list(base::seq.Date(
        from = {{ episode_start_col }},
        to = {{ episode_end_col }}, by = 1
      ))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(calendar_date)


  xx <- dplyr::left_join(xx, episode.counts)


  stopifnot(validate_chronogram(xx))

  return(xx)
}


## this uses glue to make a single expression ##
## the return is hardcoded to be "yes"
## (we want to use map2, and therefore are not mapping a third input)
.make_detect_expr <- function(col, entry_in_col) {
  base::requireNamespace("stringr")
  rlang::parse_expr(
    glue::glue(
      'stringr::str_detect(.data[[\"{col}\"]], \"{entry_in_col}\") ~ "yes"'
    )
  )
}


## purrr::map2 maps the two vectors of columns and +ve calls ##
.make_detects_exprs <- function(infection_cols, infection_present) {
  purrr::map2(infection_cols, infection_present, .make_detect_expr)
}
