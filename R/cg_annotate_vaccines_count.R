#' Calculate and assign flags based on vaccine history
#'
#' @param cg a chronogram
#' @param vaccine_date_stem the start of the vaccine date columns.
#'   Default is NULL. Try "date_dose"; asssumes "date_dose_{1,2,3...}"
#' @param dose a character vector to use to label the dose column.
#'   This is the numerical dose, filled only on the date of that dose
#'   (and empty on intervening days). Default is "dose".
#' @param dose_counter column to return the cumulative number of
#'   doses. Default is NULL. Try "dose_number"
#' @param intermediate_days number of days to pad after dose date to
#'   return as eg "1star" if within intermediate days after dose 1,
#'   numeric
#'
#' @return An annotated chronogram (class cg_tbl).
#' @export
#'
#' @examples
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#'
#' ## annotate vaccines ##
#' cg <- cg_annotate_vaccines_count(cg,
#'   dose = dose,
#'   dose_counter = dose_number,
#'   vaccine_date_stem = date_dose,
#'   intermediate_days = 7
#' )
#'
cg_annotate_vaccines_count <- function(
    cg,
    vaccine_date_stem = NULL,
    dose = dose,
    dose_counter = NULL,
    intermediate_days = 14) {
  attributes_x <- attributes(cg)

  ids_column_name <- attributes_x$col_ids
  calendar_date <- attributes_x$col_calendar_date

  quoted_vaccine_date_stem <- NULL # stop visible binding note

  quosure_vaccine_date_stem <- rlang::enquo(vaccine_date_stem)

  quoted_vaccine_date_stem <-
    rlang::as_name(quosure_vaccine_date_stem)

  quoted_dose <-
    rlang::as_name(
      rlang::enquo(dose)
    )

  targets <- colnames(cg)[stringr::str_detect(colnames(cg),
    pattern = quoted_vaccine_date_stem
  )]

  # stopifnot("No vaccine date columns found" = {
  #   length(targets) < 1 })

  message(
    "Using stem: ", paste(quoted_vaccine_date_stem),
    "\nFound vaccine dates\n", paste(targets, collapse = "\n\n")
  )

  ## fix dose dates for each person ##
  xxx <- cg %>%
    tibble::as_tibble() %>%
    group_by(.data[[{{ ids_column_name }}]]) %>%
    dplyr::slice_head() %>%
    dplyr::select(
      dplyr::all_of(targets),
      {{ ids_column_name }}
    ) %>%
    ungroup()

  xxx <- xxx %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(targets),
      values_to = {{ calendar_date }},
      names_to = quoted_dose,
      names_prefix =
        paste0(quoted_vaccine_date_stem, "_")
    )

  cg <- cg %>%
    dplyr::left_join(xxx,
      by = c(
        {{ calendar_date }},
        {{ ids_column_name }}
      )
    ) %>%
    dplyr::relocate(dplyr::all_of(dplyr::contains(quoted_dose)),
      .after = targets[length(targets)]
    )

  cg <- cg %>%
    dplyr::mutate({{ quoted_dose }} :=
      as.numeric(.data[[{{ quoted_dose }}]]))



  ## dose counting ##
  ## step 1 build some extra dose columns ##
  #
  ## DEV: a for loop to pad these columns
  # for(i in 2:10) {
  #   if(! paste(quoted_vaccine_date_stem, i, sep = "_") %in% colnames(x)) {
  #     x <- x %>%
  #       dplyr::mutate(
  #         "{{ vaccine_date_stem }}_{{i}}":= NA)
  #   }
  # }

  if (!paste(quoted_vaccine_date_stem, "5", sep = "_") %in% colnames(cg)) {
    cg <- cg %>%
      dplyr::mutate(
        "{{ vaccine_date_stem }}_5" := NA
      )
  }

  if (!paste(quoted_vaccine_date_stem, "4", sep = "_") %in% colnames(cg)) {
    cg <- cg %>%
      dplyr::mutate(
        "{{ vaccine_date_stem }}_4" := NA
      )
  }

  if (!paste(quoted_vaccine_date_stem, "3", sep = "_") %in% colnames(cg)) {
    cg <- cg %>%
      dplyr::mutate(
        "{{ vaccine_date_stem }}_3" := NA
      )
  }

  if (!paste(quoted_vaccine_date_stem, "2", sep = "_") %in% colnames(cg)) {
    cg <- cg %>%
      dplyr::mutate(
        "{{ vaccine_date_stem }}_2" := NA
      )
  }

  ###
  y <- cg %>%
    tibble::as_tibble() %>%
    dplyr::group_by(.data[[{{ ids_column_name }}]]) %>%
    dplyr::arrange({{ calendar_date }}, .by_group = TRUE) %>%
    tidyr::fill({{ dose }}, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "{{ dose_counter }}" :=
        dplyr::case_when(
          (
            (.data[[{{ calendar_date }}]] >=
              .data[[{{ glue::glue(quoted_vaccine_date_stem, "_5") }}]]) &

              (.data[[{{ calendar_date }}]] <
                (.data[[{{ glue::glue(quoted_vaccine_date_stem, "_5") }}]] +
                  .env$intermediate_days))
          ) ~ "5star",
          (.data[[{{ calendar_date }}]] >=
            .data[[{{ glue::glue(quoted_vaccine_date_stem, "_5") }}]]) ~ "5",
          (
            (.data[[{{ calendar_date }}]] >=
              .data[[{{ glue::glue(quoted_vaccine_date_stem, "_4") }}]]) &

              (.data[[{{ calendar_date }}]] <
                (.data[[{{ glue::glue(quoted_vaccine_date_stem, "_4") }}]] +
                  .env$intermediate_days))
          ) ~ "4star",
          (.data[[{{ calendar_date }}]] >=
            .data[[{{ glue::glue(quoted_vaccine_date_stem, "_4") }}]]) ~ "4",
          (
            (.data[[{{ calendar_date }}]] >=
              .data[[{{ glue::glue(quoted_vaccine_date_stem, "_3") }}]]) &

              (.data[[{{ calendar_date }}]] <
                (.data[[{{ glue::glue(quoted_vaccine_date_stem, "_3") }}]] +
                  .env$intermediate_days))
          ) ~ "3star",
          (.data[[{{ calendar_date }}]] >=
            .data[[{{ glue::glue(quoted_vaccine_date_stem, "_3") }}]]) ~ "3",
          (
            (.data[[{{ calendar_date }}]] >=
              .data[[{{ glue::glue(quoted_vaccine_date_stem, "_2") }}]]) &

              (.data[[{{ calendar_date }}]] <
                (.data[[{{ glue::glue(quoted_vaccine_date_stem, "_2") }}]] +
                  .env$intermediate_days))
          ) ~ "2star",
          (.data[[{{ calendar_date }}]] >=
            .data[[{{ glue::glue(quoted_vaccine_date_stem, "_2") }}]]) ~ "2",
          (
            (.data[[{{ calendar_date }}]] >=
              .data[[{{ glue::glue(quoted_vaccine_date_stem, "_1") }}]]) &

              (.data[[{{ calendar_date }}]] <
                (.data[[{{ glue::glue(quoted_vaccine_date_stem, "_1") }}]] +
                  .env$intermediate_days))
          ) ~ "1star",
          (.data[[{{ calendar_date }}]] >=
            .data[[{{ glue::glue(quoted_vaccine_date_stem, "_1") }}]]) ~ "1",
          (.data[[{{ calendar_date }}]] <
            .data[[{{ glue::glue(quoted_vaccine_date_stem, "_1") }}]]) ~ "0"
        )
    )

  y <- y %>%
    dplyr::relocate(dplyr::all_of(dplyr::contains("{{ dose_counter }}")),
      .after = dplyr::any_of(dplyr::contains("{{quoted_dose}}"))
    )

  #
  #   ## remove the added columns
  ## this looks back from dose 5
  for (i in 5:1) {
    if (
      all(is.na(
        y[, {{ glue::glue(quoted_vaccine_date_stem, "_", i) }}]
      ))) {
      y <- y %>%
        dplyr::select(
          !dplyr::all_of(glue::glue(quoted_vaccine_date_stem, "_", i))
        )
    }
  }


  ## set attributes, including class ##
  attributes(y)$class <- attributes_x$class
  attributes(y)$col_calendar_date <- attributes_x$col_calendar_date
  attributes(y)$col_ids <- attributes_x$col_ids
  attributes(y)$cg_pkg_version <- attributes_x$cg_pkg_version
  attributes(y)$windowed <- attributes_x$windowed
  attributes(y)$cols_metadata <- attributes_x$cols_metadata

  return(y)
}
