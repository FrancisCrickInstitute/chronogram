#' cg_plot_meta
#'
#' @param date_dose_1 column containing the date of dose.
#' @param date_dose_2 column containing the date of dose.
#' @param dose_1 column containing the vaccine formulation.
#' @param dose_2 column containing the vaccine formulation.
#' @param visit column within chronogram to indicate a study visit
#'   (i.e. samples available; NA when samples not taken, !=NA when
#'   samples available). In our small study example, serum_Ab_S fills
#'   this brief.
#' @param fill column used to determine fill
#' @inheritParams cg_plot
#'
#' @description Function to create a `ggplot2` object, plotting a
#'   user-defined metadata over time in a swimmer's plot.
#'
#'   `ggplot2` objects retain the entirety of the provided dataset.
#'   This allows later adjustments, such as adding extra geom_layers
#'   with new information, or applying facets. To find this data
#'   examine `obj$data`. If you save `ggplot2` objects, all source
#'   data is ALSO saved. `cg_plot_meta()` removes any un-used data by
#'   default (`drop_vars=TRUE`). In writing a study specific
#'   `ggplot2`, it is best practice to select minimal columns before
#'   calling `ggplot()`.
#'
#' @inherit cg_plot examples
#'
#' @export


cg_plot_meta <- function(cg, x = NULL,
                         date_dose_1 = date_dose_1, dose_1 = dose_1,
                         date_dose_2 = date_dose_2, dose_2 = dose_2,
                         visit = visit,
                         drop_vars = TRUE,
                         fill = NULL) {
  message("Function provided to illustrate chronogram ->
          ggplot2 interface.\nFunction assumes the
          presence of {dose_1, date_dose_1, dose_2, date_dose_2}
          columns.
          Users are likely to want to write their own,
          study-specific applications")

  ## if you are re-deriving outside of function,
  ## then all the {{var}} can be safely removed,
  ## as columns will be referenced in aes(x=var)

  quoted_ids_column_name <- attributes(cg)$col_ids

  quoted_calendar_date <- attributes(cg)$col_calendar_date

  x <- rlang::enquo(x)
  x <- rlang::as_label(x)

  if (x == "NULL") {
    xx <- quoted_calendar_date
  } else {
    xx <- x
  }


  quoted_fill <- rlang::enquo(fill)
  quoted_fill <- rlang::as_label(quoted_fill)

  if(drop_vars) {
    cg_to_plot <- cg %>%
      dplyr::select(dplyr::any_of(
        c(
        quoted_ids_column_name,
                                xx,
        quoted_fill)
        ),
                                {{ date_dose_1 }},
                                {{ dose_1 }},
                                {{ date_dose_2 }},
                                {{ dose_2 }},
                                {{ visit }}

        )


  } else {
    cg_to_plot <- cg
  }

  ggplot2::ggplot(
    ggplot2::aes(
      x = .data[[xx]],
      y = .data[[quoted_ids_column_name]]
    ),
    data = cg_to_plot
  ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = {{ date_dose_1 }},
        col = {{ dose_1 }}
      ),
      label = 1,
      position = ggplot2::position_nudge(y = 0.1),
      data = cg_to_plot %>%
        tibble::as_tibble() %>%
        dplyr::group_by(.data[[quoted_ids_column_name]]) %>%
        dplyr::slice_head()
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = {{ date_dose_2 }},
        col = {{ dose_2 }}
      ),
      label = 2,
      position = ggplot2::position_nudge(y = 0.1),
      data = cg_to_plot %>%
        tibble::as_tibble() %>%
        dplyr::group_by(.data[[quoted_ids_column_name]]) %>%
        dplyr::slice_head(),
      show.legend = FALSE
    ) +
    {if (quoted_fill == "NULL")ggplot2::geom_point(
      data = . %>% dplyr::filter(!is.na({{ visit }})),
      position =
        ggplot2::position_nudge(y = -0.1), show.legend = FALSE
    )} +
    {if (quoted_fill != "NULL")ggplot2::geom_point(
        ggplot2::aes(fill = .data[[quoted_fill]] ),
        shape = 22, size = 2,
        data = . %>% dplyr::filter(!is.na({{ visit }})),
        position =
          ggplot2::position_nudge(y = -0.1)
      )}

}
