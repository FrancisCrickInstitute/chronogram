#' cg_plot
#' @param cg chronogram
#' @param x a column of time to use as x axis. If NULL, will default
#'   to the chronogram's calendar date attribute. A user may want to
#'   derive and use alternatives eg 'daysSinceDose2'.
#' @param y_values column within chronogram containing the data you
#'   wish to plot.
#' @param drop_vars Default TRUE. See description.
#' @param point_alpha alpha for `geom_point()`.
#' @param point_shape shape for `geom_point()`.
#' @param link_obs Default TRUE. Draw a line to link results from same
#'   individual?
#' @param link_colour colour for `geom_line()`
#' @param link_alpha alpha for `geom_line()`
#' @param ... passed to `aes()`
#'
#' @description Create a `ggplot2` object, plotting a
#'   user-defined y axis over time.
#'
#'   `ggplot2` objects retain the entirety of the provided dataset.
#'   This allows later adjustments, such as adding extra geom_layers
#'   with new information, or applying facets. To find this data
#'   examine `obj$data`. If you save `ggplot2` objects, all source
#'   data is ALSO saved. `cg_plot()` removes any un-used data by
#'   default (`drop_vars=TRUE`). In writing a study specific
#'   `ggplot2`, it is best practice to select minimal columns before
#'   calling `ggplot()`.
#'
#'
#' @examples
#' 
#' library(ggplot2)
#' library(patchwork)
#' 
#' data("built_smallstudy")
#' cg <- built_smallstudy$chronogram
#'
#' p1 <- cg_plot_meta(cg,
#'   visit = serum_Ab_S
#' )
#'
#' p2 <- cg_plot(cg,
#'   y_values = serum_Ab_S
#' )
#' 
#' p2 / p1
#'
#' (p2 + scale_y_log10()) / p1
#'
#' @export


cg_plot <- function(cg, x = NULL, y_values,
                    drop_vars = TRUE,
                    point_alpha = 0.4,
                    point_shape = 20,
                    link_obs = TRUE,
                    link_colour = "grey",
                    link_alpha = 0.4, ...) {
  message("Function provided to illustrate chronogram ->
          ggplot2 interface.
          Users are likely to want to write their own,
          study-specific applications")

  ## if you are re-deriving outside of function,
  ## columns can be referenced eg `aes(x=var)`,
  ## rather than {{ var }}

  quoted_ids_column_name <- attributes(cg)$col_ids

  quoted_calendar_date <- attributes(cg)$col_calendar_date

  x <- rlang::enquo(x)
  x <- rlang::as_label(x)

  if (x == "NULL") {
    xx <- quoted_calendar_date
  } else {
    xx <- x
  }



  if(drop_vars) {
    cg_to_plot <- cg %>%
      dplyr::select(dplyr::any_of(
        c(
          quoted_ids_column_name,
          xx)
      ),
      {{ y_values }}
      )


  } else {
    cg_to_plot <- cg
  }

  cg_to_plot %>%
    dplyr::mutate(
      "{{y_values}}" :=
        as.numeric({{ y_values }})
    ) %>%
    dplyr::filter(!is.na({{ y_values }})) %>%
    ggplot2::ggplot(
      data = .,
      ggplot2::aes(
        x = .data[[xx]],
        y = {{ y_values }}, ...
      )
    ) +
    {
      if (link_obs) {
        ggplot2::geom_line(
          ggplot2::aes(group = .data[[quoted_ids_column_name]]),
          colour = link_colour, alpha = link_alpha
        )
      }
    } +
    ggplot2::geom_point(alpha = point_alpha, shape = point_shape)
}
