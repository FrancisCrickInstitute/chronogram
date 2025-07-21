#' Add a line to tbl header when printing a chronogram, or a grouped chronogram
#'
#' @param x a chronogram, or a grouped chronogram
#' (class tbl_chronogram or grouped_cg_df)
#' @param ... passed to [pillar::tbl_sum()]
#'
#' @return print to console
#'
#' @importFrom pillar tbl_sum
#' @seealso [summary()]
#' @export
#' @noRd

tbl_sum.cg_tbl <- function(x, ...) {
  default_header <- NextMethod()
  c(default_header, "A chronogram" = cli::col_blue("try summary()"))
}

#' Add a line to tbl footer when printing a chronogram, or a grouped chronogram
#' @importFrom pillar tbl_format_footer
#' @param ... passed to [pillar::tbl_format_footer()]
#' @export
#' @noRd
tbl_format_footer.cg_tbl <- function(x, setup, ...) {
  default_footer <- NextMethod()

  meta <- paste(attributes(x)$cols_metadata, collapse = ", ")

  extra_info <- paste(
                      cli::col_yellow("Dates:"),
                      attributes(x)$col_calendar_date,
                      "    ",
                      cli::col_yellow(cli::symbol$star),
                      cli::col_yellow("IDs:"),
                      attributes(x)$col_ids)

  extra_info_meta <- paste(cli::col_yellow("metadata:"), meta)

  extra_footer <- paste0(
    pillar::style_subtle(paste0("# ", cli::col_yellow(cli::symbol$star), " ", (extra_info))),
    "\n",
    pillar::style_subtle(paste0("# ", cli::col_yellow(cli::symbol$star), " ", (extra_info_meta)))
    )
  c(default_footer, extra_footer)
}


#' This is draft code to fine tune display of the pillars themselves.
#'
#' #' @importFrom pillar ctl_new_pillar
#' #' @importFrom pillar new_pillar_component
#' #' @importFrom pillar new_pillar
#' #' @export
#' ctl_new_pillar.cg_tbl <- function(controller, x, width, ..., title = NULL) {
#'   out <- NextMethod()
#'
#'   pillar::new_pillar(list(
#'     top_rule = pillar::new_pillar_component(list("========"), width = 8),
#'     title = out$title,
#'     type = out$type,
#'     mid_rule = pillar::new_pillar_component(list("--------"), width = 8),
#'     data = out$data,
#'     bottom_rule = pillar::new_pillar_component(list("========"), width = 8)
#'   ))
#' }

