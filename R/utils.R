#' @name dplyr::group_by
#' @keywords internal
#' @importFrom dplyr group_by
#' @aliases group_by
#' @export group_by

# auto_calendar_date_finder <- function(x) {
#   stopifnot(
#     "Error: please supply an object of class
#     chronogram_skeleton or chronogram
#     Use chronogram_skeleton() or chronogram()" = {
#       c("tbl_chronogram_skeleton") %in% class(x) |
#         c("tbl_chronogram") %in% class(x)
#     }
#   )
#
#   variable <- NULL ## to stop the "visible binding" note.
#
#   calendar_date <- x %>%
#     ## take first two columns of chronogram (i.e. was chronogram_skeleton)
#     ## if chronogram_skeleton is provided, that's already 2 columns
#     dplyr::select(1:2) %>%
#     ## class of all columns ##
#     dplyr::summarise_all(class) %>%
#     ## count for each type of class ##
#     tidyr::gather(variable, class) %>%
#     ## filter to exact match "Date" ##
#     dplyr::filter(class == "Date") %>%
#     dplyr::pull(variable)
#
#   return(calendar_date)
# }
#
# #-------------------------------------------------------------------#
#
# auto_ids_finder <- function(x) {
#   stopifnot(
#     "Error: please supply an object of class
#     chronogram_skeleton or chronogram
#     Use chronogram_skeleton() or chronogram()" = {
#       c("tbl_chronogram_skeleton") %in% class(x) |
#         c("tbl_chronogram") %in% class(x)
#     }
#   )
#
#   variable <- NULL ## to stop the "visible binding" note.
#
#   ids_column_name <- x %>%
#     ## take first two columns of chronogram (i.e. was chronogram_skeleton)
#     dplyr::select(1:2) %>%
#     ## class of all columns ##
#     dplyr::summarise_all(class) %>%
#     ## count for each type of class ##
#     tidyr::gather(variable, class) %>%
#     ## filter to exact match "Date" ##
#     dplyr::filter(class != "Date") %>%
#     dplyr::pull(variable)
# }
