#' Small study
#'
#' A fictional covid-19 vaccine study of 3 individuals. Provided a
#' vector of participant IDs, a tibble of metadata, and a tibble of
#' experimental data.
#'
#' @format ## `smallstudy` A list of three components
#' \describe{
#'   \item{small_study_ids}{IDs used in
#'   small study to identify these 3 individuals. A vector.}
#'   \item{small_study_metadata}{Example metadata. A tibble.}
#'   \item{small_study_Ab}{Example experimental data. A tibble.}
#' }
#' @examples data("smallstudy")
"smallstudy"



#' Small study: pre-built chronogram
#'
#' A fictional covid-19 vaccine study of 3 individuals. Provided a
#' vector of participant IDs, a tibble of metadata, and a tibble of
#' experimental data.
#'
#' The data are identical to `smallstudy`. `cg_assemble()` has been
#' run to shorten examples.
#'
#' @format ## `built_smallstudy`
#' A list of 2 components
#' \describe{
#'   \item{small_study_chronogram_skeleton}{Example chronogram_skeleton}
#'   \item{small_study_chronogram}{Example chronogram}
#' }
#' @examples data("built_smallstudy")
"built_smallstudy"




#' PITCH study: pre-built chronogram
#'
#' A provided example derived from the de-identified PITCH dataset
#' available here, under a CC-BY-4.0 licence:
#' https://data.mendeley.com/datasets/fyp26zjgmj/1 
#' 
#' Published: 3 November 2021,
#' 
#' DOI: 10.17632/fyp26zjgmj.1
#' 
#' Contributors: Rebecca Payne, Susan Hopkins, Victoria Hall,
#' Christina Dold, Christopher Duncan, Alex Richter, Miles Carroll,
#' Gavin Screaton, Thushan de Silva, Lance Turtle, Paul Klenerman,
#' Susanna Dunachie, PITCH Consortium authors.
#'
#' The dataset supports Payne et al. Immunogenicity of standard and
#' extended dosing intervals of BNT162b2 mRNA vaccine Cell 2021.
#'
#' NOTE: Changes have been made to the de-identified public dataset
#' (DOI: 10.17632/fyp26zjgmj.1).
#' 
#' 1. the public data reports dates as
#' MM/YYYY. To build a chronogram we have assumed 15/MM/YYYY for dose
#' 1, and used the available intervals in days to place the remaining
#' data in time. For illustration purposes, we need plausible
#' DD/MM/YYYY dates - they are not real.
#' 
#' 2. not all of the public
#' dataset has been used in this example (some assays not included).
#'
#' NB: If your motivation is to re-analyse Payne et al. for scientific
#' reasons, rather than to explore this package, do not use this
#' example dataset. Instead, consult the de-identified Mendeley data
#' (DOI: 10.17632/fyp26zjgmj.1), or the Cell manuscript's data and
#' code availability statement.
#'
#' @format ## `pitch_chronogram` A chronogram
#' \describe{
#'   \item{pitch_chronogram}{Example pitch_chronogram}
#' }
#' @examples data("pitch_chronogram")
"pitch_chronogram"
