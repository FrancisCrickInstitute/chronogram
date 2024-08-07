% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cg_window_by_visit.R
\name{cg_window_by_visit}
\alias{cg_window_by_visit}
\title{Pick a window of dates from a chronogram: with respect to visits.}
\usage{
cg_window_by_visit(cg, visit_col = NULL, preceding_days, following_days)
}
\arguments{
\item{cg}{a chronogram}

\item{visit_col}{a column with either dates of visits, or results.
This is usually an experimental data column. Default is NULL.
Provide unquoted.}

\item{preceding_days}{used as filter( date > (date_col -
preceding_days) )}

\item{following_days}{used as filter( date < (date_col +
following_days) )}
}
\value{
A subsetted chronogram
}
\description{
This is a special case of \code{cg_window_by_episode()}. It treats the
visit column as an episode, with the argument \code{episode_handling = "all"}.
}
\examples{

data("built_smallstudy")
cg <- built_smallstudy$chronogram

SevenDaysAfterEachStudyVisit <- cg_window_by_visit(
  cg,
  visit_col = serum_Ab_S, 7, 7
)

}
\seealso{
\code{\link[=cg_window_by_episode]{cg_window_by_episode()}},
\code{\link[=cg_window_by_metadata]{cg_window_by_metadata()}}
}
