% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cg_window_by_metadata.R
\name{cg_window_by_metadata}
\alias{cg_window_by_metadata}
\title{Pick a window before and after a metadata date column}
\usage{
cg_window_by_metadata(cg, windowing_date_col, preceding_days, following_days)
}
\arguments{
\item{cg}{a chronogram}

\item{windowing_date_col}{the column containing reference date (must repeat
the date for each row of that individual's record).}

\item{preceding_days}{used as \code{filter( date > (windowing_date_col - preceding_days) )}}

\item{following_days}{used as \code{filter( date < (windowing_date_col + following_days) )}}
}
\value{
A windowed chronogram
}
\description{
Pick a window before and after a metadata date column
}
\examples{
data(pitch_chronogram)

pitch_chronogram

SevenDaysPrePostDose2 <- cg_window_by_metadata(
  pitch_chronogram, dose_2_date, 7, 7
)

SevenDaysPrePostDose2

}
\seealso{
\code{\link[=cg_window_by_episode]{cg_window_by_episode()}},
\code{\link[=cg_window_by_visit]{cg_window_by_visit()}}
}
