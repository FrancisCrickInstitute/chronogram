% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_grouped_chronogram.R
\name{new_tbl2chronogram}
\alias{new_tbl2chronogram}
\title{new_tbl2chronogram}
\usage{
new_tbl2chronogram(x, ..., class = character())
}
\arguments{
\item{x}{tibble to coerce}

\item{...}{passed to \code{tibble::new_tibble()}}

\item{class}{a character vector of classes}
}
\description{
Coerce a tibble to a chronogram. This
function handles grouped chronograms. See
\code{chronogram::chronogram()} for a constructor.
}
