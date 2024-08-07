% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cg_load.R
\name{cg_load}
\alias{cg_load}
\title{Load a chronogram}
\usage{
cg_load(file = NULL)
}
\arguments{
\item{file}{quoted filename. Include an extension.}
}
\value{
re-loaded chronogram
}
\description{
Load into memory a chronogram previously written to disk by \code{cg_save()}.
}
\examples{
data("built_smallstudy")
cg_orig <- built_smallstudy$chronogram
ff <- tempfile()
cg_save(cg_orig, file = ff)
cg_new <- cg_load(ff)

summary(cg_new == cg_orig)
identical(cg_new, cg_orig)

}
\seealso{
\code{\link[=cg_save]{cg_save()}}
}
