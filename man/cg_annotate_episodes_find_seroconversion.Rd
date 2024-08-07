% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cg_annotate_episodes_find_seroconversion.R
\name{cg_annotate_episodes_find_seroconversion}
\alias{cg_annotate_episodes_find_seroconversion}
\title{Find seroconversion episodes per individual}
\usage{
cg_annotate_episodes_find_seroconversion(
  cg,
  serum_N_titre = NULL,
  serum_N_cutoff = 1,
  N_seroconversion_episode = "N_seroconversion_episode",
  episode_number_append = "number",
  episode_start_col_append = "start",
  episode_end_col_append = "end"
)
}
\arguments{
\item{cg}{a chronogram}

\item{serum_N_titre}{the column containing seroconversion
information.}

\item{serum_N_cutoff}{the numerical threshold for seropositivity.
Greater than or equal to the threshold is taken as positive.
Default is 1 - check with your assay documentation.}

\item{N_seroconversion_episode}{the column name for resulting
episode flag. Default is "N_seroconversion_episode"}

\item{episode_number_append}{appended to
\code{N_seroconversion_episode} to provide the column name for the
episode number. Default is "number"}

\item{episode_start_col_append}{appended to
\code{N_seroconversion_episode} to provide the column name for the
episode start date. Default is "start"}

\item{episode_end_col_append}{appended to
\code{N_seroconversion_episode} to provide the column name for the
episode end date. Default is "_end"}
}
\value{
a chronogram, with episode numbers annotated
}
\description{
\code{cg_annotate_episodes_find_seroconversion()} finds
episodes based on their seroconversion. Here, we use N (SARS-CoV-2
nucleocapsid). Most covid vaccines contain Spike
only, so seroconversion to N means exposure to SARS-CoV-2. For
symptoms, lateral flow tests and SARS-CoV-2 PCR positivity, the
experimental data matches exposure to within a few days (or hours).
Seroconversion occurs anytime between blood samples. Chronogram
therefore provides separate annotation functions
\code{cg_annotate_episodes_find()} and
\code{cg_annotate_episodes_find_seroconversion()}, and options to
combine the two results.
}
\examples{
##Example 1: Small study##-----------------------------------------
library(dplyr) # for dplyr::filter()

data("built_smallstudy")

cg <- built_smallstudy$chronogram
cg <- cg_annotate_episodes_find_seroconversion(cg,
  serum_N_titre =
    "serum_Ab_N"
)

## ID==1 seroconverts to N ##
## Their first N seroconversion has dates associated ##
## Their later N positive tests are flagged without dates ##
cg \%>\% 
filter(N_seroconversion_episode_number == 1) \%>\% 
cg_select(contains("episode"))
##-------------------------------------------------------------------
}
\seealso{
\code{\link[=cg_annotate_episodes_find]{cg_annotate_episodes_find()}}
}
