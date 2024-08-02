
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chronogram

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/FrancisCrickInstitute/chronogram/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FrancisCrickInstitute/chronogram/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of `chronogram` is to “cast” and annotate metadata, laboratory
and clinical data into a tidy-like data structure. This bridges between
a LIMS / database style data warehouse and data that is ready for
interrogation to test biological hypotheses.

`chronogram` was designed during the SARS-CoV-2 pandemic (2019-).
However, it is pathogen, vaccine and symptoms agnostic. It can be
adapted for any pathogen, studies that have several pathogens/vaccines
combinations.

------------------------------------------------------------------------

## Installation

Install the current version from [GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("FrancisCrickInstitute/chronogram")
```

If you have not installed packages from github before, you will to
[setup your GitHub account to interact with
R](https://usethis.r-lib.org/articles/git-credentials.html#practical-instructions).

------------------------------------------------------------------------

## Why should I use `chronogram`?

There are three reasons:

- To aggregate study data **regularly**, and **repetitively**. Perhaps
  your study has rolling recruitment, ongoing data generation or
  incremental analysis. Outsource that effort to `chronogram`.

- To **reproducibly aggregate** data within and **across several studies
  and users**. Stop troubleshooting joins by hand.

- To provide a **versatile** data shape **poised** for **new or
  follow-up analyses** without needing re-aggregation.

------------------------------------------------------------------------

## When shouldn’t I use `chronogram`?

Your study is **completed**. You have assembled a clean, de-duplicated
and fully annotated data object. You have **finished all data
analysis**. Congratulations! Nothing to gain from using this package.

------------------------------------------------------------------------

## How do I use `chronogram`?

The `chronogram` workflow can be divided into assembly, annotation and
finally, filtering, windowing and selecting data for a specific
analysis.

### chronogram assembly

- `cg_assemble()` combines cleaned metadata, experimental data, and a
  range of calendar dates into a chronogram.

- `cg_add_experiment()` allows the adding of further experiments

See the [assembly vignette](docs/articles/assembly.html) for an in-depth
guide. The [SQL vignette](articles/SQL_assembly.html) walks through
chronogram assembly from an SQL database. The [input checking
vignette](docs/articles/input_checking.html) explores how
`cg_assemble()` checks the input data.

### chronogram annotation

Symptoms, point-of-care tests, and laboratory tests of infection rarely
occur on exactly the same study day. `chronogram` finds, fills and
annotates these tests and symptoms into episodes of infection. More
details in the [annotation vignette](docs/articles/annotation.html).

### chronogram filtering, window and select

- `dplyr::filter()` to filter a chronogram based on metadata (eg vaccine
  formulation)

- `cg_window_by_metadata()` to window around an event such as 14 days
  after each participant’s vaccine

- `cg_window_by_episode()` picks a window around infection episodes

Focus on the bigger questions, let `chronogram` slice and pick your
data.

------------------------------------------------------------------------

## Analysis-ready data

There are two common scientific objectives to these kinds of studies.

Firstly, to assess vaccine response, for example by boosts in antibody
titres. This assessment is affected by prior exposures (infections),
vaccine formulations (and other covariates). Vaccine responses are
easily explored from a chronogram, as shown in the vaccine boost
vignette.

Secondly, XXX XXX XXX XXX Survival analysis

## The chronogram class

A chronogram is a tibble sub-class, with two extensions:

- Attributes that describe XXX (and other chronogram functions use these
  attributes slots rather than requiring user to re-specify columns).
- Each combination of date x participant ID is present only once. Each
  chronogram function checks this assertion.

The chronogram sub-class is a light-weight extension, any tidyverse verb
should work. We have added `print()` \[via `pillar` package\] and
`summary()` methods, but tried to keep the `tibble` “feel”.
