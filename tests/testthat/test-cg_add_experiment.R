## Fictional example data ##
data(smallstudy)
data(built_smallstudy)

## 5 requirements ##
ids <- smallstudy$small_study_ids

start <- "01012020"
end <- "10102021"

meta <- smallstudy$small_study_metadata
ab <- smallstudy$small_study_Ab # here, we just have antibody data

## Make a chronogram ##
small_study <- chronogram_skeleton(
  ids = ids,
  start_date = start,
  end_date = end,
  ## change this to your ID column name ##
  col_ids = elig_study_id,
  col_calendar_date = calendar_date
)

small_study <- chronogram(small_study, meta)

test_that("error if calendar_date is not parsed by lubridate", {


  ab$calendar_date <- stringr::str_replace_all(ab$calendar_date,
                                           "2021",
                                           "oops")

  expect_error(
    cg_add_experiment(
      small_study, ab),

    "Experiment date format not recognised "
  )
})

test_that("ymd calendar_date is parsed by lubridate", {


  ab$calendar_date <- format(ab$calendar_date, "%d-%b-%Y")

  expect_identical(
    cg_add_experiment(small_study, ab),
    built_smallstudy$chronogram

  )
})


