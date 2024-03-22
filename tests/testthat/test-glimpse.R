data("built_smallstudy")

test_that("glimpse chronogram sends something to console", {
  expect_output(glimpse(built_smallstudy$chronogram),
    regexp = NULL
  )
})

test_that("glimpse grouped chronogram sends something to console", {
  cg <- built_smallstudy$chronogram %>%
    group_by(sex)
  expect_output(glimpse(cg),
    regexp = NULL
  )
})


test_that("glimpse metadata sends something to console", {
  expect_output(glimpse_metadata(built_smallstudy$chronogram),
    regexp = NULL
  )
})

test_that("glimpse experiment data sends something to console", {
  expect_output(glimpse_experiment_data(built_smallstudy$chronogram),
    regexp = NULL
  )
})
