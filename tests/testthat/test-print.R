data("built_smallstudy")

test_that("Print chronogram_skeleton prints something to console", {
  expect_output(print(built_smallstudy$chronogram_skeleton),
    regexp = NULL
  )
})


test_that("Print chronogram prints something to console", {
  expect_output(print(built_smallstudy$chronogram),
    regexp = NULL
  )
})


test_that("Print grouped chronogram prints something to console", {
  cg <- built_smallstudy$chronogram %>%
    group_by(sex)
  expect_output(print(cg),
    regexp = NULL
  )
})
