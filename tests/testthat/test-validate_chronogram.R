data("built_smallstudy")

cg <- built_smallstudy$chronogram

test_that("validate_chronogram errors if class not tbl_chronogram", {
  class(cg) <- class(cg)[-1]

  expect_error(validate_chronogram(cg))
})


test_that("validate_chronogram returns TRUE when acceptable object", {
  expect_equal(validate_chronogram(cg), expected = TRUE)
})
