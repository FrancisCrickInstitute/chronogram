data("built_smallstudy")
cg <- built_smallstudy$chronogram

test_that("cg save writes a file", {
  ff <- tempfile()

  expect_invisible(
    cg_save(cg, file = ff)
  )
})
