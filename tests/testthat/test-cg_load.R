data("built_smallstudy")
cg_orig <- built_smallstudy$chronogram

test_that("cg_save()+cg_load() returns
          identical chronogram to the source data", {
  ff <- tempfile()
  cg_save(cg_orig, file = ff)
  cg_new <- cg_load(ff)

  expect_identical(
    cg_new,
    cg_orig
  )
})
