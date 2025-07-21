test_that("metadata: contains IDs column", {
  data(smallstudy)
  x <- smallstudy$small_study_metadata

  expect_error(.valid_metadata(small.study, ids_column_name = "studyID"))
})
