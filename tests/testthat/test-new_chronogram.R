data("smallstudy")
data("built_smallstudy")

metadata <- smallstudy$small_study_metadata
metcols <- colnames(metadata)
metcols <- metcols[!grepl(metcols, pattern = "id|ID")]


small_study_chronogram_skeleton <- built_smallstudy$chronogram_skeleton

by_hand_cg <- dplyr::left_join(
  small_study_chronogram_skeleton %>%
    dplyr::mutate(elig_study_id = as.character(elig_study_id)),
  metadata %>%
    dplyr::mutate(elig_study_id = as.character(elig_study_id))
)



test_that("input chronogram skeleton is class tbl_df", {
  expect_s3_class(
      metadata,
      c("tbl_df", "tbl", "data.frame"),
      exact = T
    )
})

test_that("input metadata_cols not a character vector", {
  expect_error(
    ## provide an input object of wrong class ##
    new_chronogram(
      by_hand_cg,
      metadata
    ),
    "metadata_cols not provided as a character vector"
  )
})

test_that("new_chronogram function returns correct class", {
  expect_s3_class(
    new_chronogram(by_hand_cg, metcols),
    c("cg_tbl", "tbl_df", "tbl", "data.frame"),
    exact = T
  )
})



test_that("new_chronogram function returns correct attributes", {
  aa <- new_chronogram(by_hand_cg, metcols)
  bb <- attributes(aa)
  expect_equal(
    names(bb),
    names(
      attributes(built_smallstudy$chronogram)
    )
  )
})
