## Fictional example data ##
data(smallstudy)

## 5 requirements ##
ids <- smallstudy$small_study_ids

start <- "01012020"
end <- "10102021"

meta <- smallstudy$small_study_metadata
ab <- smallstudy$small_study_Ab # here, we just have antibody data

small_study_treatment <-
  tibble::tribble(
    ~elig_study_id, ~calendar_date, ~treatment,
    1, "01/02/2021", "RTX"
  )
small_study_treatment <- small_study_treatment %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::contains("date"), ~ lubridate::dmy(.x)
    )
  )


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

test_that("valid chronogram errors if class not tbl_chronogram", {
  small_study <- cg_add_treatment(
    small_study,
    small_study_treatment
  )

  class(small_study) <- class(small_study)[-1]

  expect_error(validate_chronogram(small_study))
})



test_that("object is class cg_tbl", {
  small_study <- cg_add_treatment(
    small_study,
    small_study_treatment
  )

  expect_s3_class(small_study, "cg_tbl")
})



test_that("cg_add_treatment()
          outputs the same nrow as input", {
  expect_equal(
    nrow(small_study),
    cg_add_treatment(
      small_study,
      small_study_treatment
    ) %>%
      nrow()
  )
})


test_that("cg_add_treatment()
          adds columns to input", {
  expect_true(
    ncol(small_study) <
      cg_add_treatment(
        small_study,
        small_study_treatment
      ) %>%
        ncol()
  )
})
