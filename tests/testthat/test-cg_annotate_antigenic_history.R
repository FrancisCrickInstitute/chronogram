data("built_smallstudy")
small_study <- built_smallstudy$chronogram

## Simulate some variant data ##
variants_to_add <- tibble::tribble(
  ~calendar_date, ~elig_study_id, ~LFT, ~PCR, ~SGTF, ~symptoms,
  "01062020", "1", "pos", NA, "S present", NA,
  "01022021", "1", NA, NA, NA, "severe",
  "11022021", "1", "pos", NA, "S fail", "severe"
)

## Make calendar_date a date ##
variants_to_add$calendar_date <- lubridate::dmy(
  variants_to_add$calendar_date
)
## add to chronogram ##
small_study_variants <- cg_add_experiment(
  small_study,
  variants_to_add
)

small_study_variants <- cg_annotate_episodes_find(
  small_study_variants,
  infection_cols = c("LFT", "PCR", "symptoms"),
  infection_present = c("pos", "Post", "^severe")
)

small_study_variants <- cg_annotate_vaccines_count(small_study_variants,
  dose_counter = dose_number,
  vaccine_date_stem = date_dose,
  intermediate_days = 14
)

## tests #####

test_that("valid chronogram errors if class not tbl_chronogram", {
  small_study <- cg_annotate_antigenic_history(
    small_study_variants,
    episode_variant_summarised = "SGTF"
  )

  class(small_study) <- class(small_study)[-1]

  expect_error(validate_chronogram(small_study))
})



test_that("cg_annotate_antigenic_history
          outputs class cg_tbl", {
  small_study <- cg_annotate_antigenic_history(
    small_study_variants,
    episode_variant_summarised = "SGTF"
  )

  expect_s3_class(small_study, "cg_tbl")
})


test_that("cg_annotate_antigenic_history
          outputs valide cg_tbl", {
  small_study <- cg_annotate_antigenic_history(
    small_study_variants,
    episode_variant_summarised = "SGTF"
  )

  expect_true(validate_chronogram(small_study))
})



test_that("cg_annotate_antigenic_history()
          outputs the same nrow as input", {
  expect_equal(
    nrow(small_study),
    cg_annotate_antigenic_history(
      small_study_variants,
      episode_variant_summarised = "SGTF"
    ) %>%
      nrow()
  )
})


test_that("cg_annotate_antigenic_history()
          adds columns to input", {
  expect_true(
    ncol(small_study) <
      cg_annotate_antigenic_history(
        small_study_variants,
        episode_variant_summarised = "SGTF"
      ) %>%
        ncol()
  )
})
