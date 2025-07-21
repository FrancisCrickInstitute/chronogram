data("built_smallstudy")
cg <- built_smallstudy$chronogram

## Simulate some infection data ##
infections_to_add <- tibble::tribble(
  ~calendar_date, ~elig_study_id, ~LFT, ~PCR, ~symptoms,
  "01102020", "1", "pos", NA, NA,
  "11102020", "1", "pos", NA, "severe"
)
## Make calendar_date a date ##
infections_to_add$calendar_date <- lubridate::dmy(
  infections_to_add$calendar_date
)
## add to chronogram
cg <- cg_add_experiment(cg, infections_to_add)
# ## annotate vaccines ##
# cg <- cg_annotate_vaccines_count(cg,
# dose = dose,
# dose_counter = dose_number,
# vaccine_date_stem = date_dose,
# intermediate_days = 7)

test_that("cg_annotate_vaccines_count() does not add or remove rows", {
  expect_identical(
    cg_annotate_vaccines_count(cg,
      dose = dose,
      dose_counter = dose_number,
      vaccine_date_stem = date_dose,
      intermediate_days = 7
    ) %>%
      nrow(.),
    nrow(cg)
  )
})


test_that("cg_annotate_vaccines_count() does not add or remove rows,
          when no dose 2", {
  cg.noDose2 <- cg %>% dplyr::select(!date_dose_2)

  expect_identical(
    cg_annotate_vaccines_count(cg.noDose2,
      dose = dose,
      dose_counter = dose_number,
      vaccine_date_stem = date_dose,
      intermediate_days = 7
    ) %>%
      nrow(.),
    nrow(cg)
  )
})



test_that("cg_annotate_vaccines_count() returns a tbl_chronogram", {
  aaa <- cg_annotate_vaccines_count(cg,
    dose = dose,
    dose_counter = dose_number,
    vaccine_date_stem = date_dose,
    intermediate_days = 7
  )

  expect_equal(
    validate_chronogram(aaa),
    TRUE
  )
})
