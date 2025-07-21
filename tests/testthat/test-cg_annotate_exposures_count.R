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

## annotate vaccines ##
cg <- cg_annotate_vaccines_count(cg,
  dose = dose,
  dose_counter = dose_number,
  vaccine_date_stem = date_dose,
  intermediate_days = 7
)

## now infection finding ##
cg <- cg_annotate_episodes_find(cg,
  infection_cols = c("LFT", "PCR", "symptoms"),
  infection_present = c("pos", "Post", "^severe")
)

cg <- cg_annotate_episodes_find_seroconversion(cg,
  serum_N_titre =
    "serum_Ab_N"
)


test_that("cg_annotate_exposure_count does not add or remove rows", {
  expect_identical(
    cg_annotate_exposures_count(cg) %>%
      nrow(.),
    nrow(cg)
  )
})
