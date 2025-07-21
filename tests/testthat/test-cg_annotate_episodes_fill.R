data(built_smallstudy)

small_study <- built_smallstudy$chronogram

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
small_study_inf <- cg_add_experiment(
  small_study,
  infections_to_add
)

small_study_inf <- cg_annotate_episodes_find(
  small_study_inf,
  infection_cols = c("LFT", "PCR", "symptoms"),
  infection_present = c("pos", "Post", "^severe")
)


test_that("cg_annotate_episodes_fill() returns a cg_tbl", {
  expect_s3_class(
    cg_annotate_episodes_fill(
      small_study_inf,
      col_to_fill = PCR,
      col_to_return = PCR_filled,
      .direction = "down",
      episode_numbers_col = episode_number
    ),
    "cg_tbl"
  )
})


test_that("cg_annotate_episodes_fill() returns a valid cg_tbl", {
  expect_true(
    cg_annotate_episodes_fill(
      small_study_inf,
      col_to_fill = PCR,
      col_to_return = PCR_filled,
      .direction = "down",
      episode_numbers_col = episode_number
    ) %>%
      validate_chronogram()
  )
})

test_that("cg_annotate_episodes_fill() does not add/remove rows", {
  expect_equal(
    cg_annotate_episodes_fill(
      small_study_inf,
      col_to_fill = PCR,
      col_to_return = PCR_filled,
      .direction = "down",
      episode_numbers_col = episode_number
    ) %>% nrow(.),
    nrow(small_study_inf)
  )
})

test_that("cg_annotate_episodes_fill() attributes are not changed", {
  aa <- attributes(small_study_inf)

  aa <- aa[!grepl(names(aa), pattern = "names$")]

  expect_equal(
    cg_annotate_episodes_fill(
      small_study_inf,
      col_to_fill = PCR,
      col_to_return = PCR_filled,
      .direction = "down",
      episode_numbers_col = episode_number
    ) %>% attributes(.) %>%
      .[!grepl(names(.), pattern = "names$")],
    aa
  )
})
