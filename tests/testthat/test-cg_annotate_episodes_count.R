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

test_that("cg_annotate_episodes_count()
          outputs the same nrow and 1 extra column
          with provided column name", {
  expect_equal(
    colnames(
      cg_annotate_episodes_count(small_study_inf,
        count_col = checking_count_col
      )
    ),
    c(colnames(small_study_inf), "checking_count_col")
  )
  # expect_equal(
  #   nrow(
  #     cg_annotate_episodes_count(
  #       small_study_inf,
  #       ids_column_name = elig_study_id,
  #       episode_number = "episode_number",
  #       count_col = checking_count_col)) ==
  #     nrow(small_study_inf)
  # )
})

test_that("cg_annotate_episodes_count()
          outputs a column with class numeric", {
  expect_true(
    cg_annotate_episodes_count(
      small_study_inf,
      count_col = checking_count_col
    ) %>%
      dplyr::pull(checking_count_col) %>% is.numeric(.)
  )
})


test_that("cg_annotate_episodes_count()
          outputs a column that has no negative values", {
  expect_true(
    cg_annotate_episodes_count(
      small_study_inf,
      count_col = checking_count_col
    ) %>%
      dplyr::pull(checking_count_col) %>% min(.) >= 0
  )
})


test_that("cg_annotate_episodes_count()
          outputs a cg_tbl", {
  expect_s3_class(
    cg_annotate_episodes_count(
      small_study_inf,
      count_col = checking_count_col
    ),
    "cg_tbl"
  )
})
