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


test_that("cg_window_by_episode() outputs class chronogram", {
  expect_true(
    cg_window_by_episode(small_study_inf,
      episode_handling = "first",
      preceding_days = 7,
      following_days = 7
    ) %>%
      inherits(., "cg_tbl")
  )
})

test_that("cg_window_by_episode() outputs attributes(cg)$windowed == TRUE", {
  expect_true({
    a <- cg_window_by_episode(small_study_inf,
      episode_handling = "first",
      preceding_days = 7,
      following_days = 7
    )
    attributes(a)$windowed == TRUE
  })
})


test_that("cg_window_by_episode() errors if
          episode handling option is not available", {
  expect_error(
    cg_window_by_episode(small_study_inf,
      episode_handling = "everything",
      preceding_days = 7,
      following_days = 7
    ),
    "episode_handling must be one of first, last, or all"
  )
})


test_that("cg_window_by_episode() errors if
          preceding days not numeric", {
  expect_error(
    cg_window_by_episode(small_study_inf,
      episode_handling = "all",
      preceding_days = NA,
      following_days = 7
    ),
    "Please provide preceding days as a numerical value"
  )
})

test_that("cg_window_by_episode() errors if
          preceding days not numeric", {
  expect_error(
    cg_window_by_episode(small_study_inf,
      episode_handling = "first",
      preceding_days = 7,
      following_days = NA
    ),
    "Please provide following days as a numerical value"
  )
})

test_that("cg_window_by_episode(episode_handling = 'first')
          outputs the same format with less rows", {
  expect_equal(
    colnames(cg_window_by_episode(small_study_inf,
      episode_handling = "first",
      preceding_days = 7,
      following_days = 7
    )),
    colnames(small_study_inf)
  )
  expect_false(
    nrow(
      cg_window_by_episode(
        small_study_inf,
        episode_handling = "last",
        preceding_days = 7,
        following_days = 7
      )
    ) == nrow(small_study_inf)
  )
})




test_that("cg_window_by_episode(episode_handling = 'first')
          outputs the same format with less rows", {
  expect_equal(
    colnames(cg_window_by_episode(small_study_inf,
      episode_handling = "first",
      preceding_days = 7,
      following_days = 7
    )),
    colnames(small_study_inf)
  )
  expect_false(
    nrow(cg_window_by_episode(
      small_study_inf,
      episode_handling = "first",
      preceding_days = 7,
      following_days = 7
    )) == nrow(small_study_inf)
  )
})


test_that("cg_window_by_episode(episode_handling = 'last')
          outputs the same format with less rows", {
  expect_equal(
    colnames(cg_window_by_episode(small_study_inf,
      episode_handling = "last",
      preceding_days = 7,
      following_days = 7
    )),
    colnames(small_study_inf)
  )
  expect_false(
    nrow(
      cg_window_by_episode(
        small_study_inf,
        episode_handling = "last",
        preceding_days = 7,
        following_days = 7
      )
    ) == nrow(small_study_inf)
  )
})




test_that("cg_window_by_episode(episode_handling = 'last')
          outputs the same format with less rows", {
  expect_equal(
    colnames(cg_window_by_episode(small_study_inf,
      episode_handling = "last",
      preceding_days = 7,
      following_days = 7
    )),
    colnames(small_study_inf)
  )
  expect_false(
    nrow(cg_window_by_episode(
      small_study_inf,
      episode_handling = "last",
      preceding_days = 7,
      following_days = 7
    )) == nrow(small_study_inf)
  )
})



test_that("cg_window_by_episode(episode_handling = 'all')
          outputs the same format with less rows", {
  expect_equal(
    colnames(cg_window_by_episode(small_study_inf,
      episode_handling = "all",
      preceding_days = 7,
      following_days = 7
    )),
    colnames(small_study_inf)
  )
  expect_false(
    nrow(
      cg_window_by_episode(
        small_study_inf,
        episode_handling = "all",
        preceding_days = 7,
        following_days = 7
      )
    ) == nrow(small_study_inf)
  )
})




test_that("cg_window_by_episode(episode_handling = 'all')
          outputs the same format with less rows", {
  expect_equal(
    colnames(cg_window_by_episode(small_study_inf,
      episode_handling = "all",
      preceding_days = 7,
      following_days = 7
    )),
    colnames(small_study_inf)
  )
  expect_false(
    nrow(cg_window_by_episode(
      small_study_inf,
      episode_handling = "all",
      preceding_days = 7,
      following_days = 7
    )) == nrow(small_study_inf)
  )
})
