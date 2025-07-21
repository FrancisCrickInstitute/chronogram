data(built_smallstudy)

chrono <- built_smallstudy$chronogram

chrono$test_dates <- lubridate::dmy("01032020")

pre <- 3
post <- 3




test_that("cg_window_by_metadata() outputs class chronogram", {
  expect_true(
    cg_window_by_metadata(chrono,
      windowing_date_col = test_dates,
      preceding_days = pre,
      following_days = post
    ) %>%
      inherits(., "cg_tbl")
  )
})

test_that("cg_window_by_metadata() outputs attributes(cg)$windowed == TRUE", {
  expect_true({
    a <- cg_window_by_metadata(chrono,
      windowing_date_col = test_dates,
      preceding_days = pre,
      following_days = post
    )
    attributes(a)$windowed == TRUE
  })
})

test_that("cg_window_by_metadata() selects appropriate num of rows", {
  expect_equal(
    nrow(cg_window_by_metadata(chrono,
      windowing_date_col = test_dates,
      preceding_days = pre,
      following_days = post
    )), (pre + post) * nlevels(factor(chrono$elig_study_id))
  )
})


test_that("cg_window_by_metadata() does not drop columns", {
  expect_equal(
    colnames(cg_window_by_metadata(chrono,
      windowing_date_col = test_dates,
      preceding_days = pre,
      following_days = post
    )), colnames(chrono)
  )
})
