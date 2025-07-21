data("built_smallstudy")

chrono <- built_smallstudy$chronogram
chrono <- chrono %>%
  dplyr::mutate(
    daysSinceDose2 = calendar_date - date_dose_2
  )


test_that("cg_plot() returns ggplot2", {
  expect_s3_class(
    cg_plot(chrono,
      y_values = serum_Ab_S
    ), "ggplot"
  )
})


test_that("cg_plot() returns ggplot2 with drop_vars = FALSE", {
  expect_s3_class(
    cg_plot(chrono,
            y_values = serum_Ab_S,
            drop_vars = FALSE
    ), "ggplot"
  )
})



test_that("cg_plot() returns ggplot2, with a non-default x", {
  expect_s3_class(
    cg_plot(chrono,
      x = daysSinceDose2,
      y_values = serum_Ab_S
    ), "ggplot"
  )
})



test_that("cg_plot() returns a message", {
  expect_message(
    cg_plot(chrono,
      y_values = serum_Ab_S
    )
  )
})
