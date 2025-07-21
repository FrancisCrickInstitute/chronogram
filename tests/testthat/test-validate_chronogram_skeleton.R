ids <- c(1, 2, 3)
start_date <- "01012020"
end_date <- "10102021"

small_study <- chronogram_skeleton(
  ids = ids,
  start_date = start_date,
  end_date = end_date
)

test_that(
  "validate_chronogram_skeleton fails if col_ids attribute absent",
  {
    small_study_2 <- small_study
    attributes(small_study_2)$col_ids <- "new_id_column_name"

    expect_error(
      validate_chronogram_skeleton(small_study_2),
      "Invalid cg_skeleton: col_id attribute not a column name"
    )
  }
)

test_that(
  "validate_chronogram_skeleton fails if
  col_calendar_date attribute absent",
  {
    small_study_3 <- small_study
    attributes(small_study_3)$col_calendar_date <- "new_date_column_name"

    expect_error(validate_chronogram_skeleton(small_study_3))
  }
)


test_that("validate_chronogram_skeleton fails if not two columns", {
  expect_error(
    validate_chronogram_skeleton(small_study %>%
      dplyr::mutate(extra_col = "dummy")),
    "Invalid chronogram_skeleton: it must contain two columns only"
  )
})


test_that("validate_chronogram_skeleton fails
          if neither columns are date class", {
  small_study_4 <- small_study
  xx <- attributes(small_study_4)$col_calendar_date

  small_study_4 <- small_study_4 %>%
    dplyr::mutate({{ xx }} := as.character(.data[[{{ xx }}]]))

  expect_error(
    validate_chronogram_skeleton(small_study_4)
  )
})


test_that("validate_chronogram_skeleton returns
          TRUE when acceptable object", {
  expect_equal(
    validate_chronogram_skeleton(small_study),
    expected = TRUE
  )
})
