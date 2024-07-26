ids <- c(1, 2, 3)
start_date <- "01012020"
end_date <- "10102021"

small_study <- chronogram::chronogram_skeleton(
  ids = ids,
  start_date = start_date,
  end_date = end_date
)

test_that("chronogram_skeleton: correct class", {
  expect_s3_class(
    small_study,
    "tbl_df"
  )
})

test_that("chronogram_skeleton: start date after end date errors", {
  expect_error(
    chronogram_skeleton(
      ids = ids,
      start_date = end_date,
      end_date = start_date
    ),
    "Start date is later than end date"
  )
})



test_that("chronogram_skeleton: two columns", {
  expect_equal(
    ncol(small_study),
    2
  )
})

test_that("chronogram_skeleton: correct row number", {
  interval <- as.numeric(
    lubridate::dmy(end_date) - lubridate::dmy(start_date)
  )
  interval <- interval + 1

  expect_equal(
    nrow(small_study),
    interval *
      length(ids[unique(ids)])
  )
})


test_that("chronogram_skeleton: duplicate IDs give a warning", {
  ## duplicated ids ###
  ids <- c(1, 2, 3, 3)

  expect_warning(
    chronogram_skeleton(
      ids = ids,
      start_date = start_date,
      end_date = end_date
    ),
    paste("Duplicate IDs found : ", ids[duplicated(ids)],
      "Check your input ID vector...
      provide a unique identifier for
      each individual\nReturning a de-duplicated chronogram_skeleton",
      collapse = " "
    )
  )
})



test_that(
  "chronogram_skeleton: includes a cg_pkg_version number in attributes",
  {
    expect_false(
      is.na(attributes(small_study)$cg_pkg_version) |
        is.null(attributes(small_study)$cg_pkg_version)
    )
  }
)

test_that(
  "chronogram_skeleton: includes an ID column in attributes",
  {
    expect_false(
      is.na(attributes(small_study)$col_ids) |
        is.null(attributes(small_study)$col_ids)
    )
  }
)

test_that(
  "chronogram_skeleton: includes an date column in attributes",
  {
    expect_false(
      is.na(attributes(small_study)$col_calendar_date) |
        is.null(attributes(small_study)$col_calendar_date)
    )
  }
)


test_that(
  "chronogram_skeleton: date column in attributes is a column name",
  {
    expect_true(
      attributes(small_study)$col_calendar_date %in% colnames(small_study)
    )
  }
)

test_that(
  "chronogram_skeleton: ID column in attributes is a column name",
  {
    expect_true(
      attributes(small_study)$col_ids %in% colnames(small_study)
    )
  }
)



