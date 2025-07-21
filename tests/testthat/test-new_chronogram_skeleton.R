data("smallstudy")
data("built_smallstudy")

cg_skeleton_by_hand <-
  tibble::tibble(cal_date = seq.Date(
    from = lubridate::dmy("01012020"),
    to = lubridate::dmy("10102021"),
    by = 1
  ))

cg_skeleton_by_hand <- tidyr::crossing(
  cg_skeleton_by_hand,
  tibble::tibble(ID := c(1, 2, 3))
)

## need a wrapper function to mirror real-world NSE ####
testing_fun <- function(x, col_ids, col_calendar_date) {
  new_chronogram_skeleton(x,
    calendar_date_col = rlang::enquo(col_calendar_date),
    ids_col = rlang::enquo(col_ids)
  )
}


## now tests themselves ####

test_that("new_chronogram_skeleton input x requires tibble", {
  expect_error(
    testing_fun(
      ## not a tibble ##
      x = c(1, 2, 3),
      col_ids = ID, col_calendar_date = cal_date
    )
  )
})


test_that("new_chronogram_skeleton requires a present calendar date column", {
  expect_error(
    testing_fun(
      x = cg_skeleton_by_hand,
      col_ids = ID, col_calendar_date = cal_date_2
    ),
    "Supplied calendar date col not present in x"
  )
})

test_that("new_chronogram_skeleton requires a present ID column", {
  expect_error(
    testing_fun(
      x = cg_skeleton_by_hand,
      col_ids = ID_column_misnamed, col_calendar_date = cal_date
    ),
    "Supplied ids_column_name not present in x"
  )
})



test_that("new_chronogram_skeleton returns expected attribute slots", {
  cg_s <- testing_fun(
    x = cg_skeleton_by_hand,
    col_ids = ID, col_calendar_date = cal_date
  )

  expect_equal(
    names(attributes(cg_s)),
    names(attributes(built_smallstudy$chronogram_skeleton))
  )
})


test_that("new_chronogram_skeleton returns correct class", {
  cg_s <- testing_fun(
    x = cg_skeleton_by_hand,
    col_ids = ID, col_calendar_date = cal_date
  )

  expect_equal(
    class(cg_s),
    class(built_smallstudy$chronogram_skeleton)
  )
})
