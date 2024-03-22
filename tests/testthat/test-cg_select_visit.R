data(built_smallstudy)
cg <- built_smallstudy$chronogram

## in deployed use will be windowed
attributes(cg)$windowed <- TRUE


test_that("cg_select_visit() errors if input chronogram is not windowed", {
  expect_error(
    cg_select_visit(built_smallstudy$chronogram,
      visit = "earliest",
      visit_col = serum_Ab_S
    ),
    "cg not windowed: use a cg_window family function first"
  )
})


test_that("cg_select_visit() outputs class chronogram", {
  expect_true(
    cg_select_visit(cg,
      visit = "earliest",
      visit_col = serum_Ab_S
    ) %>%
      inherits(., "cg_tbl")
  )
})

test_that("cg_select_visit(visit='earliest') outputs fewer rows", {
  expect_equal(
    colnames(
      cg_select_visit(cg,
        visit = "earliest",
        visit_col = serum_Ab_S
      )
    ),
    colnames(cg)
  )

  expect_equal(
    nrow(cg_select_visit(cg,
      visit = "earliest",
      visit_col = "serum_Ab_S"
    )),
    nlevels(factor(cg$elig_study_id))
  )
})


test_that("cg_select_visit(visit='latest') outputs fewer rows", {
  expect_equal(
    colnames(
      cg_select_visit(cg,
        visit = "latest",
        visit_col = "serum_Ab_S"
      )
    ),
    colnames(cg)
  )

  expect_equal(
    nrow(cg_select_visit(cg,
      visit = "latest",
      visit_col = "serum_Ab_S"
    )),
    nlevels(factor(cg$elig_study_id))
  )
})




test_that("cg_select_visit(visit='latest') errors if
          specified visit_col  not present", {
  data(built_smallstudy)

  # expect_error(
  #   cg_select_visit(built_smallstudy$chronogram,
  #               ids_column_name = "Typo",
  #               visit = "latest", visit_col = "serum_Ab_S")
  #
  # )

  expect_error(
    cg_select_visit(
      built_smallstudy$chronogram,
      visit = "latest", visit_col = "Typo"
    )
  )
})



## "earliest"

test_that("cg_select_visit(visit='earliest') errors if
          specified visit_col  not present", {
  data(built_smallstudy)

  # expect_error(
  #   cg_select_visit(built_smallstudy$chronogram,
  #               ids_column_name = "Typo",
  #               visit = "latest", visit_col = "serum_Ab_S")
  #
  # )

  expect_error(
    cg_select_visit(
      built_smallstudy$chronogram,
      visit = "earliest", visit_col = "Typo"
    )
  )
})
