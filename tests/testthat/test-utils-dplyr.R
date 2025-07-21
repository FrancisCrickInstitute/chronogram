## Note:
## dplyr_row_slice()
## dplyr_col_modify()
## dplyr_reconstruct()
## for cg_tbl and grouped_cg_df classes
## are tested extensively elsewhere
## (by nearly all annotate functions)


data("built_smallstudy")
cg <- built_smallstudy$chronogram
to_group <- attributes(cg)$col_ids
to_second_group <- attributes(cg)$col_calendar_date
cg <- cg %>% group_by(.data[[to_group]])
cg

rows_to_expect <- nlevels(factor(cg[[to_group]]))

test_that("grouped chronogram can be subsetted", {
  expect_equal(
    (cg[1, ] %>% nrow()), 1
  )

  expect_equal(
    (cg[1, ] %>% dplyr::groups()) %>%
      .[[1]] %>% as.character(.),
    to_group
  )
})


test_that("grouped chronogram works with slice_head()", {
  expect_equal(
    (cg %>% dplyr::slice_head() %>% nrow()),
    rows_to_expect
  ) &
    expect_equal(
      (cg %>% dplyr::slice_head() %>%
        dplyr::groups() %>% .[[1]] %>%
        as.character(.) %>% length()),
      length(to_group)
    )
})


test_that("grouped chronogram works with slice_tail()", {
  expect_equal(
    (cg %>% dplyr::slice_tail() %>% nrow()),
    rows_to_expect
  ) &
    expect_equal(
      (cg %>% dplyr::slice_tail() %>%
        dplyr::groups() %>% .[[1]] %>%
        as.character(.) %>% length()),
      length(to_group)
    )
})



test_that("grouped chronogram can preserve groups with second group_by()", {
  expect_equal(
    cg %>%
      group_by(.data[[to_second_group]], .add = TRUE) %>%
      dplyr::groups() %>% length(),
    2
  )
})

test_that("grouped chronogram can preserve groups with second group_by()", {
  expect_equal(
    cg %>%
      group_by(.data[[to_second_group]], .add = TRUE) %>%
      dplyr::groups() %>% list2DF(.) %>% as.character(),
    c(to_group, to_second_group)
  )
})
