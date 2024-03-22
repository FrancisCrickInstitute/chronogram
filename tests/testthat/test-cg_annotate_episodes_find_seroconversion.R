data("built_smallstudy")

cg <- built_smallstudy$chronogram

test_that(
  "chronogram returned by cg_annotate_episodes_find_seroconversion",
  {
    a <- cg_annotate_episodes_find_seroconversion(cg,
      serum_N_titre =
        "serum_Ab_N"
    )
    expect_s3_class(a, class = "cg_tbl")
  }
)


test_that(
  "cg_annotate_episodes_find_seroconversion fails if output cols
  already exist",
  {
    a <- cg_annotate_episodes_find_seroconversion(
      cg,
      serum_N_titre = "serum_Ab_N"
    )

    expect_error(
      cg_annotate_episodes_find_seroconversion(
        a,
        serum_N_titre = "serum_Ab_N"
      )
    )
  }
)


test_that(
  "cg_annotate_episodes_find_seroconversion returns same nrow",
  {
    a <- cg_annotate_episodes_find_seroconversion(
      cg,
      serum_N_titre = "serum_Ab_N"
    )

    expect_equal(nrow(a), nrow(cg))
  }
)

test_that(
  "cg_annotate_episodes_find_seroconversion returns extra cols",
  {
    a <- cg_annotate_episodes_find_seroconversion(
      cg,
      serum_N_titre = "serum_Ab_N"
    )

    expect_true(
      all(colnames(cg) %in% colnames(a))
    )

    expect_true(
      length(colnames(cg)) <
        length(colnames(a))
    )
  }
)
