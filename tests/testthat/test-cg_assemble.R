data("smallstudy")
small_study_Ab <-
  tibble::tribble(
    ~elig_study_id, ~calendar_date, ~serum_Ab_S, ~serum_Ab_N,
    1, "05/01/2021", 500, 100,
    1, "15/01/2021", 4000, 100,
    1, "03/02/2021", 3750, 100,
    1, "15/02/2021", 10000, 100,
    2, "05/01/2021", 0, 0,
    2, "15/01/2021", 4000 / 2, 0,
    2, "03/02/2021", 3750 / 2, 0,
    2, "15/02/2021", 10000 / 2, 0,
    3, "05/01/2021", 0, 0,
    3, "25/01/2021", 4000 / 2, 0,
    3, "03/02/2021", 3750 / 2, 0,
    3, "20/03/2021", 10000 / 2, 0
  )

small_study_Ab <- small_study_Ab %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::contains("date"), ~ lubridate::dmy(.x)
    )
  )

## Or two items of experiment data ##
infections_to_add <- tibble::tribble(
  ~calendar_date, ~elig_study_id, ~LFT, ~PCR, ~symptoms,
  "01102020", "1", "pos", NA, NA,
  "11102020", "1", "pos", NA, "severe"
)
## Make calendar_date a date ##
infections_to_add$calendar_date <- lubridate::dmy(
  infections_to_add$calendar_date
)

## tests #####

test_that("cg_assemble fails with invalid start date", {
  expect_error(
    ## suppress the lubridate warning (as expect_error not _warning)
    suppressWarnings(
      cg_assemble(
        ## start date impossible ##
        "50012020",
        "10012020",
        smallstudy$small_study_metadata,
        elig_study_id,
        calendar_date_col = calendar_date
      )
      ## note closing suppressWarnings ... error msg matching
    ),
    "Start date not parsed to date by lubridate::dmy"
  )
})



test_that("cg_assemble fails with invalid end date", {
  expect_error(
    ## suppress the lubridate warning (as expect_error not _warning)
    suppressWarnings(
      cg_assemble("01012020",
        ## end date impossible ##
        "50012020",
        smallstudy$small_study_metadata,
        elig_study_id,
        calendar_date_col = calendar_date
      )
    ),
    "End date not parsed to date by lubridate::dmy"
  )
})


test_that("cg_assemble fails with start date after end date", {
  expect_error(
    ## suppress the lubridate warning (as expect_error not _warning)
    suppressWarnings(
      cg_assemble(
        start_date = "10012020",
        end_date = "01012020",
        smallstudy$small_study_metadata,
        elig_study_id,
        calendar_date_col = calendar_date
      ),
      "Start date is later than end date"
    )
  )
})

test_that("cg_assemble fails if metadata ids col does not exist", {
  expect_error(
    cg_assemble("01012020", "10012020",
      smallstudy$small_study_metadata,
      ## trailing 2 ##
      elig_study_id2,
      calendar_date_col = calendar_date
    ),
    "ID column not in metadata"
  )
})

test_that(
  "cg_assemble fails if experiment data not a list",
  {
    expect_error(
      cg_assemble("01012020", "10102021",
        smallstudy$small_study_metadata,
        elig_study_id,
        ## proposed date column is in metadata
        calendar_date_col = calendar_date,
        experiment_data_list =
          infections_to_add
      ),
      "Experiment data not provided as a list"
    )
  }
)

test_that(
  "cg_assemble fails if experiment data parts are not tbl",
  {
    expect_error(
      cg_assemble("01012020", "10102021",
        smallstudy$small_study_metadata,
        elig_study_id,
        ## proposed date column is in metadata
        calendar_date_col = calendar_date,
        experiment_data_list =
          list(as.data.frame(infections_to_add))
      ),
      "Experiment data list slots does not a tibble
        Consider adding one-by-one with cg_add_experiment()"
    )
  }
)



test_that(
  "cg_assemble fails if calendar_date column already in metadata",
  {
    expect_error(
      cg_assemble("01012020", "10012020",
        smallstudy$small_study_metadata,
        elig_study_id,
        ## proposed date column is in metadata
        calendar_date_col = age
      ),
      "calendar date column also in metadata. Consult assembly vignette."
    )
  }
)

test_that(
  "cg_assemble fails if IDs column not in experiment data",
  {
    expect_error(
      cg_assemble("01012020", "10102021",
        smallstudy$small_study_metadata,
        elig_study_id,
        ## proposed date column is in metadata
        calendar_date_col = calendar_date,
        experiment_data_list =
          list(infections_to_add %>%
            dplyr::select(!elig_study_id))
      ),
      "IDs column not present"
    )
  }
)

test_that(
  "cg_assemble fails if calendar date column not in experiment data",
  {
    expect_error(
      cg_assemble("01012020", "10102021",
        smallstudy$small_study_metadata,
        elig_study_id,
        ## proposed date column is in metadata
        calendar_date_col = calendar_date,
        experiment_data_list =
          list(infections_to_add %>%
            dplyr::select(!calendar_date))
      ),
      "calendar date column not present"
    )
  }
)


test_that(
  "cg_assemble messages if experiment data",
  {
    expect_message(
      cg_assemble("01012020", "10102021",
        smallstudy$small_study_metadata,
        elig_study_id,
        ## proposed date column is in metadata
        calendar_date_col = calendar_date
      ),
      "--no experiment data provided. Add later: cg_add_experiment()"
    )
  }
)


test_that(
  "cg_assemble warns if columns in an experiment already in cg",
  {
    expect_warning(
      cg <- cg_assemble("01012020", "10102021",
        smallstudy$small_study_metadata,
        elig_study_id,
        ## proposed date column is in metadata
        calendar_date_col = calendar_date,
        experiment_data_list =
          list(infections_to_add, infections_to_add)
      )
    )
  }
)
