## code to prepare `smallstudy` dataset goes here

## a 3-person study ##
small_study_ids <- c(1, 2, 3)

## Create a tibble containing some metadata for our 3 individuals ##
small_study_metadata <-
  tibble::tribble(
    ~elig_study_id, ~age, ~sex, ~dose_1, ~date_dose_1, ~dose_2, ~date_dose_2,
    1, 40, "F", "AZD1222", "05/01/2021", "AZD1222", "05/02/2021",
    2, 45, "F", "BNT162b2", "05/01/2021", "BNT162b2", "05/02/2021",
    3, 35, "M", "BNT162b2", "10/01/2021", "BNT162b2", "10/03/2021"
  )

## Set appropriate metadata column classes ##
small_study_metadata <- small_study_metadata %>%
  dplyr::mutate(dplyr::across(c(sex, dose_1, dose_2), ~ as.factor(.x)))

small_study_metadata <- small_study_metadata %>%
  dplyr::mutate(dplyr::across(dplyr::contains("date"), ~ lubridate::dmy(.x)))

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
  dplyr::mutate(dplyr::across(dplyr::contains("date"), ~ lubridate::dmy(.x)))

smallstudy <- list(
  small_study_ids =
    small_study_ids,
  small_study_metadata =
    small_study_metadata,
  small_study_Ab = small_study_Ab
)

usethis::use_data(smallstudy, overwrite = TRUE)



#--------------------------------------------------------------------#
## Assembled chronogram_skeleton + chronogram #####
#--------------------------------------------------------------------#
small_study_chronogram_skeleton <- chronogram_skeleton(
  ids = small_study_ids,
  col_ids = elig_study_id,
  col_calendar_date = calendar_date,
  start_date = "01012020",
  end_date = "10102021"
)


small_study_chronogram <- chronogram(
  small_study_chronogram_skeleton,
  small_study_metadata
)

small_study_chronogram <- cg_add_experiment(
  small_study_chronogram,
  small_study_Ab
)


built_smallstudy <- list(
  chronogram_skeleton = small_study_chronogram_skeleton,
  chronogram = small_study_chronogram
)

usethis::use_data(built_smallstudy, overwrite = TRUE)
