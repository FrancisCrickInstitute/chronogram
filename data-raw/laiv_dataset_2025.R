## This script utilises data from the Comprehensive Multimodal Immune Response 
## Dataset for LAIV Vaccination in Pediatric Cohorts
## This is not data produced at the Francis Crick Institute and is only used 
## here to demonstrate the implementation of the Chronogram package on LAIV data

#--------------------------------------------------------------------#
## Background ####
#--------------------------------------------------------------------#

# Data available here:
# https://zenodo.org/records/14719593
# Published: 22 January 2025
# Version 1
# DOI:10.5281/zenodo.14719592.
# Contributors:  Tomic, Adriana (Project leader)
# Tomic, Ivan (Data curator)
# de Silva, Thushan (Data collector)

# Pre-print available here:
# https://pubmed.ncbi.nlm.nih.gov/39896552/
# Accessed 21/07/25


#--------------------------------------------------------------------#
## Libraries and Paths ####
#--------------------------------------------------------------------#

library(tidyverse)
library(dplyr)
library(lubridate)
library(chronogram)

input_data_path = "inst/extdata/LAIV_Immune_Response_Integrated_Dataset.csv"
output_data_dir = "data/"

#--------------------------------------------------------------------#
## Load the laiv dataset from csv
#--------------------------------------------------------------------#
laiv_df = read.csv(input_data_path)

#--------------------------------------------------------------------#
##  Isolate metadata and static variables
#--------------------------------------------------------------------#
meta_data_cols = c("subject_ID", "sex", "year", "z_score_continuous", "v0_resp_virus_positive", "cluster")
meta_df = laiv_df %>% 
  select(all_of(meta_data_cols)) %>%
  mutate(vaccine_date = dmy(paste0("01/11/", year)))
meta_df <- as_tibble(meta_df)

# drop any static columns from primary data.frame and create as seperate for optional saving 
static_laiv_df <- laiv_df %>%
  select(matches("FC|responder|subject_ID|year"))

laiv_df <- laiv_df %>%
  select(-matches("FC|responder"))

#--------------------------------------------------------------------#
##  Subset assay data 
#--------------------------------------------------------------------#
assay_dict = list(
  hai = "hai",
  IgA = "IgA|H1N1|N2",
  cd8 = "cd8",
  IVPM = "IVPM|NC99|MICH15|Cal09|GD|SWISS|HK14|KAN|B_PHU",
  cH6_ch7 = "cH6|ch7",
  shed = "shed",
  seropositive = "seropositive",
  ul = "ul",
  TFH = "TFH",
  mdc_pdc_monocyte = "mdc|pdc|monocyte"
)

# create a data.frame for each key, pattern pair 
create_subsets <- function(df, dict) {
  map2(dict, names(dict), function(pattern, key) {
    if (key == "IgA") {
      df %>%
        select(subject_ID, year, matches(pattern, ignore.case = TRUE) & 
                 !matches("cH6|ch7", ignore.case = TRUE))
    } else {
      df %>%
        select(subject_ID, year, matches(pattern, ignore.case = TRUE))
    }
  })
}

subsets <- create_subsets(laiv_df, assay_dict)
names(subsets) <- names(assay_dict)

#--------------------------------------------------------------------#
##  Check if any columns are missing from the sub-setting
#--------------------------------------------------------------------#

# Get all unique columns included in any subset
subset_cols <- subsets %>% 
  map(colnames) %>% 
  unlist() %>% 
  unique()

# Columns in laiv_df not in any subset
missing_cols <- setdiff(colnames(laiv_df), subset_cols)

# If any missing columns, give a warning listing them
if (length(missing_cols) > 0) {
  warning("Double check columns missing from assay subsets:", paste(missing_cols, collapse = ", "))
}

#--------------------------------------------------------------------#
##   Function for processing the assay data 
#--------------------------------------------------------------------#
process_subset <- function(df) {
  
  timepoint_pattern = "v0|v2|v7|v21"
  timepoint_cols = colnames(df)[
    str_detect(colnames(df), regex(timepoint_pattern, ignore_case = TRUE))
  ]
  
  # Constant columns are all other columns except subject_ID and year
  constant_cols <- setdiff(colnames(df), c("subject_ID", "year", timepoint_cols))
  
  # Check if timepoint columns exist and have at least one data column (exclude subject_ID, year)
  if (length(timepoint_cols) > 0) {
    long_timepoint <- df %>%
      select(subject_ID, year, all_of(timepoint_cols)) %>%
      pivot_longer(
        cols = -c(subject_ID, year),
        names_to = "assay",
        values_to = "value"
      ) %>%
      mutate(
        rel_day = case_when(
          str_detect(assay, regex("v0", ignore_case = TRUE)) ~ 0L,
          str_detect(assay, regex("v2", ignore_case = TRUE)) ~ 2L,
          str_detect(assay, regex("v7", ignore_case = TRUE)) ~ 7L,
          str_detect(assay, regex("v21", ignore_case = TRUE)) ~ 21L,
          TRUE ~ NA_integer_
        )
      )
  } else {
    long_timepoint <- tibble(subject_ID = character(), year = integer(), assay = character(), rel_day = integer(), value = numeric())
  }
  
  # Same for constant columns
  if (length(constant_cols) > 0) {
    long_constant <- df %>%
      select(subject_ID, year, all_of(constant_cols)) %>%
      pivot_longer(
        cols = -c(subject_ID, year),
        names_to = "assay",
        values_to = "value"
      ) %>%
      crossing(rel_day = c(0, 2, 7, 21))
  } else {
    long_constant <- tibble(subject_ID = character(), year = integer(), assay = character(), rel_day = integer(), value = numeric())
  }
  
  # Combine or return empty tibble if both are empty
  if (nrow(long_timepoint) == 0 && nrow(long_constant) == 0) {
    # No data columns to process, create empty tibble with all combinations and NA values
    combined <- crossing(
      subject_ID = unique(df$subject_ID),
      assay = character(0),
      year = unique(df$year),
      rel_day = c(0, 2, 7, 21)
    ) %>%
      mutate(value = NA_real_,
             date = case_when(
               rel_day == 0  ~ paste0("01/11/", year),
               rel_day == 2  ~ paste0("03/11/", year),
               rel_day == 7  ~ paste0("08/11/", year),
               rel_day == 21 ~ paste0("22/11/", year),
               TRUE ~ NA_character_
             )) %>%
      mutate(date = lubridate::dmy(date)) %>%
      select(subject_ID, assay, year, rel_day, date, value)
    
    return(combined)
  }
  
  # Otherwise combine and fill missing
  combined = bind_rows(long_timepoint, long_constant) %>%
    right_join(
      crossing(
        subject_ID = unique(df$subject_ID),
        assay = unique(c(long_timepoint$assay, long_constant$assay)),
        year = unique(df$year),
        rel_day = c(0, 2, 7, 21)
      ),
      by = c("subject_ID", "assay", "year", "rel_day")
    ) %>%
    mutate(
      assay = assay %>% 
        # Strip the timepoint info from assay names
        # 1. Remove timepoint at start or end (with optional underscore)
        str_remove_all(regex("^v(0|2|7|21)_?|_?v(0|2|7|21)$", ignore_case = TRUE)) %>%
        
        # 2. Replace _vX_ (middle of string) with _
        str_replace_all(regex("_v(0|2|7|21)_", ignore_case = TRUE), "_")
    ) %>% 
    mutate(
      # Create the date column based on rel_day and year 
      date = case_when(
        rel_day == 0  ~ paste0("01/11/", year),
        rel_day == 2  ~ paste0("03/11/", year),
        rel_day == 7  ~ paste0("08/11/", year),
        rel_day == 21 ~ paste0("22/11/", year),
        TRUE ~ NA_character_
      ),
      date = lubridate::dmy(date)
    ) %>%
    select(subject_ID, assay, year, rel_day, date, value)
  
  return(combined)
  
}

#--------------------------------------------------------------------#
##  Function for pivoting the assay data
#--------------------------------------------------------------------#
pivot_subset = function(df){
  combined = df %>% 
    filter(!is.na(value)) %>% 
    pivot_wider(id_cols = c(subject_ID,date),
                names_from = assay,
                values_from = value)
  
  return(combined)
}

#--------------------------------------------------------------------#
##  Apply the preprocess_subset() and pivot_subset() functions
#--------------------------------------------------------------------#
long_subsets = map(subsets, process_subset)
names(long_subsets) = names(subsets)
pivoted_subsets = map(long_subsets, pivot_subset)


#--------------------------------------------------------------------#
##  Implement cg_assemble() 
#--------------------------------------------------------------------#
cg <- cg_assemble(
  start_date = "01112017",
  end_date = "01122018",
  ## the provided metadata ##
  metadata = meta_df,
  ## the column name in the metadata that contains participant IDs ##
  metadata_ids_col = subject_ID,
  ## column name for dates ##
  calendar_date_col = date,
  ## the provided experiment data (we have 1 assay, so a list of 1) ##
  experiment_data_list = pivoted_subsets
)

message("Success! Chronogram object has been built from the LAIV dataset.")

#--------------------------------------------------------------------#
##   Save the chronogram object 
#--------------------------------------------------------------------#
save(cg, file = file.path(output_data_dir, paste0("laiv_dataset_chronogram.rda")))