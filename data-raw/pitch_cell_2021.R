## code to prepare PITCH dataset goes here

## NB: this is pseudo-data.
## The original has dates in MM-YYYY.
## For the purposes of a working chronogram example, here we:
## - set all dose 1 to a random pick of 1-28 for DD for DD-MM-YYYY
## - use the day intervals available to calculate dates
##   of dose 2 and experiment results

## This is not an exact reproduction of the source data used in Payne et al.
## (nor is it an attempt to exactly reverse engineer full dates).

## No discovery analyses should be performed using this data.

#--------------------------------------------------------------------#
## Background ####
#--------------------------------------------------------------------#

# Data available here:
# https://data.mendeley.com/datasets/fyp26zjgmj/1
# Published: 3 November 2021
#  Version 1
#  DOI: 10.17632/fyp26zjgmj.1
# Contributors:  Rebecca Payne, Susan Hopkins, Victoria Hall,
# Christina Dold, Christopher Duncan, Alex Richter, Miles Carroll,
# Gavin Screaton, Thushan de Silva, Lance Turtle, Paul Klenerman,
# Susanna Dunachie, PITCH Consortium authors

#--------------------------------------------------------------------#
## Libraries ####
#--------------------------------------------------------------------#
library(dplyr)
library(stringr)
library(dbplyr)
library(RSQLite)
library(DBI)

#--------------------------------------------------------------------#
## Download zip, if not already present ####
#--------------------------------------------------------------------#
fls_web <-
  "prod-dcd-datasets-cache-zipfiles.s3.eu-west-1.amazonaws.com"
fls_name <- "fyp26zjgmj-1.zip"

fls <- paste0("https://", fls_web, "/", fls_name)

destfile <- "PayneEtAl.zip"
dir <- stringr::str_remove(fls, ".*/") %>%
  stringr::str_remove(., "\\.zip")


if (!any(grepl(list.files(), pattern = destfile))) {
  download.file(fls, destfile = destfile)
}

#--------------------------------------------------------------------#
## Unzip ####
#--------------------------------------------------------------------#
system(
  # -o flag overwrites without a prompt #
  paste("unzip -o", destfile)
)

#--------------------------------------------------------------------#
## Assemble chronogram ####
#--------------------------------------------------------------------#
devtools::load_all()
#--------------------------------------------------------------------#
## 1. metadata ####
#--------------------------------------------------------------------#

metadata <- read.csv(file = paste0(dir, "/Metadata/", "Metadata.csv"))

## the public dataset has MM/YYYY dates #
## it does provide the interval between doses 1 and 2 in days ##
## for example, we do not need real dates, but do need DD/MM/YYYY ##

metadata <- metadata %>%
  ## set a random start day for each person ##
  mutate(
    dose_1_day =
      sample(1:28, size = nrow(.), replace = TRUE)
  ) %>%
  filter(Vaccine_dose_1_date != "NULL") %>%
  mutate(
    dose_1_date =
      lubridate::dmy(paste0(dose_1_day, "/", Vaccine_dose_1_date))
  ) %>%
  mutate(
    dose_2_date =
      dose_1_date + as.numeric(Vaccine_interval_days)
  ) %>%
  select(
    ID,
    Centre_code,
    Previous_infection,
    Vaccine_interval_regminen,
    dose_1_date,
    dose_2_date
  )

metadata <- as_tibble(metadata)

metadata <- metadata %>%
  filter(!is.na(Previous_infection)) %>%
  filter(!is.na(Vaccine_interval_regminen)) %>%
  rename(Vaccine_interval = Vaccine_interval_regminen)

#--------------------------------------------------------------------#
## 2. chronogram_skeleton ####
#--------------------------------------------------------------------#
## STUDY IDs ##
## we wish to use the cleaned-up metadata to build study IDs ##
## the source metadata includes IDs without dates of doses   ##
## - these are not useful to demo chronogram features        ##

studyId <- metadata$ID

pitch_chronogram_skeleton <- chronogram_skeleton(
  ids = studyId,
  start_date = "01012020",
  ## use published date as end date ##
  end_date = "03112021",
  col_ids = ID,
  col_calendar_date = calendar_date
)


pitch_chronogram <- chronogram(pitch_chronogram_skeleton, metadata)

#--------------------------------------------------------------------#
## 3. experimental data: nAb ####
#--------------------------------------------------------------------#
## the public experimental data is days after dose 1 or dose 2 ##
## as we are using invented (but plausible dates), needs adjusting ##

nAb_data <- read.csv(paste0(
  dir,
  "/Assay_data/Neutralization_assay/",
  "Neutralization_assay.csv"
))

nAb_data <- as_tibble(nAb_data)

## remove annotations re prior publications (these start ("data..."))
nAb_data <- nAb_data %>% select(!contains("data"))

nAb_data <- nAb_data %>%
  mutate(reference_dose = stringr::str_sub(visit_reason, 2, 2))

nAb_data <- nAb_data %>%
  left_join(., metadata %>% select(ID, dose_1_date, dose_2_date))

## all are "plus"
nAb_data <- nAb_data %>%
  mutate(visit_date = case_when(
    reference_dose == 1 ~ dose_1_date + exact_day,
    reference_dose == 2 ~ dose_2_date + exact_day
  ))

nAb_data <- nAb_data %>%
  select(ID, visit_date, Victoria, Beta, Gamma, Delta) %>%
  rename(calendar_date = visit_date)


pitch_chronogram <- cg_add_experiment(
  pitch_chronogram,
  nAb_data
)
#--------------------------------------------------------------------#
## 4. experimental data: MSD ####
#--------------------------------------------------------------------#
## the public experimental data is days after dose 1 or dose 2 ##
## as we are using invented (but plausible dates), needs adjusting ##

MSD_data <- read.csv(paste0(
  dir,
  "/Assay_data/MesoScaleDiscovery_assay/",
  "MesoScaleDiscovery_assay.csv"
))

MSD_data <- as_tibble(MSD_data)

MSD_data <- MSD_data %>% rename(exact_day = exact_days)
## rename so in line with nAb source data

## remove annotations re prior publications (these start ("data..."))
MSD_data <- MSD_data %>% select(!contains("data"))

## Assign a calendar date based on 'exact_days' & 'visit_reason_MSD' #

MSD_data <- MSD_data %>%
  left_join(., metadata %>% select(ID, dose_1_date, dose_2_date))

MSD_data <- MSD_data %>%
  mutate(visit_date = case_when(
    str_detect(Visit_reason_MSD, "V1plus") ~ dose_1_date + exact_day,
    str_detect(Visit_reason_MSD, "V2plus") ~ dose_2_date + exact_day,
    ## note for baseline it's exact_days BEFORE dose 1 ##
    str_detect(Visit_reason_MSD, "baseline") ~ dose_1_date - exact_day,
    str_detect(Visit_reason_MSD, "preV2") ~ dose_2_date - exact_day
  ))

MSD_data <- MSD_data %>%
  select(-c(
    "Visit_reason_MSD",
    Sample_date,
    Sample_id,
    exact_day,
    dose_1_date,
    dose_2_date
  )) %>%
  rename(calendar_date = visit_date) %>%
  filter(!is.na(calendar_date))

## Deal with repeated assays ##
## (there are only 5, so we will keep 1 arbitrarily) ##
MSD_data <- MSD_data %>%
  group_by(ID, calendar_date) %>%
  slice_head() %>%
  ungroup()


pitch_chronogram <- cg_add_experiment(
  pitch_chronogram,
  MSD_data
)

usethis::use_data(pitch_chronogram, compress = "xz", overwrite = T)

#--------------------------------------------------------------------#
## Make an SQL version ####
#--------------------------------------------------------------------#
dir.create("inst/extdata/", recursive = T)
my_db_file <- "inst/extdata/pitch-database-output.sqlite"
pitch_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
library(dbplyr)
copy_to(pitch_db, metadata, temporary = F, overwrite = T)
copy_to(pitch_db, nAb_data, temporary = F, overwrite = T)
copy_to(pitch_db, MSD_data, temporary = F, overwrite = T)
pitch_db
dbplyr::src_dbi(pitch_db)
DBI::dbDisconnect(pitch_db)

pitch_db_access <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
dbplyr::src_dbi(pitch_db_access)
DBI::dbDisconnect(pitch_db_access)
