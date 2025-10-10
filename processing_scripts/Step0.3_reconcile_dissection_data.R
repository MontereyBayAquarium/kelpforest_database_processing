

################################################################################
# About
# data processing script written by JG.Smith jogsmith@ucsc.edu


#context: data were entered twice. This script joins the two separate entries and
#identifies mismatches. The mismatched entries are then uploaded as a spreadsheet
#to Google drive for reconciliation. 

#steps involved


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive, stringr)
#gs4_auth()

#set dir
datdir <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database"

#read original data
dissection_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1Ih-hBXRtfXVMdxw5ibZnXy_dZErdcx5FfeKMSc0HEc4/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()


#read QAQC data
dissection_qc <- read_sheet("https://docs.google.com/spreadsheets/d/1JokIivixTaecVwtY7fIYhn41yvP7ZrzeZ3YJ-LXDNE0/edit?gid=0#gid=0",
                      sheet = 1) %>% clean_names()


################################################################################
#Process dissection entry

dissection_raw_build1 <- dissection_raw %>%
  mutate(
    institution = as.factor(institution),
    name_of_data_enterer = as.character(name_of_data_enterer),
    # normalize dates (handles "-" or "/")
    date_collected = parse_date_time(date_collected, orders = c("ymd", "Y/m/d", "m/d/Y")),
    date_fixed     = parse_date_time(date_fixed,     orders = c("ymd", "Y/m/d", "m/d/Y")),
    date_processed = parse_date_time(date_processed, orders = c("ymd", "Y/m/d", "m/d/Y")),
    
    site_number = as.character(site_number),
    transect = as.character(transect),
    treatment = as.factor(treatment),
    sex = tolower(as.character(sex)),   # normalize to lower case
    test_height_mm = as.numeric(test_height_mm),
    test_diameter_mm = as.numeric(test_diameter_mm),
    animal_wet_mass_g = as.numeric(animal_wet_mass_g),
    animal_24hr_mass_g = as.numeric(animal_24hr_mass_g),
    gonad_mass_g = as.numeric(gonad_mass_g),
    soft_tissue_mass_g = as.numeric(soft_tissue_mass_g),
    notes = as.character(notes),
    date_entered = ymd_hms(date_entered)
  ) %>%
  # clean site names consistently
  mutate(
    site_number = str_replace_all(site_number, "-", "_"),
    site_number = str_replace(site_number, "^REC_", "REC"),
    site_number = str_replace(site_number, "^MAR_", "MAR")
  ) %>%
  # remove leading zeros from sample_number
  mutate(sample_number = sub("^0+", "", as.character(sample_number))) %>%
  select(-institution, -name_of_data_enterer, -notes) 


dissection_qc_build1 <- dissection_qc %>%
  mutate(
    institution = as.factor(institution),
    name_of_data_enterer = as.character(name_of_data_enterer),
    # normalize dates (handles "-" or "/")
    date_collected = parse_date_time(date_collected, orders = c("ymd", "Y/m/d", "m/d/Y")),
    date_fixed     = parse_date_time(date_fixed,     orders = c("ymd", "Y/m/d", "m/d/Y")),
    date_processed = parse_date_time(date_processed, orders = c("ymd", "Y/m/d", "m/d/Y")),
    
    site_number = as.character(site_number),
    transect = as.character(transect),
    treatment = as.factor(treatment),
    sex = tolower(as.character(sex)),   # normalize to lower case
    test_height_mm = as.numeric(test_height_mm),
    test_diameter_mm = as.numeric(test_diameter_mm),
    animal_wet_mass_g = as.numeric(animal_wet_mass_g),
    animal_24hr_mass_g = as.numeric(animal_24hr_mass_g),
    gonad_mass_g = as.numeric(gonad_mass_g),
    soft_tissue_mass_g = as.numeric(soft_tissue_mass_g),
    notes = as.character(notes),
    date_entered = ymd_hms(date_entered)
  ) %>%
  # clean site names consistently
  mutate(
    site_number = str_replace_all(site_number, "-", "_"),
    site_number = str_replace(site_number, "^REC_", "REC"),
    site_number = str_replace(site_number, "^MAR_", "MAR")
  ) %>%
  # remove leading zeros from sample_number
  mutate(sample_number = sub("^0+", "", as.character(sample_number))) %>%
  select(-institution, -name_of_data_enterer, -notes)




library(dplyr)
library(stringr)
library(lubridate)

# helper: flatten any list-cols (e.g., sample_number)
flatten_lists <- function(df) {
  df %>% mutate(across(where(is.list), ~ vapply(.x, function(v) {
    if (length(v) == 0 || all(is.na(v))) NA_character_ else as.character(v[[1]])
  }, FUN.VALUE = character(1))))
}

# Prep comparable copies (factor->char, dates to Date, normalize sample_number)
raw_c <- dissection_raw_build1 %>%
  flatten_lists() %>%
  mutate(
    across(where(is.factor), as.character),
    date_processed = as.Date(date_processed),
    sample_number  = sub("^0+", "", as.character(sample_number))
  )

qc_c <- dissection_qc_build1 %>%
  flatten_lists() %>%
  mutate(
    across(where(is.factor), as.character),
    date_processed = as.Date(date_processed),
    sample_number  = sub("^0+", "", as.character(sample_number))
  )

# ---- 0) Sanity: find many-to-many keys (should be empty) ----
key4  <- c("site_number","transect","date_processed","species")
mm_raw <- raw_c %>% count(across(all_of(key4)), name="n") %>% filter(n>1)
mm_qc  <- qc_c  %>% count(across(all_of(key4)), name="n") %>% filter(n>1)
# If either has rows, you *need* sample_number in the key.

# ---- 1) Check missing sample_numbers on either side (using full key incl. sample_number) ----
key5 <- c(key4, "sample_number")

samples_missing_in_qc <- raw_c %>%
  anti_join(qc_c, by = key5) %>%
  select(all_of(key5)) %>% distinct()

samples_missing_in_raw <- qc_c %>%
  anti_join(raw_c, by = key5) %>%
  select(all_of(key5)) %>% distinct()

# ---- 2) Compare values only for matched rows (no Cartesian explosion) ----
check_cols <- c("sex","test_height_mm","test_diameter_mm","animal_wet_mass_g","gonad_mass_g")
dissection_discrep_values <- raw_c %>%
  inner_join(qc_c, by = key5, suffix = c("_raw","_qc")) %>%
  mutate(
    across(
      all_of(paste0(check_cols, "_raw")),
      ~ {
        other <- get(str_replace(cur_column(), "_raw$", "_qc"))
        if_else(!is.na(.) & !is.na(other) & . != other,
                paste0(., " ≠ ", other),
                NA_character_)
      },
      .names = "{.col}_diff"
    )
  ) %>%
  select(all_of(key5), ends_with("_diff")) %>%
  filter(if_any(ends_with("_diff"), ~ !is.na(.))) %>%
  mutate(resolved = "")


#Export
# Define file path for export
dissection_file <- "dissection_discrep_values.csv"
dissection_keys <- "dissection_keys.csv"

# Write the CSV locally
write_csv(dissection_discrep_values, dissection_file)

# Upload to the specified Google Drive folder

#2024 upload
#drive_upload(quad_file, path = as_id("1IaTpgTw6Q8-EDvSo3oONBCMDVIfLyzRB"), overwrite = TRUE) 

#2025 upload
drive_upload(dissection_file, path = as_id("1WWnvFwSsM8ZQgGCdwsXVG9abg49WM5eA"), overwrite = TRUE)





