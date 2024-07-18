

################################################################################
# About
# 


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate)
gs4_auth()

#set paths
figdir <- here::here("dissection_data","figures")

#read urchin data
urch_dat_orig <- read_sheet("https://docs.google.com/spreadsheets/d/1Ih-hBXRtfXVMdxw5ibZnXy_dZErdcx5FfeKMSc0HEc4/edit?gid=0#gid=0") %>%
  clean_names()

################################################################################

#inspect
View(urch_dat_orig)

# Convert sample_number from list to character vector

# Perform further transformations
urch_dat <- urch_dat_orig %>%
 # mutate(
  #  sample_number = ifelse(sample_number == "NULL", NA, sample_number),  # Replace "NULL" with NA
  #  sample_number = str_remove(sample_number, "^0+"),  # Remove leading zeros
  #  sample_number = as.numeric(sample_number)  # Convert to numeric
  #) %>%
  mutate(
    institution = as.factor(institution),
    name_of_data_enterer = as.character(name_of_data_enterer),
    date_collected = ymd(date_collected),
    date_fixed = ymd(date_fixed),
    date_processed = ymd(date_processed),
    site_number = as.factor(site_number),
    transect = as.factor(transect), 
    treatment = as.factor(treatment),
    sex = as.factor(sex),
    test_height_mm = as.numeric(test_height_mm),
    test_diameter_mm = as.numeric(test_diameter_mm),
    animal_wet_mass_g = as.numeric(animal_wet_mass_g),
    animal_24hr_mass_g = as.numeric(animal_24hr_mass_g),
    gonad_mass_g = as.numeric(gonad_mass_g),
    soft_tissue_mass_g = as.numeric(soft_tissue_mass_g),
    notes = as.character(notes),
    date_entered = ymd_hms(date_entered)
  )

# View the structure of the transformed dataframe
str(urch_dat)

