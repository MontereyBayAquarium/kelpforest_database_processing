
#jossmith@mbayaq.org

#This script is for processing the site waypoints spreadsheet. 
#the output is a cleaned metadata file

#Steps involved:



################################################################################
#

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate)
gs4_auth()


#read margin
margin_orig <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1EnLOhGma-IBsMr4nLl159BfLarCeHO25k2yhLnDsTow/edit?gid=0#gid=0",
  range = "A5:Z") %>% # skip rows that are not needed
  clean_names()

#read margin
recovery_orig <- read_sheet("https://docs.google.com/spreadsheets/d/1EnLOhGma-IBsMr4nLl159BfLarCeHO25k2yhLnDsTow/edit?gid=931904684#gid=931904684",
                            sheet = 2, range = "A5:Z") %>% # skip rows that are not needed
  clean_names()

################################################################################
#Step 1: process margin data

marge_build1 <- margin_orig %>%
  #set column types
  mutate(survey_type = factor(survey_type),
         region = factor(region),
         site_name = factor(site_name),
         transect = as.numeric(transect),
         target_latitude = as.numeric(target_latitude),
         target_longitude = as.numeric(target_longitude),
         actual_latitude = as.numeric(actual_latitude),
         actual_longitude = as.numeric(actual_longitude),
         date_surveyed = as.Date(date_surveyed, format = "%Y-%m-%d"),
         resite_needed = factor(resite_needed),
         resite_date = as.Date(resite_date, format = "%Y-%m-%d"),
         in_stack = factor(in_stack),
         notes = as.character(notes)
         ) %>%
         #drop sites that were never surveyed
         filter((in_stack =="yes")) %>% #sites where in_stack == yes means they were in the raw data
         # Replace date_surveyed with resite_date if resite_date is not NA
         mutate(date_surveyed = coalesce(resite_date, date_surveyed),
                #fix lat/longs
                actual_latitude = coalesce(actual_latitude, target_latitude),
                actual_longitude = coalesce(actual_longitude, target_longitude),
                # Remove underscores from site_name
                site_name = str_replace_all(site_name, "_", "")
                ) %>%
         #drop columns and rename
         select(-target_latitude, -target_longitude, -resite_needed, 
                -resite_date, -in_stack) %>% select(1:8)

################################################################################
#Step 2: process margin data

reco_build1 <- recovery_orig %>%
  #drop columns
  select(-site_long_1, -site_short) %>%
  mutate(across(everything(), as.character)) %>%       # Convert all columns to character
  mutate(across(everything(), ~ na_if(., "NULL"))) %>% # Replace "NULL" with NA
  type_convert()   %>%
  #set column types
  mutate(survey_type = factor(survey_type),
         region = factor(region),
         site_name = factor(site_name),
         site_type = factor(site_type),
         site_long_7 = factor(site_long_7),
         transect = factor(transect),
         target_latitude = as.numeric(target_latitude),
         target_longitude = as.numeric(target_longitude),
         actual_latitude = as.numeric(actual_latitude),
         actual_longitude = as.numeric(actual_longitude),
         original_date_surveyed = as.Date(original_date_surveyed, format = "%Y-%m-%d"),
         resite_date = as.Date(resite_date, format = "%Y-%m-%d"),
         in_stack = factor(in_stack),
         notes = as.character(notes)
  ) %>%
  #drop sites that were never surveyed
  filter((in_stack =="yes")) %>% #sites where in_stack == yes means they were in the raw data
  # Replace date_surveyed with resite_date if resite_date is not NA
  mutate(date_surveyed = coalesce(resite_date, original_date_surveyed),
         #fix lat/longs
         actual_latitude = coalesce(actual_latitude, target_latitude),
         actual_longitude = coalesce(actual_longitude, target_longitude)
  ) %>%
  #drop columns and rename
  select(-target_latitude, -target_longitude, 
         -resite_date, -original_date_surveyed, -in_stack) %>% select(1:6, date_surveyed,
                                                                      actual_latitude,
                                                                      actual_longitude, notes) %>%
  rename(site_long = site_long_7,
         latitude = actual_latitude,
         longitude = actual_longitude)


################################################################################
#Step 3: export





