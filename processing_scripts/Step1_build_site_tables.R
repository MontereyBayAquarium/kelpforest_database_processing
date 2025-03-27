
#jossmith@mbayaq.org

#This script is for processing the site waypoints spreadsheet. 
#the output is a cleaned metadata file

#Steps involved:



################################################################################
#

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate)
gs4_auth()

#Set paths
datout <- "/Volumes/seaotterdb$/kelp_recovery/data/MBA_kelp_forest_database/processed"

#read margin lookup table
margin_orig <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1EnLOhGma-IBsMr4nLl159BfLarCeHO25k2yhLnDsTow/edit?gid=634855602#gid=634855602",
  sheet = 5) %>% 
  clean_names()

#read recovery sites
recovery_orig <- read_sheet("https://docs.google.com/spreadsheets/d/1EnLOhGma-IBsMr4nLl159BfLarCeHO25k2yhLnDsTow/edit?gid=1807281007#gid=1807281007",
                            sheet = 3) %>% 
  clean_names()

################################################################################
#Step 1: process margin data

marge_build1 <- margin_orig %>%
  #set column types
  mutate(survey_type = factor(survey_type),
         region = factor(region),
         site_name_2024 = factor(site_name_2024),
         site_official = factor(site_name_2025),
         transect = as.numeric(transect),
         heading_out = as.numeric(heading_out),
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
         #drop sites that were never surveyed or were bad margins
         filter(!(is.na(site_official))) %>% #sites where in_stack == yes means they were in the raw data
         select(survey_type, region, site_official, site_name_2024, transect,
                heading_out, target_latitude, target_longitude, actual_latitude,
                actual_longitude, date_surveyed_originally=date_surveyed, resite_needed, resite_date,
                notes)%>%
          # Apply standard site naming
          mutate(
            # Use a function within str_replace to process each match
            site_official = str_replace(site_official, "([A-Za-z]+)([0-9]+)", function(x) {
              # Extract letters and numbers
              parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
              letters <- toupper(parts[, 2])   # Convert to uppercase if needed
              numbers <- parts[, 3]
              # Pad numbers with leading zero
              numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
              # Combine parts with underscore
              paste0(letters, "_", numbers_padded)
            })
          ) %>%
         mutate(
           # Add date_surveyed to resite_date if NA
           survey_date = if_else(is.na(resite_date), date_surveyed_originally, resite_date),
                #fix lat/longs
                actual_latitude = coalesce(actual_latitude, target_latitude),
                actual_longitude = coalesce(actual_longitude, target_longitude)) %>%
         #drop columns and rename
         select(-target_latitude, -target_longitude) %>%
         select(survey_type, region, site_official, site_name_2024, transect, heading_out,
                latitude = actual_latitude, longitude = actual_longitude, 
                survey_date,date_surveyed_originally)
         

################################################################################
#Step 2: process recovery data

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
  rename(site = site_name)%>%
  # Apply standard site naming
  mutate(
    # Use a function within str_replace to process each match
    site = str_replace(site, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    })
  ) %>%
  rename(original_survey_date = original_date_surveyed)%>%
  mutate(resite_conducted = ifelse(is.na(resite_date), "no", "yes"),
         official_survey_date = coalesce(resite_date, original_survey_date),
         #fix lat/longs
         actual_latitude = coalesce(actual_latitude, target_latitude),
         actual_longitude = coalesce(actual_longitude, target_longitude)
  ) %>%
  #drop columns and rename
  select(-target_latitude, -target_longitude, 
         -in_stack) %>% select(
           survey_type, region, site, site_type, site_long = site_long_7,
           zone = transect, latitude = actual_latitude, longitude = actual_longitude,
           official_survey_date, original_survey_date, resite_date, resite_conducted,
           notes
         ) %>%
  #set data types
  mutate(site = as.factor(site))
       

################################################################################
#Step 3: export

write_csv(marge_build1, file.path(datout, "margin_site_table.csv")) #last write 5 Nov 2024

write_csv(reco_build1, file.path(datout, "recovery_site_table.csv")) #last write 29 Oct 2024


