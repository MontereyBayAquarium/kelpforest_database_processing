
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
  mutate(across(everything(), as.character)) %>%       # Convert all columns to character
  mutate(across(everything(), ~ na_if(., "NULL"))) %>% # Replace "NULL" with NA
  type_convert()   %>%
  #set column types
  mutate(site_long = factor(site_long),
         survey_type = factor(survey_type),
         region = factor(region),
         site_name_2024 = factor(site_name_2024),
         site_type_2024 = factor(site_type_2024),
         site_short_2024 = factor(site_short_2024),
         site_long_2024 = factor(site_long_2024),
         site_name_2025 = factor(site_name_2025),
         site_type_2025 = factor(site_type_2025),
         transect = factor(transect),
         old_latitude = as.numeric(old_latitude),
         old_longitude = as.numeric(old_longitude),
         new_latitude = as.numeric(new_latitude),
         new_longitude = as.numeric(new_longitude),
         reprojected_coords = factor(reprojected_coords),
         target_depth_meters = as.numeric(target_depth_meters),
         uc_heading = as.numeric(uc_heading),
         dc_heading = as.numeric(dc_heading),
         original_date_surveyed = as.Date(original_date_surveyed, format = "%Y-%m-%d"),
         resite_date = as.Date(resite_date, format = "%Y-%m-%d"),
         in_stack = factor(in_stack),
         notes = as.character(notes)
  ) %>%
  # Replace date_surveyed with resite_date if resite_date is not NA
  mutate(original_survey_date_official = if_else(is.na(resite_date),original_date_surveyed,resite_date))%>%
  # Apply standard site naming
  mutate(
    # Use a function within str_replace to process each match
    site_name_2025 = str_replace(site_name_2025, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    }),
    site_name_2024 = str_replace(site_name_2024, "([A-Za-z]+)([0-9]+)", function(x) {
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
  #drop columns and rename
select(survey_type, region, site = site_name_2025, site_type = site_type_2025,
       site_old = site_name_2024, site_type_old = site_type_2024, zone = transect,
           latitude = new_latitude, longitude = new_longitude, latitude_old = old_latitude,
       longitude_old = old_longitude, survey_date_2024 = original_survey_date_official,
           notes
         ) 
       

################################################################################
#Step 3: export

write_csv(marge_build1, file.path(datout, "margin_site_table.csv")) #last write 27 Mar 2025

write_csv(reco_build1, file.path(datout, "recovery_site_table.csv")) #last write 27 Mar 2025


