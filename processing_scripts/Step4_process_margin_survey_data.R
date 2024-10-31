

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
gs4_auth()

#set dir
datdir <- "/Volumes/seaotterdb$/kelp_recovery/data/MBA_kelp_forest_database/"

#read data
upc_raw <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
  sheet = 1, col_types = "c" ) %>% # Set all columns to character type for troubleshooting) 
  clean_names()

urch_size_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                         sheet = 2) %>% clean_names()


swath_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()



urch_den_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()

#site metdata
reco_meta <- read_csv(file.path(datdir, "processed/recovery_survey_metadata.csv"))


################################################################################
#Step 1 - process UPC data

upc_build1 <- upc_raw %>%
  #########################
  # General tidying
  #########################
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0) %>%
  # Apply standard site naming
  mutate(site = str_replace(site, "([A-Za-z]+)([0-9]+)", "\\1_\\2"),
         site = str_replace(site, "Mar", "MAR")) %>%
  # Set data types
  mutate(
    name_of_data_enterer = as.factor(name_of_data_enterer),
    site = as.factor(site),
    date = ymd(date),  # converts date to year-month-day format
    heading_out = as.numeric(heading_out),
    buddy = as.character(buddy),
    transect = as.numeric(transect),
    depth_start = as.numeric(depth_start),
    depth_end = as.numeric(depth_end),
    depth_units = as.factor(depth_units),
    # Convert all substrate columns to factor
    across(starts_with("substrate"), as.factor),
    # Convert all upc columns to factor
    across(starts_with("upc"), as.factor),
    drift_algae = as.numeric(drift_algae),
    juvenile_laminariales = as.numeric(juvenile_laminariales)
  ) %>%
  # Convert depths to meters if depth_units is Feet
  mutate(
    depth_start = if_else(depth_units == "Feet", depth_start * 0.3048, depth_start),
    depth_end = if_else(depth_units == "Feet", depth_end * 0.3048, depth_end)
  ) %>%
  # Drop depth units
  select(-depth_units) %>%
  #########################
  # Calculate percent of each substrate
  #########################
  rowwise() %>%
  mutate(
    bedrock = sum(c_across(starts_with("substrate")) == "Bedrock (> 1m)") / sum(!is.na(c_across(starts_with("substrate")))),
    boulder = sum(c_across(starts_with("substrate")) == "Boulder (10 cm -1 m)") / sum(!is.na(c_across(starts_with("substrate")))),
    cobble = sum(c_across(starts_with("substrate")) == "Cobble (< 10 cm)") / sum(!is.na(c_across(starts_with("substrate")))),
    sand = sum(c_across(starts_with("substrate")) == "Sand") / sum(!is.na(c_across(starts_with("substrate"))))
  ) %>%
  ungroup() %>%
  select(-substrate_1, -substrate_2, -substrate_3, -substrate_4, -substrate_5) %>%
  #########################
 # Calculate percent of each relief category
 #########################
  #first convert relief to character
 mutate(across(starts_with("relief"), as.character)) %>%
  rowwise() %>%
  mutate(
    total_relief_points = rowSums(!is.na(across(starts_with("relief")))),
    
    relief_0_10cm = rowSums(across(starts_with("relief"), ~ . == "0-10cm"), na.rm = TRUE) / total_relief_points,
    relief_10cm_1m = rowSums(across(starts_with("relief"), ~ . == "10cm - 1 m"), na.rm = TRUE) / total_relief_points,
    relief_1m_2m = rowSums(across(starts_with("relief"), ~ . == "1 m - 2 m"), na.rm = TRUE) / total_relief_points,
    relief_gt_2m = rowSums(across(starts_with("relief"), ~ . == "> 2m"), na.rm = TRUE) / total_relief_points
  ) %>%
  # Remove temporary column used for calculations
  select(-total_relief_points)%>%
    ungroup() %>%
  select(-relief_1, -relief_2, -relief_3, -relief_4, -relief_5) 


#Calculate UPC proportions and store in a temporary object
upc_proportions <- upc_build1 %>%
  #########################
  # Convert upc columns to long format and filter out NA values
  #########################
  pivot_longer(cols = starts_with("upc"), names_to = "upc", values_to = "species") %>%
  filter(!is.na(species)) %>%  # Exclude NA values from the `upc` columns
  # Group by relevant columns to calculate total points per segment
  group_by(site, date, transect, segment) %>%
  mutate(total_points = n()) %>%
  # Calculate percent cover for each unique species
  group_by(site, date, transect, segment, species) %>%
  summarise(
    percent_cover = (n() / first(total_points)),  # Calculate percent cover based on total points
    .groups = 'drop'
  ) %>%
  # Pivot back to wide format with each species as a column
  pivot_wider(names_from = species, values_from = percent_cover, values_fill = 0, names_prefix = "upc_")

#Join the calculated `upc_proportions` data back to `upc_build1`
upc_build2 <- upc_build1 %>%
  left_join(upc_proportions, by = c("site", "date", "transect", "segment")) %>%
  select(-upc_1, -upc_2, -upc_3, -upc_4, -upc_5) %>%
  clean_names() %>%
  mutate(segment = case_when(
    segment == "0-5M" ~ 5,
    segment == "5-10M" ~ 10,
    segment == "10-15M" ~ 15,
    segment == "15-20M" ~ 20,
    segment == "20-25M" ~ 25,
    segment == "25-30M" ~ 30,
    segment == "30-35M" ~ 35,
    segment == "35-40M" ~ 40,
    segment == "40-45M" ~ 45,
    segment == "45-50M" ~ 50,
    segment == "50-55M" ~ 55,
    segment == "55-60M" ~ 60,
    segment == "60-65M" ~ 65,
    segment == "65-70M" ~ 70,
    segment == "70-75M" ~ 75,
    segment == "75-80M" ~ 80,
    TRUE ~ NA_real_  # Set NA for any unexpected values
  )) %>%
  rename(sub_bedrock = bedrock,
         sub_boulder = boulder,
         sub_cobble = cobble,
         sub_sand = sand) 




