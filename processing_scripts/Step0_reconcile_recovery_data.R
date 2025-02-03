

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
#gs4_auth()

#set dir
datdir <- "/Volumes/seaotterdb$/kelp_recovery/data/MBA_kelp_forest_database"

#read original data
quad_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                         sheet = 2) %>% clean_names()


kelp_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()

#read QAQC data
quad_qc <- read_sheet("https://docs.google.com/spreadsheets/d/10JlfhROxqXfnPoM21-UUPfGthjCls4uY1UbdxxFRPvg/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_qc <- read_sheet("https://docs.google.com/spreadsheets/d/10JlfhROxqXfnPoM21-UUPfGthjCls4uY1UbdxxFRPvg/edit?gid=0#gid=0",
                         sheet = 2) %>% clean_names()


kelp_qc <- read_sheet("https://docs.google.com/spreadsheets/d/10JlfhROxqXfnPoM21-UUPfGthjCls4uY1UbdxxFRPvg/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()



#site metdata
reco_meta <- read_csv(file.path(datdir, "processed/recovery_site_table.csv")) %>%
  rename(survey_date = official_survey_date)%>%
  #recreate factors for join
  mutate(
    survey_type = as.factor(survey_type),
    region = as.factor(region),
    site = as.factor(site),
    site_type = as.factor(site_type),
    site_long = as.factor(site_long)
  )

################################################################################
# process quadrat entry

quad_raw_build1 <- quad_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  data.frame()%>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0)%>%
  # Set quadrat to numeric by removing R/L
  mutate(quadrat = str_remove(quadrat, "[RL]$")) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(survey_date),
    transect = as.numeric(transect),
    quadrat = as.numeric(quadrat),
    substrate = factor(substrate),
    drift_superlayer = as.character(drift_superlayer)
  ) %>%
  #fix site name
  mutate(site = str_replace(site, "REC(\\d+)", "REC_\\1"))%>%
  select(-name_of_data_enterer,
          -observer_buddy,
          -write_in,
          -notes) 

quad_qc_build1 <-  quad_qc %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0) %>%
  # Set quadrat to numeric by removing R/L
  mutate(quadrat = str_remove(quadrat, "[RL]$")) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(survey_date),
    transect = as.numeric(transect),
    quadrat = as.numeric(quadrat),
    substrate = factor(substrate),
    drift_superlayer = as.character(drift_superlayer)
  ) %>%
  select(-name_of_data_enterer,
         -observer_buddy,
         -write_in,
         -notes) 

# Find rows in quad_raw that are NOT in quad_qc
discrep <- anti_join(quad_raw_build1, quad_qc_build1)

quad_discrep_values <- quad_raw_build1 %>%
  inner_join(quad_qc_build1, by = c("site", "site_type", "zone", "survey_date", "transect", "quadrat"), suffix = c("_raw", "_qc")) %>%
  mutate(across(ends_with("_raw"), ~ if_else(. != get(str_replace(cur_column(), "_raw$", "_qc")), paste(.," ≠ ", get(str_replace(cur_column(), "_raw$", "_qc"))), NA_character_), .names = "{.col}_diff")) %>%
  select(site, site_type, zone, survey_date, transect, quadrat, ends_with("_diff")) %>%
  filter(if_any(ends_with("_diff"), ~ !is.na(.)))


################################################################################
# process urchin entry

# Process urchin entry
urch_raw_build1 <- urchin_raw %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    size = as.numeric(size),
    count = as.numeric(count)
  ) %>%
  
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x15) %>%
  
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) %>%
  
  # Ensure unique species-size per grouping
  distinct()

urch_qc_build1 <- urchin_qc %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    size = as.numeric(size),
    count = as.numeric(count)
  ) %>%
  
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x15) %>%
  
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) %>%
  
  # Ensure unique species-size per grouping
  distinct()

# **Step 1: Find rows in urchin_raw that are NOT in urchin_qc**
urch_discrep <- anti_join(urch_raw_build1, urch_qc_build1, 
                          by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species", "size"))

# **Step 2: Identify discrepancies within matching groups**
urch_discrep_values <- urch_raw_build1 %>%
  inner_join(urch_qc_build1, 
             by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species", "size"), 
             suffix = c("_raw", "_qc")) %>%
  
  # Compare count values
  mutate(count_diff = if_else(count_raw != count_qc, paste(count_raw, "≠", count_qc), NA_character_)) %>%
  
  # Keep only mismatches
  filter(!is.na(count_diff)) %>%
  
  # Select relevant columns for output
  select(site, site_type, zone, date, transect, depth, depth_units, species, size, count_diff)



################################################################################
# process kelp entry

# Process urchin entry
kelp_raw_build1 <- kelp_raw %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x16) %>%
  # Ensure unique species-size per grouping
  distinct()

# Process urchin entry
kelp_qc_build1 <- kelp_qc %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x16) %>%
  # Ensure unique species-size per grouping
  distinct()
  


# **Step 1: Find rows in urchin_raw that are NOT in urchin_qc**
urch_discrep <- anti_join(urch_raw_build1, urch_qc_build1, 
                          by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species", "size"))

# **Step 2: Identify discrepancies within matching groups**
urch_discrep_values <- urch_raw_build1 %>%
  inner_join(urch_qc_build1, 
             by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species", "size"), 
             suffix = c("_raw", "_qc")) %>%
  
  # Compare count values
  mutate(count_diff = if_else(count_raw != count_qc, paste(count_raw, "≠", count_qc), NA_character_)) %>%
  
  # Keep only mismatches
  filter(!is.na(count_diff)) %>%
  
  # Select relevant columns for output
  select(site, site_type, zone, date, transect, depth, depth_units, species, size, count_diff)














library(dplyr)
library(stringr)
library(lubridate)

# **Process Kelp Data (Raw)**
kelp_raw_build1 <- kelp_raw %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x16) %>%
  
  # Arrange by relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species) %>%
  
  # Ensure unique species per grouping
  distinct()

# **Process Kelp Data (QC)**
kelp_qc_build1 <- kelp_qc %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x16) %>%
  
  # Arrange by relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species) %>%
  
  # Ensure unique species per grouping
  distinct()

# **Step 1: Find rows in `kelp_raw_build1` that are NOT in `kelp_qc_build1`**
kelp_discrep <- anti_join(kelp_raw_build1, kelp_qc_build1, 
                          by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species"))

# **Step 2: Identify discrepancies within matching groups**
kelp_discrep_values <- kelp_raw_build1 %>%
  inner_join(kelp_qc_build1, 
             by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species"), 
             suffix = c("_raw", "_qc")) %>%
  
  # Compare `count`, `stipe_counts_macrocystis_only`, and `subsample_meter` values
  mutate(
    count_diff = if_else(count_raw != count_qc, paste(count_raw, "≠", count_qc), NA_character_),
    stipe_count_diff = if_else(stipe_counts_macrocystis_only_raw != stipe_counts_macrocystis_only_qc, 
                               paste(stipe_counts_macrocystis_only_raw, "≠", stipe_counts_macrocystis_only_qc), NA_character_),
    subsample_meter_diff = if_else(subsample_meter_raw != subsample_meter_qc, 
                                   paste(subsample_meter_raw, "≠", subsample_meter_qc), NA_character_)
  ) %>%
  
  # Keep only mismatches
  filter(!is.na(count_diff) | !is.na(stipe_count_diff) | !is.na(subsample_meter_diff)) %>%
  
  # Select relevant columns for output
  select(site, site_type, zone, date, transect, depth, depth_units, species, 
         stipe_counts_macrocystis_only_raw, stipe_counts_macrocystis_only_qc, stipe_count_diff,
         count_raw, count_qc, count_diff,
         subsample_meter_raw, subsample_meter_qc, subsample_meter_diff)

# Print results
print(kelp_discrep_values)


