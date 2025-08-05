

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive)
#gs4_auth()

#set dir
datdir <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database"

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
                        sheet = 3) %>% clean_names()


kelp_qc <- read_sheet("https://docs.google.com/spreadsheets/d/10JlfhROxqXfnPoM21-UUPfGthjCls4uY1UbdxxFRPvg/edit?gid=0#gid=0",
                      sheet = 3) %>% clean_names()



#site metdata
reco_meta <- read_csv(file.path(datdir, "processed/recovery_site_table.csv")) %>%
  #recreate factors for join
  mutate(
    survey_type = as.factor(survey_type),
    region = as.factor(region),
    site_official = as.factor(site_official),
    site_type = as.factor(site_type)
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
  #fix site name
  mutate(site = str_replace(site, "REC(\\d+)", "REC_\\1"))%>%
  select(-name_of_data_enterer,
         -observer_buddy,
         -write_in,
         -notes) 



# **Step 1: Identify Mismatched Keys**
keys_missing_in_quad_qc <- anti_join(quad_raw_build1, quad_qc_build1, 
                                     by = c("site", "site_type", "zone", "survey_date", "transect", "quadrat")) %>%
  select("site", "site_type", "zone", "survey_date", "transect", "quadrat")


#Step 2: Deal with non-UPC values first
quad_discrep_values <- quad_raw_build1 %>%
  inner_join(quad_qc_build1, by = c("site", "site_type", "zone", "survey_date", "transect", "quadrat"), suffix = c("_raw", "_qc")) %>%
  mutate(across(ends_with("_raw"), ~ if_else(. != get(str_replace(cur_column(), "_raw$", "_qc")), paste(.," ≠ ", get(str_replace(cur_column(), "_raw$", "_qc"))), NA_character_), .names = "{.col}_diff")) %>%
  select(site, site_type, zone, survey_date, transect, quadrat, ends_with("_diff")) %>%
  filter(if_any(ends_with("_diff"), ~ !is.na(.))) %>%
  mutate(resolved = "")


#Export
# Define file path for export
quad_file <- "quad_discrep_values.csv"
quad_keys <- "quad_keys.csv"

# Write the CSV locally
#write_csv(keys_missing_in_quad_qc, quad_keys)

# Upload to the specified Google Drive folder
#drive_upload(quad_keys, path = as_id("1IaTpgTw6Q8-EDvSo3oONBCMDVIfLyzRB"), overwrite = TRUE)


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
  select(-name_of_data_enterer, -observer, -buddy) %>%
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) %>%
  # Ensure unique species-size per grouping
  distinct()


# **Step 1: Find missing keys**
urch_discrep <- anti_join(urch_raw_build1, urch_qc_build1, 
                          by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species", "size"))

# **Step 2: Identify discrepancies within matching groups**
urch_discrep_values <- urch_qc_build1 %>%
  inner_join(urch_raw_build1, 
             by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species", "size"), 
             suffix = c("_raw", "_qc")) %>%
  
  # Compare count values
  mutate(count_diff = if_else(count_raw != count_qc, paste(count_raw, "≠", count_qc), NA_character_)) %>%
  
  # Keep only mismatches
  filter(!is.na(count_diff)) %>%
  
  # Select relevant columns for output
  select(site, site_type, zone, date, transect, depth, depth_units, species, size, count_diff)



#Export
# Define file path for export
quad_urchin <- "quad_urchin.csv"

# Write the CSV locally
write_csv(urch_discrep_values, quad_urchin)

# Upload to the specified Google Drive folder
drive_upload(quad_urchin, path = as_id("1IaTpgTw6Q8-EDvSo3oONBCMDVIfLyzRB"), overwrite = TRUE)


################################################################################
# process kelp entry

#**************************
#*Note: Kelp was ultimately sorted directly within the spreadsheet. 
#*Code below is incomplete.
#**************************

# Process kelp entry
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

# Process kelp entry
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


#step 2: separate macrocystis and make wider
kelp_raw_build2_mac <- kelp_raw_build1 %>%
  filter(species == "MACPYR") %>%
  arrange(site, site_type, zone, date, transect, depth, depth_units, stipe_counts_macrocystis_only)



#check for duplicates
kelp_raw_build1 %>%
  dplyr::group_by(site, site_type, zone, date, transect, depth, depth_units, stipe_counts_macrocystis_only, subsample_meter, species) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

#only one duplicate ... take first entry
kelp_raw_build2_other <- kelp_raw_build1 %>%
  filter(species != "MACPYR") %>%
  pivot_wider(
    names_from = "species", 
    values_from = "count",
    values_fn = function(x) x[1],  # Take the first value
    values_fill = NA
  )

kelp_qc_build2_mac <- kelp_qc_build1 %>%
  filter(species == "MACPYR")%>%
  arrange(site, site_type, zone, date, transect, depth, depth_units, stipe_counts_macrocystis_only)


#check for duplicates
kelp_qc_build1 %>%
  dplyr::group_by(site, site_type, zone, date, transect, depth, depth_units, stipe_counts_macrocystis_only, subsample_meter, species) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

#No duplicates, but keep code for consistency
kelp_qc_build2_other <- kelp_qc_build1 %>%
  filter(species != "MACPYR") %>%
  pivot_wider(
    names_from = "species", 
    values_from = "count",
    values_fn = function(x) x[1],  # Take the first value
    values_fill = NA
  )

View(anti_join(kelp_qc_build2_mac, kelp_raw_build2_mac))

macro_discrep_values <- kelp_qc_build2_mac %>%
  full_join(kelp_raw_build2_mac, 
            by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units"), 
            suffix = c("_qc", "_raw")) 

