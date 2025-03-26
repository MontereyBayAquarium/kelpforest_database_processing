

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive)
#gs4_auth()

#set dir
datdir <- "/Volumes/seaotterdb$/kelp_recovery/data/MBA_kelp_forest_database"

#read original data
upc_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_size_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                         sheet = 2) %>% clean_names()


kelp_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()


urchin_den_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                       sheet = 4) %>% clean_names()


#read QAQC data
upc_qc <- read_sheet("https://docs.google.com/spreadsheets/d/1buJTQ4sYNnXIyk041N-FwSdVT3pcx_TM_gZXK_3gtNE/edit?gid=0#gid=0",
                      sheet = 1) %>% clean_names()

urchin_size_qc <- read_sheet("https://docs.google.com/spreadsheets/d/1buJTQ4sYNnXIyk041N-FwSdVT3pcx_TM_gZXK_3gtNE/edit?gid=0#gid=0",
                        sheet = 2) %>% clean_names()


kelp_qc <- read_sheet("https://docs.google.com/spreadsheets/d/1buJTQ4sYNnXIyk041N-FwSdVT3pcx_TM_gZXK_3gtNE/edit?gid=0#gid=0",
                      sheet = 3) %>% clean_names()

urchin_den_qc <- read_sheet("https://docs.google.com/spreadsheets/d/1buJTQ4sYNnXIyk041N-FwSdVT3pcx_TM_gZXK_3gtNE/edit?gid=0#gid=0",
                             sheet = 4) %>% clean_names()


################################################################################
# process UPC entry

upc_raw_build1 <- upc_raw %>%
  #########################
# General tidying
#########################
# Remove example first row and classifiers
slice(-1) %>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0) %>%
  data.frame()%>%
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
  # Set data types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
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
  select(-name_of_data_enterer,
         -observer,
         -buddy)


upc_qc_build1 <- upc_qc %>%
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0) %>%
  data.frame() %>%
  mutate(
    site = str_replace(site, "([A-Za-z]+)([0-9]+)", function(x) {
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])
      numbers <- parts[, 3]
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      paste0(letters, "_", numbers_padded)
    }),
    drift_algae = map_chr(drift_algae, ~ if (length(.x) == 0) NA_character_ else as.character(.x[1])),
    drift_algae = na_if(drift_algae, "NULL"),
    drift_algae = na_if(drift_algae, "Other"),
    drift_algae = as.numeric(drift_algae)
  ) %>%
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    date = ymd(date),
    heading_out = parse_number(as.character(heading_out)),
    buddy = as.character(buddy),
    transect = as.numeric(transect),
    depth_start = as.numeric(parse_number(as.character(depth_start))),
    depth_end = as.numeric(parse_number(as.character(depth_end))),
    depth_units = as.factor(depth_units),
    across(starts_with("substrate"), as.factor),
    across(starts_with("upc"), as.factor),
    drift_algae = as.numeric(drift_algae),
    juvenile_laminariales = as.numeric(juvenile_laminariales)
  )%>%
  select(-name_of_data_enterer,
         -observer,
         -buddy,
         -notes)

ncol(upc_raw_build1)
ncol(upc_qc_build1)
nrow(upc_raw_build1)
nrow(upc_qc_build1)

# **Step 1: Identify Mismatched Keys**
upc_key_raw <- upc_raw_build1 %>%
  select("site", "date", "heading_out", "transect", "depth_start","depth_end",
"depth_units","segment")

upc_key_qc <- upc_qc_build1 %>%
  select("site", "date", "heading_out", "transect", "depth_start","depth_end",
         "depth_units","segment")

keys_missing_in_upc_qc <- anti_join(upc_key_raw, upc_key_qc)


#Step 2: Deal with non-UPC values first
upc_discrep_values <- upc_raw_build1 %>%
  inner_join(
    upc_qc_build1,
    by = c("site", "date", "heading_out", "transect", "depth_start", "depth_end",
           "depth_units", "segment"),
    suffix = c("_raw", "_qc")
  ) %>%
  mutate(across(
    ends_with("_raw"),
    ~ {
      raw_val <- as.character(.)
      qc_val  <- as.character(get(str_replace(cur_column(), "_raw$", "_qc")))
      if_else(raw_val != qc_val,
              paste(raw_val, " ≠ ", qc_val),
              NA_character_)
    },
    .names = "{.col}_diff"
  )) %>%
  select(
    site, date, heading_out, transect, depth_start, depth_end,
    depth_units, segment, ends_with("_diff")
  ) %>%
  filter(if_any(ends_with("_diff"), ~ !is.na(.)))


#write csv
temp_file <- tempfile(fileext = ".csv")
write_csv(upc_discrep_values, temp_file)

drive_upload(
  media = temp_file,
  path = as_id("1G1JaycgihbplJ2pOED_mXs-895hx4chS"),
  name = "upc_discrep_values.csv",
  overwrite = TRUE
)


################################################################################
# process urchin entry

urch_size_raw_build1 <- urchin_size_raw %>%
  ########################
# General tidying
#########################
# Remove example first row and classifiers
slice(-1) %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
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
  # Set data types
  mutate(
    name_of_data_enterer = as.factor(name_of_data_enterer),
    site = as.factor(site),
    date = ymd(date),  # converts date to year-month-day format
    heading_out = as.numeric(heading_out),
    observer = as.character(observer),
    buddy = as.character(buddy),
    transect = as.numeric(transect),
    depth_start = as.numeric(depth_start),
    depth_end = as.numeric(depth_end),
    depth_units = as.factor(depth_units),
    segment = as.factor(segment),
    species = as.factor(species),
    size = as.factor(size),
    count = as.numeric(count)
  ) %>%
  #convert segment to numeric
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
  #make size numeric
  mutate(
    size_cm = as.numeric(gsub("cm", "", size))
  ) %>%
  select(-size, -name_of_data_enterer,
         -observer, -buddy)%>%
  data.frame()


urch_size_qc_build1 <- urchin_size_qc %>%
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  mutate(
    site = str_replace(site, "([A-Za-z]+)([0-9]+)", function(x) {
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])
      numbers <- parts[, 3]
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      paste0(letters, "_", numbers_padded)
    })
  ) %>%
  mutate(
    name_of_data_enterer = as.factor(name_of_data_enterer),
    site = as.factor(site),
    date = ymd(date),
    heading_out = as.numeric(heading_out),
    observer = as.character(observer),
    buddy = as.character(buddy),
    transect = as.numeric(transect),
    depth_start = as.numeric(parse_number(na_if(as.character(depth_start), "NULL"))),
    depth_end = as.numeric(parse_number(na_if(as.character(depth_end), "NULL"))),
    depth_units = as.factor(depth_units),
    segment = as.factor(segment),
    species = as.factor(species),
    size = as.factor(size),
    count = as.numeric(count)
  ) %>%
  mutate(
    segment = case_when(
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
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    size_cm = as.numeric(gsub("cm", "", size))
  ) %>%
  select(-size, -name_of_data_enterer, -observer, -buddy,
         -notes) %>%
  data.frame()


# Summarize raw data by grouping keys, creating sorted vectors for sizes and counts
raw_summary <- urch_size_raw_build1 %>%
  group_by(site, date, heading_out, transect, depth_start, depth_end, depth_units, segment, species) %>%
  summarise(
    sizes_raw = list(sort(size_cm)),
    counts_raw = list(sort(count)),
    .groups = "drop"
  )

# Summarize QC data by grouping keys in the same way
qc_summary <- urch_size_qc_build1 %>%
  group_by(site, date, heading_out, transect, depth_start, depth_end, depth_units, segment, species) %>%
  summarise(
    sizes_qc = list(sort(size_cm)),
    counts_qc = list(sort(count)),
    .groups = "drop"
  )

# Join the summaries and compare the sorted vectors
urch_size_discrep_values <- inner_join(
  raw_summary, qc_summary, 
  by = c("site", "date", "heading_out", "transect", "depth_start", 
         "depth_end", "depth_units", "segment", "species")
) %>%
  mutate(
    sizes_diff = map2_chr(sizes_raw, sizes_qc, ~ 
                            if (all(.x == .y)) NA_character_ else paste("Raw sizes:", toString(.x), "≠ QC sizes:", toString(.y))),
    counts_diff = map2_chr(counts_raw, counts_qc, ~ 
                             if (all(.x == .y)) NA_character_ else paste("Raw counts:", toString(.x), "≠ QC counts:", toString(.y)))
  ) %>%
  # Keep only rows where either sizes or counts differ
  filter(!is.na(sizes_diff) | !is.na(counts_diff))



#write csv
temp_file <- tempfile(fileext = ".csv")
write_csv(urch_size_discrep_values, temp_file)

drive_upload(
  media = temp_file,
  path = as_id("1G1JaycgihbplJ2pOED_mXs-895hx4chS"),
  name = "urch_size_discrep_values.csv",
  overwrite = TRUE
)




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

