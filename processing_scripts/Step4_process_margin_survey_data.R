

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
gs4_auth()

#set dir
datadir <- "/Volumes/seaotterdb$/kelp_recovery/data/MBA_kelp_forest_database/"

#read data
upc_raw <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
  sheet = 1, col_types = "c" ) %>% # Set all columns to character type for troubleshooting) 
  clean_names()

urch_size_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                         sheet = 2, col_types = "c" ) %>% clean_names()


swath_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                       sheet = 3,col_types = "c" ) %>% clean_names()



urch_den_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1LB_ze2e68ZI7by8-uT1JNsLxEcNZBZF6jEuoWI1xLIU/edit?gid=0#gid=0",
                       sheet = 4,col_types = "c" ) %>% clean_names()

#site metdata
margin_meta <- read_csv(file.path(datadir, "processed/margin_site_table.csv"))


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
         sub_sand = sand) %>%
  data.frame()

##useful for checking
nrow(upc_raw) - 1 == nrow(upc_build2)

duplicates <- upc_build2 %>%
  group_by(site, date, transect, segment) %>%
  filter(n() > 1) %>%
  ungroup()

# Check if any duplicates exist
if (nrow(duplicates) > 0) {
  print("Duplicate segments found within the same site, date, and transect:")
  print(duplicates)
} else {
  print("No duplicate segments found within each site, date, and transect.")
}


################################################################################
#Step 2 - process urchin size data


urch_size_build1 <- urch_size_raw %>%
  #########################
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
  # Convert depths to meters if depth_units is Feet
  mutate(
    depth_start = if_else(depth_units == "Feet", depth_start * 0.3048, depth_start),
    depth_end = if_else(depth_units == "Feet", depth_end * 0.3048, depth_end)
  ) %>%
  # Drop depth units
  select(-depth_units) %>%
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
  select(-size, -name_of_data_enterer)%>%
  data.frame()

#prepare df for join 
urch_size_build2 <- urch_size_build1 %>%
                    select(-heading_out, -observer, -buddy, -depth_start,
                           -depth_end) %>%
                    #make long format
                    uncount(count) %>%
                    #calculate summary stats for join
                    group_by(site, date, transect, segment, species) %>%
                    summarize(sz_mean = mean(size_cm, na.rm=TRUE),
                              sz_sd = sd(size_cm, na.rm=TRUE)) %>%
                    #make wider
                    pivot_wider(names_from = "species", values_from = c("sz_mean","sz_sd")) %>%
                    clean_names()


################################################################################
#Step 3 - process kelp swath

swath_build1 <- swath_raw %>%
  #########################
# General tidying
#########################
# Remove example first row and classifiers
slice(-1) %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
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
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  # Convert depths to meters if depth_units is Feet
  mutate(
    depth_start = if_else(depth_units == "Feet", depth_start * 0.3048, depth_start),
    depth_end = if_else(depth_units == "Feet", depth_end * 0.3048, depth_end)
  ) %>%
  # Drop depth units
  select(-depth_units) %>%
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
  #drop field that are not needed
  select(-depth_start, -depth_end, -observer, -buddy, -name_of_data_enterer,
         -heading_out)


#process macrocystis first -- no subsampling 

mac_build1 <- swath_build1 %>% filter(species == "Macrocystis pyrifera") %>%
                select(-subsample_meter)

mac_n_plant <- mac_build1 %>% group_by(site, date, transect, segment, species) %>%
               summarize(n_macro_plants = n(),
                         avg_macro_stipe_density = mean(stipe_counts_macrocystis_only, na.rm=TRUE),
                         sd_macro_stipe = sd(stipe_counts_macrocystis_only, na.rm=TRUE)) %>%
  data.frame() %>%
  select(-species)

#process other macroalgae that were subsampled
algae_build1 <- swath_build1 %>% filter(species != "Macrocystis pyrifera") %>%
                select(-stipe_counts_macrocystis_only) %>%
                #4 cases where subsample meter is < 1 m -- change to 1 m because <1 is unrealistic
                mutate(subsample_meter = ifelse(subsample_meter < 1, 1, subsample_meter)) %>%
                #convert subsample to estimated total counts
                mutate(density = count*(5/subsample_meter))%>%
                #clean up
                  mutate(
                    species = trimws(gsub("\\s*\\(.*?\\)", "", species))
                  ) %>%
                select(-count) 

###checking
duplicates <- algae_build1 %>%
  group_by(site, date, transect, segment, species) %>%
  filter(n() > 1) %>%
  ungroup()

# Check if any duplicates exist
if (nrow(duplicates) > 0) {
  print("Duplicate segments found within the same site, date, and transect:")
  print(duplicates)
} else {
  print("No duplicate segments found within each site, date, and transect.")
}

#drop rows that were enterred incorrectly -- checked in raw data by JS
algae_build2 <- algae_build1 %>%
  filter(
    !(site == "MAR_03" & date == as.Date("2024-07-09") & transect == 1 & segment == 55 & species == "Stephanocystis" & density == 4),
    !(site == "MAR_09" & date == as.Date("2024-07-17") & transect == 3 &  segment== 65 & species == "Stephanocystis" & density == 1),
    !(site == "MAR_09" & date == as.Date("2024-07-17") & transect == 2 & segment == 70 & species == "Laminaria setchellii" & density == 6),
    !(site == "MAR_04" & date == as.Date("2024-06-24")& transect == 3 & segment == 70 & species == "Pterygophora" & density == 2)
  ) %>%
  #drop columns not needed
  select(-subsample_meter) %>%
  #make wider
  mutate(species = paste0("den_", species)) %>%  # Add "den_" prefix to species names
  pivot_wider(names_from = species, values_from = density) %>%
  clean_names() 

################################################################################
#Step 4 - process urchin density

urch_den_build1 <- urch_den_raw %>%
  #########################
    # General tidying
    #########################
    # Remove example first row and classifiers
    slice(-1) %>%
    select(-windows_ctrl_alt_shift_7_mac_command_option_shift_7) %>%
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
    purple_density = as.numeric(purple_density),
    subsample_meter_13 = as.numeric(subsample_meter_13),
    purple_conceiled = as.numeric(purple_conceiled),
    red_density = as.numeric(red_density),
    subsample_meter_16 = as.numeric(subsample_meter_16),
    red_conceiled = as.numeric(red_conceiled)
  ) %>%
  # Convert depths to meters if depth_units is Feet
  mutate(
    depth_start = if_else(depth_units == "Feet", depth_start * 0.3048, depth_start),
    depth_end = if_else(depth_units == "Feet", depth_end * 0.3048, depth_end)
  ) %>%
  # Drop depth units
  select(-depth_units) %>%
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
  #extrapolate densities
  mutate(purple_urchin_density = purple_density*(5/subsample_meter_13),
         red_urchin_density = red_density*(5/subsample_meter_16),
         purple_urchin_conceiled_density = purple_conceiled*(5/subsample_meter_13),
         red_urchin_conceiled_density = red_conceiled*(5/subsample_meter_16))%>%
  #drop columns
  select(-purple_density, -red_density, -subsample_meter_13, -subsample_meter_16,
         -purple_conceiled, -red_conceiled) %>%
  #fix one instance where red conceiled density is > total counts
  mutate(red_urchin_conceiled_density = if_else(
    is.na(red_urchin_conceiled_density) | is.na(red_urchin_density),
    red_urchin_conceiled_density,  # Keep NA if either is NA
    if_else(red_urchin_conceiled_density > red_urchin_density, 
            red_urchin_density, 
            red_urchin_conceiled_density)
  )) %>%
  #drop field that are not needed
  select(-depth_start, -depth_end, -observer, -buddy, -name_of_data_enterer,
         -heading_out)

  


#check
any(urch_den_build1$purple_urchin_conceiled_density > urch_den_build1$purple_urchin_density)
any(urch_den_build1$red_urchin_conceiled_density > urch_den_build1$red_urchin_density)


################################################################################
#join everything

#for join: upc_build2, urch_size_build1, mac_n_plant, algae_build2, urch_den_build1


#upc_build2 is the base df with complete segments
sapply(upc_build2[, c("site","date","transect","segment")], class)
sapply(mac_n_plant[, c("site","date","transect","segment")], class)


margin_join1 <- left_join(upc_build2, mac_n_plant, by = c("site","date","transect",
                                                          "segment")) %>%
                #replace NAs with true zeros
                  mutate(
                    n_macro_plants = replace_na(n_macro_plants, 0),
                    avg_macro_stipe_density = replace_na(avg_macro_stipe_density, 0),
                    sd_macro_stipe = replace_na(sd_macro_stipe, 0)
                  )

margin_join2 <- left_join(margin_join1, algae_build2, by = c("site","date","transect",
                                                             "segment")) %>%
                  #replace NAs with true zeros
                  mutate(
                    den_stephanocystis = replace_na(den_stephanocystis, 0),
                    den_nereocystis = replace_na(den_nereocystis, 0),
                    den_laminaria_setchellii = replace_na(den_laminaria_setchellii, 0),
                    den_pterygophora = replace_na(den_pterygophora, 0),
                    den_costaria_costata = replace_na(den_costaria_costata, 0),
                    den_mac_stump = replace_na(den_mac_stump, 0),
                    den_lam_stump = replace_na(den_lam_stump,0)
                  )

margin_join3 <- left_join(margin_join2, urch_den_build1, by = c("site","date","transect",
                                                                "segment")) %>%
                  #clean names to match scheme
                  rename(den_purple_urchin = purple_urchin_density,
                         den_red_urchin = red_urchin_density,
                         den_purple_conceiled = purple_urchin_conceiled_density,
                         den_red_conceiled = red_urchin_conceiled_density)

margin_join4 <- left_join(margin_join3, urch_size_build2, by = c("site","date","transect",
                                                                 "segment")) 



################################################################################
#export

write_csv(urch_size_build1, file.path(datadir, "processed/margin_urchin_size_fq.csv")) #last write 5 Nov 2024


write_csv(margin_join4, file.path(datadir, "processed/margin_combined_data.csv")) #last write 5 Nov 2024




