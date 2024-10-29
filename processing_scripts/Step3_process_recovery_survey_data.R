

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org




################################################################################
#steps still required

#1. Need to account for subsample kelp on line 179
#2. Check survey date as part of final join
#2. Check final join to identify data inconsistencies



#When urchins conceiled > total counts, set # conceiled as total counts. 3 total cases. 

################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
gs4_auth()

#set dir
datdir <- "/Volumes/seaotterdb$/kelp_recovery/data/MBA_kelp_forest_database/"

#read data
quad_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                         sheet = 2) %>% clean_names()


kelp_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()

#site metdata
reco_meta <- read_csv(file.path(datdir, "processed/recovery_survey_metadata.csv"))

################################################################################
#Step 1 - process quadrat data

#inspect
View(quad_raw)

quad_build <- quad_raw %>%
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
    substrate = factor(substrate)
  ) %>%
  # Extrapolate densities for subsamples
  mutate(
    purple_urchin_densitym2 = purple_urchins * (4 / purple_quadrants_sampled),
    purple_urchin_conceiledm2 = purple_conceiled * (4 / purple_quadrants_sampled),
    red_urchin_densitym2 = red_urchins * (4 / red_quadrants_sampled),
    red_urchin_conceiledm2 = red_conceiled * (4 / red_quadrants_sampled),
    tegula_densitym2 = tegula * (4 / tegula_quadrants_sampled),
    pomaulax_densitym2 = pomaulax * (4 / pomaulax_quadrants_sampled)
  ) %>%
  # Drop old columns
  select(-purple_urchins, -purple_conceiled, -purple_quadrants_sampled,
         -red_urchins, -red_conceiled, -red_quadrants_sampled,
         -tegula, -tegula_quadrants_sampled, -pomaulax, -pomaulax_quadrants_sampled) %>%
  #calculate upc
  # Step 1: Convert upc1 through upc8 columns to long format
  pivot_longer(cols = starts_with("upc"), names_to = "upc", values_to = "species") %>%
  # Group by quadrat and calculate total UPC points per quadrat
  group_by(name_of_data_enterer, site, site_type, zone, survey_date, observer_buddy,
           transect, quadrat, substrate) %>%
  mutate(total_points = n()) %>%  # Calculate total points per quadrat
  # Calculate percent cover for each species
  group_by(name_of_data_enterer, site, site_type, zone, survey_date, observer_buddy,
           transect, quadrat, substrate, species, purple_urchin_densitym2, purple_urchin_conceiledm2,
           red_urchin_densitym2, red_urchin_conceiledm2, tegula_densitym2, pomaulax_densitym2) %>%
  summarise(
    percent_cover = (n() / first(total_points)) * 100,  # Use total points dynamically for each quadrat
    .groups = 'drop'
  ) %>%
  # Step 4: Reshape back to wide format with species as columns, adding 'upc_' prefix
  pivot_wider(names_from = species, values_from = percent_cover, values_fill = 0,
              names_prefix = "upc_") %>%
  # Clean column names and remove any columns with 'na' in the name
  clean_names() %>%
  #clean up
  mutate(substrate = word(substrate, 1)) %>%
  #fix conceiled counts
  mutate(purple_urchin_conceiledm2 = ifelse(purple_urchin_conceiledm2 > purple_urchin_densitym2,
                                            purple_urchin_densitym2, purple_urchin_conceiledm2),
         red_urchin_conceiledm2 = ifelse(red_urchin_conceiledm2 > red_urchin_densitym2,
                                            red_urchin_densitym2, red_urchin_conceiledm2)) %>%
  #clean up
  rename_with(~ str_replace(., "^upc_", "cov_")) %>%
  rename(site_name = site,
         date_surveyed = survey_date)%>%
  #get the latest survey date for each site, site_type, zone, and transect by
  #joining metadata
  #check what didn't work
  #anti_join(reco_meta, by = c("site_name", "site_type", "zone", "date_surveyed"))
  #fix dates
  semi_join(reco_meta, by = c("site_name", "site_type", "zone", "date_surveyed"))
      #sites dropped: OK because resmapled REC01 INCIP Shallow, REC04 BAR Deep,
      #REC10 FOR Deep, REC01 INCIP Shallow, REC10 FOR Shallow, MAC01
  
# Result: `quad_build` now contains only records with the latest survey dates


#checking
#check if any instances where purple conceiled exceeds total counts
quad_build %>% filter(purple_urchin_conceiledm2 > purple_urchin_densitym2) #one case, fixed above
#check if any instances where purple conceiled exceeds total counts
quad_build %>% filter(red_urchin_conceiledm2 > red_urchin_densitym2) #two cases, fixed above



################################################################################
#Step 2 - process urchin size data

#inspect
View(urchin_raw)

#build raw size fq
urch_build <- urchin_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = factor(depth_units),
    species = factor(species),
    size = factor(size),
    count = as.numeric(count)
  ) %>%
  #convert feet to meters
  mutate(depth_m = ifelse(depth_units == "Feet",depth*0.3048,depth)) %>%
  select(-depth_units, -depth) %>%
  select(name_of_data_enterer, survey_date, everything()) %>%
  mutate(size = as.numeric(as.character(size))) %>%
  rename(size_cm = size) %>% select(-x15) %>%
  rename(site_name = site,
         date_surveyed = survey_date)%>%
  #fix incorrect dates
  mutate(date_surveyed = if_else(site_name == "REC01" & site_type == "FOR" & zone == "Deep" & date_surveyed == as.Date("2024-08-05"),
                               as.Date("2024-09-09"),
                               date_surveyed),
         site_type = if_else(site_name == "REC12" & zone == 'Shallow', "BAR",site_type)
         ) %>%
  anti_join(reco_meta, by = c("site_name", "site_type", "zone", "date_surveyed"))
#sites dropped: OK because resmapled REC01 INCIP Shallow, REC04 BAR Deep,
#REC10 FOR Deep, REC01 INCIP Shallow, REC10 FOR Shallow, MAC01

################################################################################
#Step 3 - process swath data

#inspect
View(kelp_raw)

#build kelp
kelp_build <- kelp_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = factor(depth_units),
    species = factor(species),
    count = as.numeric(count)
  ) %>%
  #convert feet to meters
  mutate(depth_m = ifelse(depth_units == "Feet",depth*0.3048,depth)) %>%
  select(-depth_units, -depth, -x16)

#calculate macro density
macro_density <- kelp_build %>% filter(species == "MACPYR") %>%
  #macro is not subsampled
  select(-subsample_meter, -count) %>%
  group_by(survey_date, site, site_type, zone, 
           transect)%>%
  summarize(n_macro_plants_20m2 = n(),
            macro_stipe_density_20m2 = mean(stipe_counts_macrocystis_only, na.rm =TRUE),
            macro_stipe_sd_20m2 = sd(stipe_counts_macrocystis_only, na.rm =TRUE),
            ) 
  #add true zeros



# Step 1: Find the latest survey date for each site_name, site_type, and zone in reco_meta
latest_surveys <- reco_meta %>%
  group_by(site_name, site_type, zone) %>%
  summarize(latest_date = max(date_surveyed), .groups = 'drop')

# Step 2: Ensure each site_name, site_type, and zone combination has four transects
# Create a full set of expected combinations (site_name, site_type, zone, transect 1-4)
expected_combinations <- latest_surveys %>%
  mutate(transect = list(1:4)) %>%
  unnest(transect)


# Step 3: Left join `macro_density` with `expected_combinations` to find missing rows
# Replace NA in `macro_density` columns with zeros where rows are missing
macro_density_complete <- expected_combinations %>%
  left_join(macro_density, 
            by = c("site_name" = "site", "site_type", "zone", "transect", "latest_date" = "survey_date")) %>%
  mutate(across(c(n_macro_plants_20m2, macro_stipe_density_20m2, macro_stipe_sd_20m2), ~ replace_na(.x, 0)))







kelp_density <- kelp_build %>% filter(species != "MACPYR") %>%
  select(-stipe_counts_macrocystis_only) %>%
  #######ACCOUNT FOR SUMSAMPLE HERE############
group_by(survey_date, site, site_type, zone, 
         transect, depth_m, species)%>%
  summarize(density20m2 = mean(count)) %>%
  # Rename species to include "density20m2_" prefix
  mutate(species = paste0("density20m2_", species)) %>%
  # Pivot wider by species
  pivot_wider(
    names_from = species, 
    values_from = density20m2,
    values_fill = 0 #replace NA with true 0
  ) %>% 
  clean_names()


################################################################################
#Step 4 - join everything

#inspect for join
str(quad_build) #base df
str(urch_size_summary)
str(macro_density)
str(kelp_density)


quad_build_combined <- quad_build %>%
  left_join(urch_size_summary, by = c("survey_date", "site", "site_type", "zone", "transect")) %>%
  left_join(macro_density, by = c("survey_date", "site", "site_type", "zone", "transect")) %>%
  left_join(kelp_density, by = c("survey_date", "site", "site_type", "zone", "transect"))


################################################################################
#Step 5 - export

#write to 2024 intern folder
output_file_path <- "/Users/jossmith/2024_KFI_projects/output/raw/recovery_data.csv"
write.csv(quad_build_combined, file = output_file_path, row.names = FALSE)




