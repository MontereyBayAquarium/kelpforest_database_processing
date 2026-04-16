################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org
################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
#gs4_auth()

#set dir
datdir <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database"

#read reconciled data for 2024
upc_raw_2024 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Yf9eRnXy2pW4Xx6N8DVy2gKqRVF3e5QlirpSCjVVtqI/edit?gid=0#gid=0",
  sheet = 1, col_types = "c" ) %>% 
  clean_names()

urch_size_raw_2024 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Yf9eRnXy2pW4Xx6N8DVy2gKqRVF3e5QlirpSCjVVtqI/edit?gid=0#gid=0",
  sheet = 2, col_types = "c" ) %>% 
  clean_names()

swath_raw_2024 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Yf9eRnXy2pW4Xx6N8DVy2gKqRVF3e5QlirpSCjVVtqI/edit?gid=0#gid=0",
  sheet = 3,col_types = "c" ) %>% 
  clean_names()

urch_den_raw_2024 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Yf9eRnXy2pW4Xx6N8DVy2gKqRVF3e5QlirpSCjVVtqI/edit?gid=0#gid=0",
  sheet = 4,col_types = "c" ) %>% 
  clean_names()

#read reconciled date for 2025
upc_raw_2025 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/17nqbsYx7L1kJ9hBbrU5GtaVjnZ1drI9uCs7adlb5x8g/edit?gid=0#gid=0",
  sheet = 1, col_types = "c" ) %>% 
  clean_names()

urch_size_raw_2025 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/17nqbsYx7L1kJ9hBbrU5GtaVjnZ1drI9uCs7adlb5x8g/edit?gid=0#gid=0",
  sheet = 3, col_types = "c" ) %>% 
  clean_names()

swath_raw_2025 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/17nqbsYx7L1kJ9hBbrU5GtaVjnZ1drI9uCs7adlb5x8g/edit?gid=0#gid=0",
  sheet = 4,col_types = "c" ) %>% 
  clean_names()

urch_den_raw_2025 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/17nqbsYx7L1kJ9hBbrU5GtaVjnZ1drI9uCs7adlb5x8g/edit?gid=0#gid=0",
  sheet = 5,col_types = "c" ) %>% 
  clean_names()

#site metdata
margin_meta <- read_csv(file.path(datdir, "processed/margin_site_table.csv"))

#read dissection data
dissection_dat <- read_csv(file.path(datdir, "processed/dissection/dissection_data_cleanedv2.csv")) %>%
  filter(survey_type == "Margin") %>%
  dplyr::select(-site_type, -zone, -soft_tissue_mass_g)

################################################################################
# Step 1 - process UPC data
################################################################################

upc_build1 <- rbind(upc_raw_2024, upc_raw_2025)

#quickly check that it worked
nrow(upc_build1) - ((nrow(upc_raw_2024) + nrow(upc_raw_2025)))

upc_build2 <- upc_build1 %>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0) %>%
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
    buddy = as.character(buddy),
    transect = as.numeric(transect),
    depth_start = as.numeric(depth_start),
    depth_end = as.numeric(depth_end),
    depth_units = as.factor(depth_units),
    across(starts_with("substrate"), as.factor),
    across(starts_with("upc"), as.factor),
    drift_algae = as.numeric(drift_algae),
    juvenile_laminariales = as.numeric(juvenile_laminariales)
  ) %>%
  mutate(
    depth_start = if_else(depth_units == "Feet", depth_start * 0.3048, depth_start),
    depth_end = if_else(depth_units == "Feet", depth_end * 0.3048, depth_end)
  ) %>%
  select(-depth_units) %>%
  rowwise() %>%
  mutate(
    bedrock = sum(c_across(starts_with("substrate")) == "Bedrock (> 1m)") / sum(!is.na(c_across(starts_with("substrate")))),
    boulder = sum(c_across(starts_with("substrate")) == "Boulder (10 cm -1 m)") / sum(!is.na(c_across(starts_with("substrate")))),
    cobble = sum(c_across(starts_with("substrate")) == "Cobble (< 10 cm)") / sum(!is.na(c_across(starts_with("substrate")))),
    sand = sum(c_across(starts_with("substrate")) == "Sand") / sum(!is.na(c_across(starts_with("substrate"))))
  ) %>%
  ungroup() %>%
  select(-substrate_1, -substrate_2, -substrate_3, -substrate_4, -substrate_5) %>%
  mutate(across(starts_with("relief"), as.character)) %>%
  rowwise() %>%
  mutate(
    total_relief_points = rowSums(!is.na(across(starts_with("relief")))),
    relief_0_10cm = rowSums(across(starts_with("relief"), ~ . == "0-10cm"), na.rm = TRUE) / total_relief_points,
    relief_10cm_1m = rowSums(across(starts_with("relief"), ~ . == "10cm - 1 m"), na.rm = TRUE) / total_relief_points,
    relief_1m_2m = rowSums(across(starts_with("relief"), ~ . == "1 m - 2 m"), na.rm = TRUE) / total_relief_points,
    relief_gt_2m = rowSums(across(starts_with("relief"), ~ . == "> 2m"), na.rm = TRUE) / total_relief_points
  ) %>%
  select(-total_relief_points) %>%
  ungroup() %>%
  select(-relief_1, -relief_2, -relief_3, -relief_4, -relief_5)

upc_proportions <- upc_build2 %>%
  pivot_longer(cols = starts_with("upc"), names_to = "upc", values_to = "species") %>%
  filter(!is.na(species)) %>%
  group_by(site, date, transect, segment) %>%
  mutate(total_points = n()) %>%
  group_by(site, date, transect, segment, species) %>%
  summarise(
    percent_cover = (n() / first(total_points)),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = species, values_from = percent_cover, values_fill = 0, names_prefix = "upc_")

upc_build3 <- upc_build2 %>%
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
    TRUE ~ NA_real_
  )) %>%
  rename(
    sub_bedrock = bedrock,
    sub_boulder = boulder,
    sub_cobble = cobble,
    sub_sand = sand
  ) %>%
  data.frame()

nrow(upc_build1) == nrow(upc_build2)

duplicates <- upc_build2 %>%
  group_by(site, date, transect, segment) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicates) > 0) {
  print("Duplicate segments found within the same site, date, and transect:")
  print(duplicates)
} else {
  print("No duplicate segments found within each site, date, and transect.")
}

################################################################################
# Step 2 - process urchin size data
################################################################################

urch_size_build1 <- rbind(urch_size_raw_2024, urch_size_raw_2025)

nrow(urch_size_build1) - ((nrow(urch_size_raw_2024) + nrow(urch_size_raw_2025)))

urch_size_build2 <- urch_size_build1 %>%
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
    depth_start = as.numeric(depth_start),
    depth_end = as.numeric(depth_end),
    depth_units = as.factor(depth_units),
    segment = as.factor(segment),
    species = as.factor(species),
    size = as.factor(size),
    count = as.numeric(count)
  ) %>%
  mutate(
    depth_start = if_else(depth_units == "Feet", depth_start * 0.3048, depth_start),
    depth_end = if_else(depth_units == "Feet", depth_end * 0.3048, depth_end)
  ) %>%
  select(-depth_units) %>%
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
    TRUE ~ NA_real_
  )) %>%
  mutate(
    size_cm = as.numeric(gsub("cm", "", size))
  ) %>%
  select(-size, -name_of_data_enterer) %>%
  data.frame()

urch_size_build3 <- urch_size_build2 %>%
  filter(!is.na(count)) %>%
  select(-heading_out, -observer, -buddy, -depth_start, -depth_end) %>%
  uncount(count) %>%
  group_by(site, date, transect, segment, species) %>%
  summarize(
    sz_mean = mean(size_cm, na.rm = TRUE),
    sz_sd = sd(size_cm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "species", values_from = c("sz_mean", "sz_sd")) %>%
  clean_names()

################################################################################
# Step 3 - process kelp swath
################################################################################

swath_build1 <- rbind(swath_raw_2024, swath_raw_2025)

nrow(swath_build1) - ((nrow(swath_raw_2024) + nrow(swath_raw_2025)))

swath_build2 <- swath_build1 %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
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
    depth_start = as.numeric(depth_start),
    depth_end = as.numeric(depth_end),
    depth_units = as.factor(depth_units),
    segment = as.factor(segment),
    species = as.factor(species),
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  mutate(
    depth_start = if_else(depth_units == "Feet", depth_start * 0.3048, depth_start),
    depth_end = if_else(depth_units == "Feet", depth_end * 0.3048, depth_end)
  ) %>%
  select(-depth_units) %>%
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
    TRUE ~ NA_real_
  )) %>%
  select(-depth_start, -depth_end, -observer, -buddy, -name_of_data_enterer, -heading_out)

mac_build1 <- swath_build2 %>%
  filter(species == "Macrocystis pyrifera") %>%
  select(-subsample_meter)

mac_n_plant <- mac_build1 %>%
  group_by(site, date, transect, segment, species) %>%
  mutate(
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count)
  ) %>%
  summarize(
    n_macro_plants = n(),
    avg_macro_stipe_density = mean(stipe_counts_macrocystis_only, na.rm = TRUE),
    sd_macro_stipe = sd(stipe_counts_macrocystis_only, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  data.frame() %>%
  select(-species)

algae_build1 <- swath_build2 %>%
  filter(species != "Macrocystis pyrifera") %>%
  select(-stipe_counts_macrocystis_only) %>%
  mutate(
    subsample_meter = ifelse(is.na(subsample_meter), 5, subsample_meter),
    density = count * (5 / subsample_meter),
    species = trimws(gsub("\\s*\\(.*?\\)", "", species)),
    species = paste0("den_", species)
  ) %>%
  select(-count, -notes, -subsample_meter) %>%
  tidyr::pivot_wider(
    id_cols = c(site, date, transect, segment),
    names_from = species,
    values_from = density,
    values_fn = \(x) sum(x, na.rm = TRUE),
    values_fill = 0
  ) %>%
  janitor::clean_names()

################################################################################
# Step 4 - process urchin density
################################################################################

urch_den_build1 <- rbind(urch_den_raw_2024, urch_den_raw_2025)

nrow(urch_den_build1) - ((nrow(urch_den_raw_2024) + nrow(urch_den_raw_2025)))

urch_den_build2 <- urch_den_build1 %>%
  select(-windows_ctrl_alt_shift_7_mac_command_option_shift_7) %>%
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
  mutate(
    depth_start = if_else(depth_units == "Feet", depth_start * 0.3048, depth_start),
    depth_end = if_else(depth_units == "Feet", depth_end * 0.3048, depth_end)
  ) %>%
  select(-depth_units) %>%
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
    TRUE ~ NA_real_
  )) %>%
  mutate(
    purple_urchin_density = purple_density * (5 / subsample_meter_13),
    red_urchin_density = red_density * (5 / subsample_meter_16),
    purple_urchin_conceiled_density = purple_conceiled * (5 / subsample_meter_13),
    red_urchin_conceiled_density = red_conceiled * (5 / subsample_meter_16)
  ) %>%
  select(-purple_density, -red_density, -subsample_meter_13, -subsample_meter_16,
         -purple_conceiled, -red_conceiled) %>%
  mutate(
    red_urchin_conceiled_density = if_else(
      is.na(red_urchin_conceiled_density) | is.na(red_urchin_density),
      red_urchin_conceiled_density,
      if_else(red_urchin_conceiled_density > red_urchin_density,
              red_urchin_density,
              red_urchin_conceiled_density)
    )
  ) %>%
  select(-depth_start, -depth_end, -observer, -buddy, -name_of_data_enterer,
         -heading_out, -notes)

#check
any(urch_den_build2$purple_urchin_conceiled_density > urch_den_build2$purple_urchin_density)
any(urch_den_build2$red_urchin_conceiled_density > urch_den_build2$red_urchin_density)

################################################################################
# join everything
################################################################################

sapply(upc_build3[, c("site","date","transect","segment")], class)
sapply(mac_n_plant[, c("site","date","transect","segment")], class)

margin_join1 <- left_join(upc_build3, mac_n_plant, by = c("site","date","transect","segment")) %>%
  mutate(
    n_macro_plants = replace_na(n_macro_plants, 0),
    avg_macro_stipe_density = replace_na(avg_macro_stipe_density, 0),
    sd_macro_stipe = replace_na(sd_macro_stipe, 0)
  )

margin_join2 <- left_join(margin_join1, algae_build1, by = c("site","date","transect","segment")) %>%
  mutate(
    den_stephanocystis = replace_na(den_stephanocystis, 0),
    den_nereocystis = replace_na(den_nereocystis, 0),
    den_laminaria_setchellii = replace_na(den_laminaria_setchellii, 0),
    den_pterygophora = replace_na(den_pterygophora, 0),
    den_costaria_costata = replace_na(den_costaria_costata, 0),
    den_mac_stump = replace_na(den_mac_stump, 0),
    den_lam_stump = replace_na(den_lam_stump, 0)
  )

margin_join3 <- left_join(margin_join2, urch_den_build2, by = c("site","date","transect","segment")) %>%
  rename(
    den_purple_urchin = purple_urchin_density,
    den_red_urchin = red_urchin_density,
    den_purple_conceiled = purple_urchin_conceiled_density,
    den_red_conceiled = red_urchin_conceiled_density
  )

margin_join4 <- left_join(margin_join3, urch_size_build3, by = c("site","date","transect","segment"))

nrow(upc_build3) == nrow(margin_join4)

################################################################################
# update official site names and metadata
################################################################################

margin_2024 <- margin_join4 %>%
  filter(year(date) == 2024) %>%
  left_join(., margin_meta, by = c("site" = "site_name_2024",
                                   "date" = "survey_date",
                                   "transect")) %>%
  filter(!is.na(date_surveyed_originally)) %>%
  select(-name_of_data_enterer, -heading_out.x, -notes,
         -sz_mean_na, -sz_sd_na, -date_surveyed_originally) %>%
  rename(site_official = site_name_2025) %>%
  select(-site, -observer, -buddy) %>%
  select(survey_type, region, site = site_official, latitude,
         longitude, date, transect, heading = heading_out.y,
         depth_start, depth_end, segment, everything())

colnames(margin_2024)

margin_2025 <- margin_join4 %>%
  filter(year(date) == 2025) %>%
  left_join(., margin_meta, by = c("site" = "site_name_2025",
                                   "transect")) %>%
  filter(!is.na(date_surveyed_originally)) %>%
  select(-name_of_data_enterer, -heading_out.x, -notes,
         -sz_mean_na, -sz_sd_na, -date_surveyed_originally) %>%
  select(-observer, -buddy, -site_name_2024, -survey_date) %>%
  select(survey_type, region, site, latitude,
         longitude, date, transect, heading = heading_out.y,
         depth_start, depth_end, segment, everything())

margin_merge <- rbind(margin_2024, margin_2025)

################################################################################
# Step 5 - process dissection data and join to margin data
################################################################################

dissection_summary <- dissection_dat %>%
  filter(species == "purple_urchin") %>%
  mutate(
    site_number = str_replace(site_number, "([A-Za-z]+)([0-9]+)", function(x) {
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])
      numbers <- parts[, 3]
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      paste0(letters, "_", numbers_padded)
    })
  ) %>%
  group_by(
    site = site_number,
    date = date_collected,
    transect,
    segment = sample_number
  ) %>%
  summarize(
    n_dissected = n(),
    mean_gonad_index = mean(gonad_index, na.rm = TRUE),
    sd_gonad_index = sd(gonad_index, na.rm = TRUE),
    mean_test_diameter_mm = mean(test_diameter_mm, na.rm = TRUE),
    mean_test_height_mm = mean(test_height_mm, na.rm = TRUE),
    mean_animal_24hr_mass_g = mean(animal_24hr_mass_g, na.rm = TRUE),
    mean_gonad_mass_g = mean(gonad_mass_g, na.rm = TRUE),
    prop_female = mean(sex == "Female", na.rm = TRUE),
    prop_male = mean(sex == "Male", na.rm = TRUE),
    .groups = "drop"
  )

margin_final <- margin_merge %>%
  left_join(
    dissection_summary,
    by = c("site", "date", "transect", "segment")
  )

################################################################################
# QC for dissection join
################################################################################

margin_final %>%
  summarize(
    total_segments = n(),
    with_dissections = sum(!is.na(mean_gonad_index)),
    prop_with_dissections = with_dissections / total_segments
  ) %>%
  print()

margin_final %>%
  filter(!is.na(mean_gonad_index)) %>%
  select(site, date, transect, segment, n_dissected, mean_gonad_index) %>%
  head() %>%
  print()

################################################################################
# export
################################################################################

write_csv(urch_size_build1, file.path(datdir, "processed/margin_urchin_size_fq.csv"))
write_csv(margin_final, file.path(datdir, "processed/margin_combined_datav2.csv"))


