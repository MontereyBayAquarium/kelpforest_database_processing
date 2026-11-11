

################################################################################
# Recovery survey data processing
#
# Combines and processes reconciled 2024 and 2025 survey data.
# Final objects: quad_data, urchin_sizefq, and kelp_data.
# Original script author: JG.Smith (jossmith@mbayaq.org)



# Start with a clean R session.
rm(list=ls())

# Load packages used throughout the processing steps.
librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
# Uncomment if Google Sheets authentication is needed for the source sheets.
#gs4_auth()

# Root directory containing the recovery data and processed outputs.
datdir <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database"

# Read reconciled quadrat, urchin, and kelp data for 2024.
quad_raw_2024 <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_raw_2024 <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=172495760#gid=172495760",
                         sheet = 2) %>% clean_names() %>% select(-x15)


kelp_raw_2024 <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=265242012#gid=265242012",
                       sheet = 3) %>% clean_names()


# Read reconciled quadrat, urchin, and kelp data for 2025.
quad_raw_2025 <- read_sheet("https://docs.google.com/spreadsheets/d/1CDyHJqlKW5uRpg2Y9a7cfW5_a-5n_OkGv_RbiEKGDnA/edit?gid=0#gid=0",
                            sheet = 1) %>% clean_names()

urchin_raw_2025 <- read_sheet("https://docs.google.com/spreadsheets/d/1CDyHJqlKW5uRpg2Y9a7cfW5_a-5n_OkGv_RbiEKGDnA/edit?gid=0#gid=0",
                              sheet = 3) %>% clean_names()


kelp_raw_2025 <- read_sheet("https://docs.google.com/spreadsheets/d/1CDyHJqlKW5uRpg2Y9a7cfW5_a-5n_OkGv_RbiEKGDnA/edit?gid=0#gid=0",
                            sheet = 4) %>% clean_names()


# Read site metadata used to standardize survey records.
reco_meta <- read_csv(file.path(datdir, "processed/recovery_site_table.csv")) 


################################################################################
# Quadrat data
################################################################################


# Step 1: Combine the 2024 and 2025 source tables.

quad_build1 <- rbind(quad_raw_2024, quad_raw_2025)
# A result of zero indicates that the combined row count matches the inputs.
nrow(quad_build1)-((nrow(quad_raw_2024)+nrow(quad_raw_2025)))

# Step 2: Standardize fields and calculate quadrat-level measures.
quad_build2 <- quad_build1 %>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0) %>%
  # Remove trailing R/L labels before converting quadrat identifiers to numeric.
  mutate(quadrat = str_remove(quadrat, "[RL]$")) %>%
  # Set the analysis fields to their intended types.
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
  ##############################################################################
  # Extrapolate densities from sampled quadrants to the full quadrat.
  ##############################################################################
  # Missing counts are treated as zero under the current processing convention.
  mutate(across(c(purple_urchins, purple_conceiled, red_urchins, red_conceiled, tegula, pomaulax),
                ~ replace_na(.x, 0))) %>%
  # Missing sampled-quadrant counts are assumed to represent all four quadrants.
  mutate(across(c(purple_quadrants_sampled, red_quadrants_sampled, tegula_quadrants_sampled, 
                  pomaulax_quadrants_sampled),
                ~ replace_na(.x, 4))) %>%
  # Scale observed counts to the full four-quadrant area.
  mutate(
    purple_urchin_densitym2 = purple_urchins * (4 / purple_quadrants_sampled),
    purple_urchin_conceiledm2 = purple_conceiled * (4 / purple_quadrants_sampled),
    red_urchin_densitym2 = red_urchins * (4 / red_quadrants_sampled),
    red_urchin_conceiledm2 = red_conceiled * (4 / red_quadrants_sampled),
    tegula_densitym2 = tegula * (4 / tegula_quadrants_sampled),
    pomaulax_densitym2 = pomaulax * (4 / pomaulax_quadrants_sampled)
  ) %>%
  # Retain the scaled density fields instead of the original count/effort fields.
  select(-purple_urchins, -purple_conceiled, -purple_quadrants_sampled,
         -red_urchins, -red_conceiled, -red_quadrants_sampled,
         -tegula, -tegula_quadrants_sampled, -pomaulax, -pomaulax_quadrants_sampled) %>%
  ##############################################################################
  # Calculate percent cover from the UPC observations.
  ##############################################################################
  # Put UPC observations in long format so each recorded point is one row.
  pivot_longer(cols = starts_with("upc"), names_to = "upc", values_to = "species") %>%
  # Exclude unrecorded points; the denominator is the number of recorded points.
  filter(!is.na(species)) %>%
  # Count recorded UPC points within each quadrat.
  group_by(name_of_data_enterer, site, site_type, zone, survey_date, observer_buddy,
           transect, quadrat, substrate) %>%
  mutate(total_points = n()) %>%
  # Calculate each species' share of the recorded points within the quadrat.
  group_by(name_of_data_enterer, site, site_type, zone, survey_date, observer_buddy,
           transect, quadrat, substrate, relief, risk, species, 
           purple_urchin_densitym2, purple_urchin_conceiledm2,
           red_urchin_densitym2, red_urchin_conceiledm2, 
           tegula_densitym2, pomaulax_densitym2, lamr, macr, macj, nerj, ptej, 
           lsetj, eisj) %>%
  summarise(
    percent_cover = (n() / first(total_points)) * 100,
    .groups = 'drop'
  ) %>%
  # Return to wide format, with one cover field per observed species.
  pivot_wider(names_from = species, values_from = percent_cover, values_fill = 0,
              names_prefix = "upc_") %>%
  # Standardize generated names and clean the substrate label.
  clean_names() %>%
  mutate(substrate = word(substrate, 1)) %>%
  rename_with(~ str_replace(., "^upc_", "cov_")) %>%
  # Convert relief and risk observations to the derived centimeter/index fields.
  mutate(relief = ifelse(relief == 730, 30, relief),
         # Relief was estimated in 10 cm increments.
         relief_cm = relief*10,
         # Risk was estimated in 5 cm increments; include the quadrat diagonal.
         risk_cm = (risk*5) + 141.4,
         risk_index = risk_cm - 141.4
  ) %>%
  ##############################################################################
  # Standardize site names to uppercase letters plus a zero-padded number.
  ##############################################################################
  mutate(
    site = str_replace(site, "([A-Za-z]+)([0-9]+)", function(x) {
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])
      numbers <- parts[, 3]
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      paste0(letters, "_", numbers_padded)
    })) 

# Step 3: Join each survey year to its corresponding site metadata.
reco_meta_2024 <- reco_meta %>% filter(year(survey_date_2024) == 2024)

quad_build_2024 <- quad_build2 %>% filter(year(survey_date) == 2024) %>%
                      left_join(reco_meta_2024, by = c("site"="site_name_2024", 
                                                       "site_type" = "site_type_2024",
                                                       "zone","survey_date"="survey_date_2024")) %>%
                      # Carry official site names and metadata into the output.
                      select(survey_type, region, latitude, longitude,
                             site_official = site_name_2025, site_type_official = site_type_2025,
                             survey_date, zone, transect, quadrat, relief_cm,
                             risk_index, everything()) %>%
                      select(-latitude_old, -longitude_old, -survey_date_2025, -notes,
                             -relief, -risk_cm) %>%
                      # Drop rows without an official site name (practice sites).
                      filter(!is.na(site_official)) %>%
                      # Keep the latest survey date for each site, site type, and zone.
                      group_by(
                        site, site_type, zone
                      ) %>%
                      filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
                      ungroup() %>%
                      select(-risk, -name_of_data_enterer, -site, 
                             -site_type, -observer_buddy)
                            


reco_meta_2025 <- reco_meta %>% filter(year(survey_date_2025) == 2025)

quad_build_2025 <- quad_build2 %>% filter(year(survey_date) == 2025) %>%
  left_join(reco_meta_2025, by = c("site"="site_name_2025", 
                                   "site_type" = "site_type_2025",
                                   "zone","survey_date"="survey_date_2025")) %>%
select(-latitude_old, -longitude_old, -notes,
         -relief, risk_cm) %>%
  # Keep the latest survey date for each site, site type, and zone.
  group_by(
    site, site_type, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() %>%
  select(-risk, -name_of_data_enterer, 
         -observer_buddy, -risk_cm, -site_name_2024, -site_type_2024, -survey_date_2024) %>%
  rename(site_official = site, site_type_official = site_type)


# Combine the year-specific quadrat outputs and apply the official site fields.
ncol(quad_build_2024)
ncol(quad_build_2025)

quad_build3 <- rbind(quad_build_2024, quad_build_2025) %>%
                rename(site = site_official, site_type = site_type_official) 


# Diagnostic checks: concealed counts should not exceed total counts.
quad_build3 %>% filter(purple_urchin_conceiledm2 > purple_urchin_densitym2) 
quad_build3 %>% filter(red_urchin_conceiledm2 > red_urchin_densitym2) 

# Diagnostic plot: number of survey dates per site, site type, and zone.
date_counts <- quad_build3 %>%
  #quad_raw_2025 %>%
  group_by(site, site_type, zone) %>%
  summarise(num_dates = n_distinct(survey_date), .groups = "drop")

ggplot(date_counts, aes(x = num_dates, y = site, fill = site_type)) +
  geom_col(position = "dodge") +
  facet_wrap(~zone) +
  labs(
    x = "Number of Unique Survey Dates",
    y = "Site",
    title = "Unique Survey Dates per Site, Site Type, and Zone",
    fill = "Site Type"
  ) +
  theme_minimal()

# Diagnostic plot: number of survey dates in the 2025 quadrat output.
date_counts <- #quad_build3 %>%
  quad_build_2025 %>%
  group_by(site_official, site_type_official, zone) %>%
  summarise(num_dates = n_distinct(survey_date), .groups = "drop")

ggplot(date_counts, aes(x = num_dates, y = site_official, fill = site_type_official)) +
  geom_col(position = "dodge") +
  facet_wrap(~zone) +
  labs(
    x = "Number of Unique Survey Dates",
    y = "Site",
    title = "Unique Survey Dates per Site, Site Type, and Zone",
    fill = "Site Type"
  ) +
  theme_minimal()


# Diagnostic plot: transects recorded per 2025 site, site type, and zone.

transect_counts <- quad_raw_2025 %>%
  group_by(site, site_type, zone) %>%
  summarise(num_transects = n_distinct(transect), .groups = "drop")


ggplot(transect_counts, aes(x = num_transects, y = site, fill = site_type)) +
  geom_col(position = "dodge") +
  facet_wrap(~zone) +
  labs(
    x = "Number of Unique Transects",
    y = "Site",
    title = "Number of Transects per Site, Site Type, and Zone",
    fill = "Site Type"
  ) +
  theme_minimal()


# Diagnostic plot: unique quadrats recorded per 2024 transect.
quadrat_counts <- quad_raw_2024 %>%
  group_by(site, site_type, zone, transect) %>%
  summarise(num_quadrats = n_distinct(quadrat), .groups = "drop")

ggplot(quadrat_counts, aes(x = num_quadrats, y = as.factor(transect), fill = site_type)) +
  geom_col(position = "dodge") +
  facet_grid(zone ~ site, scales = "free_y") +  # Facet by zone and site
  labs(
    x = "Number of Unique Quadrats",
    y = "Transect",
    title = "Number of Quadrats per Site, Site Type, Zone, and Transect",
    fill = "Site Type"
  ) +
  theme_minimal()


# Inspect the quadrat identifiers recorded in the 2025 source table.
unique(quad_raw_2025$quadrat)


################################################################################
# Urchin size-frequency data
################################################################################

# Step 1: Combine the 2024 and 2025 source tables.

urchin_build1 <- rbind(urchin_raw_2024, urchin_raw_2025)
# A result of zero indicates that the combined row count matches the inputs.
nrow(urchin_build1)-((nrow(urchin_raw_2024)+nrow(urchin_raw_2025)))


# Step 2: Standardize fields and convert depths to meters.
urch_build2 <- urchin_build1 %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  # Set the analysis fields to their intended types.
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
  # Convert recorded feet to meters; other depth units are retained as-is.
  mutate(depth_m = ifelse(depth_units == "Feet",depth*0.3048,depth)) %>%
  select(-depth_units, -depth) %>%
  select(name_of_data_enterer, survey_date, everything()) %>%
  mutate(size = as.numeric(as.character(size))) %>%
  rename(size_cm = size) %>% 
  select(-date)%>%
  ##############################################################################
  # Apply the standardized site naming used for quadrat data.
  ##############################################################################
  mutate(
    site = str_replace(site, "([A-Za-z]+)([0-9]+)", function(x) {
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])
      numbers <- parts[, 3]
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      paste0(letters, "_", numbers_padded)
    }))

# Step 3: Join each survey year to site metadata and retain the latest survey
# date within each official site, site type, and zone.

urch_build_2024 <- urch_build2 %>% filter(year(survey_date) == 2024) %>%
  left_join(reco_meta_2024, by = c("site"="site_name_2024", 
                                   "site_type" = "site_type_2024",
                                   "zone","survey_date"="survey_date_2024")) %>%
  # Carry official site names and metadata into the output.
  select(survey_type, region, latitude, longitude,
         site_official = site_name_2025, site_type_official = site_type_2025,
         survey_date, zone, transect, depth_m, species, size_cm, count) %>%
  # Drop rows without an official site name (practice sites).
  filter(!is.na(site_official)) %>%
  # Keep the latest survey date for each official site, site type, and zone.
  group_by(
    site_official, site_type_official, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() 

# 2024 sites omitted after latest-survey filtering because they were resampled:
# REC01 INCIP Shallow, REC04 BAR Deep, REC10 FOR Deep, REC10 FOR Shallow, MAC01.

urch_build_2025 <- urch_build2 %>% filter(year(survey_date) == 2025) %>%
  left_join(reco_meta_2025, by = c("site"="site_name_2025", 
                                   "site_type" = "site_type_2025",
                                   "zone","survey_date"="survey_date_2025")) %>%
  # Carry the site fields into the output.
  select(survey_type, region, latitude, longitude,
         site_official = site, site_type_official = site_type,
         survey_date, zone, transect, depth_m, species, size_cm, count) %>%
  # Drop rows without a site name.
  filter(!is.na(site_official)) %>%
  # Keep the latest survey date for each official site, site type, and zone.
  group_by(
    site_official, site_type_official, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() 


# Combine the year-specific urchin size-frequency outputs.
ncol(urch_build_2024)
ncol(urch_build_2025)

urch_build3 <- rbind(urch_build_2024, urch_build_2025) %>%
  rename(site = site_official, site_type = site_type_official) 


################################################################################
# Kelp swath data
################################################################################

# Step 1: Combine the 2024 and 2025 source tables.
kelp_build1 <- rbind(kelp_raw_2024, kelp_raw_2025)
# A result of zero indicates that the combined row count matches the inputs.
nrow(kelp_build1)-((nrow(kelp_raw_2024)+nrow(kelp_raw_2025)))


# Step 2: Standardize fields and convert depths to meters.
kelp_build2 <- kelp_build1 %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  # Set the analysis fields to their intended types.
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
  # Convert recorded feet to meters; other depth units are retained as-is.
  mutate(depth_m = ifelse(depth_units == "Feet",depth*0.3048,depth)) %>%
  select(-depth_units, -depth, -date) %>%
  ##############################################################################
# Apply the standardized site naming used in the other data streams.
##############################################################################
mutate(
  site = str_replace(site, "([A-Za-z]+)([0-9]+)", function(x) {
    parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
    letters <- toupper(parts[, 2])
    numbers <- parts[, 3]
    numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
    paste0(letters, "_", numbers_padded)
  }))

# Step 3: Join each survey year to site metadata and retain the latest survey
# date within each official site, site type, and zone.

kelp_build_2024 <- kelp_build2 %>% filter(year(survey_date) == 2024) %>%
  left_join(reco_meta_2024, by = c("site"="site_name_2024", 
                                   "site_type" = "site_type_2024",
                                   "zone","survey_date"="survey_date_2024")) %>%
  # Carry official site names and metadata into the output.
  select(survey_type, region, latitude, longitude,
         site_official = site_name_2025, site_type_official = site_type_2025,
         survey_date, zone, transect, depth_m, species, stipe_counts_macrocystis_only,
         count, subsample_meter) %>%
  # Drop rows without an official site name (practice sites).
  filter(!is.na(site_official)) %>%
  # Keep the latest survey date for each official site, site type, and zone.
  group_by(
    site_official, site_type_official, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() 


kelp_build_2025 <- kelp_build2 %>% filter(year(survey_date) == 2025) %>%
  left_join(reco_meta_2025, by = c("site"="site_name_2025", 
                                   "site_type" = "site_type_2025",
                                   "zone","survey_date"="survey_date_2025")) %>%
  # Carry the site fields into the output.
  select(survey_type, region, latitude, longitude,
         site_official = site, site_type_official = site_type,
         survey_date, zone, transect, depth_m, species, stipe_counts_macrocystis_only,
         count, subsample_meter) %>%
  # Drop rows without a site name.
  filter(!is.na(site_official)) %>%
  # Keep the latest survey date for each official site, site type, and zone.
  group_by(
    site_official, site_type_official, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() 


# Combine the year-specific kelp swath outputs.
ncol(kelp_build_2024)
ncol(kelp_build_2025)

kelp_build3 <- rbind(kelp_build_2024, kelp_build_2025) %>%
  rename(site = site_official, site_type = site_type_official) 


##############################################################################
  # Calculate Macrocystis plant and stipe summaries by transect.
##############################################################################

macro_build1 <- kelp_build3 %>% filter(species == "MACPYR") %>%
    # Macrocystis is not subsampled.
    select(-subsample_meter, -count) %>%
    group_by(survey_date, site, site_type, zone, 
             transect)%>%
    summarize(n_macro_plants_20m2 = n(),
              macro_stipe_density_20m2 = mean(stipe_counts_macrocystis_only, na.rm =TRUE),
              macro_stipe_sd_20m2 = sd(stipe_counts_macrocystis_only, na.rm =TRUE),
              ) 

# Create a four-transect scaffold from site metadata, join observed summaries,
# and fill missing macro summary fields with zero under the current convention.
macro_build2_2024 <- reco_meta_2024 %>%
  # Create transect rows for each site and survey date.
  select(site = site_name_2025, site_type = site_type_2025, zone, latitude, longitude, 
         survey_date = survey_date_2024)%>%
  mutate(transect = list(1:4)) %>%
  unnest(transect) %>%
  # Join the observed Macrocystis summaries.
  left_join(macro_build1 %>% filter(year(survey_date) == 2024)) %>%
  mutate(across(c(n_macro_plants_20m2, macro_stipe_density_20m2, macro_stipe_sd_20m2), ~ replace_na(.x, 0)))

macro_build2_2025 <- reco_meta_2025 %>%
  # Create transect rows for each site and survey date.
  select(site = site_name_2025, site_type = site_type_2025, zone, latitude, longitude, 
         survey_date = survey_date_2025)%>%
  mutate(transect = list(1:4)) %>%
  unnest(transect) %>%
  # Join the observed Macrocystis summaries.
  left_join(macro_build1 %>% filter(year(survey_date) == 2025)) %>%
  mutate(across(c(n_macro_plants_20m2, macro_stipe_density_20m2, macro_stipe_sd_20m2), ~ replace_na(.x, 0)))

macro_build3 <- rbind(macro_build2_2024, macro_build2_2025)


##############################################################################
# Other kelp and algae densities
# Macrocystis is handled above. Scale other species' counts from the sampled
# portion to the full 10 m transect, then reshape species into separate columns.
##############################################################################

# Calculate sampled length, expansion scalar, and density for each record.
kelp_build4 <- kelp_build3 %>% filter(species != "MACPYR") %>%
  select(-stipe_counts_macrocystis_only) %>%
  # Values above 10 are treated as tape positions rather than sampled lengths.
  mutate(linear_meters_sampled = ifelse(subsample_meter < 10, 
                                        subsample_meter,
                                        ifelse(
                                          subsample_meter >= 10 & subsample_meter <=20, 
                                          (subsample_meter-10),
                                          ifelse(
                                            subsample_meter >= 20 & subsample_meter <=30, 
                                            (30-subsample_meter),
                                            subsample_meter
                                          )
                                        )),
         # Missing sampled length is treated as a full 10 m transect.
         linear_meters_sampled = ifelse(is.na(linear_meters_sampled),
                                        10,
                                        linear_meters_sampled
         ),
         # Expand observed counts to the full 10 m transect.
         scalar = 10/linear_meters_sampled,
         scalar = ifelse(scalar == 0.0, 1, scalar),
         density_20m2 = count*scalar
  )%>%
  select(-linear_meters_sampled, -scalar, -count, -subsample_meter)%>%
  mutate(species = paste0("density20m2_", species)) %>%
  # Create one density column per species.
  pivot_wider(
    names_from = species, 
    values_from = density_20m2,
    values_fill = 0 #replace NA with true 0
  ) %>% 
  clean_names() 


# Join non-Macrocystis densities to the transect-level Macrocystis summary.
kelp_build5 <- macro_build3 %>%
  left_join(kelp_build4) %>%
  mutate(across(14:21, ~ replace_na(.x, 0))) %>%
  select(-survey_type, -region, -depth_m) %>%
  select(site, site_type, latitude, longitude, zone, transect, 
         everything())


################################################################################
# Export data
################################################################################

# Assign the final processed data objects.
quad_data <- quad_build3
urchin_sizefq <- urch_build3
kelp_data <- kelp_build5

# Save the final data objects to the processed recovery directory.
save(quad_data, urchin_sizefq, kelp_data, file = 
       "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database/processed/recovery/kelp_recovery_data.rda") #last write 30 Sept 2025

# Optional: upload the saved file to Google Drive.
#drive_upload(
#  media = "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database/processed/recovery/kelp_recovery_data.rda",
#  path = as_id("1nlQtt72vigpL8qroFobFsR5j0nQ_eRZx"),
#  overwrite = TRUE
#) #last write 29 August 2025



# Optional CSV exports retained from earlier workflow versions.
#write.csv(quad_build, file.path(datdir,"processed/recovery/recovery_quad.csv"), row.names = FALSE) #last write 7 April 2025
#write.csv(urch_build, file.path(datdir,"processed/recovery/recovery_urch_sizefq.csv"), row.names = FALSE) #last write 28 March 2025
#write.csv(kelp_build1, file.path(datdir,"processed/recovery/recovery_kelpswath.csv"), row.names = FALSE) #last write 28 March 2025











