

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org


################################################################################
#steps involved


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
#gs4_auth()

#set dir
datdir <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database"

#read reconciled data for 2024
quad_raw_2024 <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_raw_2024 <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=172495760#gid=172495760",
                         sheet = 2) %>% clean_names() %>% select(-x15)


kelp_raw_2024 <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=265242012#gid=265242012",
                       sheet = 3) %>% clean_names()


#read reconciled data for 2025
quad_raw_2025 <- read_sheet("https://docs.google.com/spreadsheets/d/1CDyHJqlKW5uRpg2Y9a7cfW5_a-5n_OkGv_RbiEKGDnA/edit?gid=0#gid=0",
                            sheet = 1) %>% clean_names()

urchin_raw_2025 <- read_sheet("https://docs.google.com/spreadsheets/d/1CDyHJqlKW5uRpg2Y9a7cfW5_a-5n_OkGv_RbiEKGDnA/edit?gid=0#gid=0",
                              sheet = 3) %>% clean_names()


kelp_raw_2025 <- read_sheet("https://docs.google.com/spreadsheets/d/1CDyHJqlKW5uRpg2Y9a7cfW5_a-5n_OkGv_RbiEKGDnA/edit?gid=0#gid=0",
                            sheet = 4) %>% clean_names()


#read site table
reco_meta <- read_csv(file.path(datdir, "processed/recovery_site_table.csv")) 


################################################################################
#process quadrat data
################################################################################


#Step 1 - join 2024 and 2025 data

quad_build1 <- rbind(quad_raw_2024, quad_raw_2025)
#quickly check that it worked
nrow(quad_build1)-((nrow(quad_raw_2024)+nrow(quad_raw_2025)))

#Step 2 - process quad data
quad_build2 <- quad_build1 %>%
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
  ##############################################################################
  # Extrapolate densities for subsamples
  ##############################################################################
  #first change NA to 0 --- these are true zeroes
  mutate(across(c(purple_urchins, purple_conceiled, red_urchins, red_conceiled, tegula, pomaulax),
                ~ replace_na(.x, 0))) %>%
  #next change subsampled quadrants to 4 if NA --- these are assumed 4
  mutate(across(c(purple_quadrants_sampled, red_quadrants_sampled, tegula_quadrants_sampled, 
                  pomaulax_quadrants_sampled),
                ~ replace_na(.x, 4))) %>%
  #now apply scalar to extrapolate urchin densities to the full quadrat
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
  ##############################################################################
  #calculate percent cover
  ##############################################################################
  # Step 1: Convert upc1 through upc8 columns to long format
  pivot_longer(cols = starts_with("upc"), names_to = "upc", values_to = "species") %>%
  # Remove rows where species is NA (ignoring those UPC points)
  filter(!is.na(species)) %>%
  # Group by quadrat and calculate total UPC points per quadrat
  group_by(name_of_data_enterer, site, site_type, zone, survey_date, observer_buddy,
           transect, quadrat, substrate) %>%
  mutate(total_points = n()) %>%  # Calculate total points per quadrat
  # Calculate percent cover for each species based on the total points that were quantified
  group_by(name_of_data_enterer, site, site_type, zone, survey_date, observer_buddy,
           transect, quadrat, substrate, relief, risk, species, 
           purple_urchin_densitym2, purple_urchin_conceiledm2,
           red_urchin_densitym2, red_urchin_conceiledm2, 
           tegula_densitym2, pomaulax_densitym2, lamr, macr, macj, nerj, ptej, 
           lsetj, eisj) %>%
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
  #clean up
  rename_with(~ str_replace(., "^upc_", "cov_")) %>%
  #fix relief
  mutate(relief = ifelse(relief == 730, 30, relief),
         #convert relief to cm -- note: relief was estimated in 10cm increments
         relief_cm = relief*10,
         #calcualte risk index -- note: risk was estimated in 5cm increments
         risk_cm = (risk*5) + 141.4, #need to add the diagonal distance
         risk_index = risk_cm - 141.4 #141.4 is the diagonal distance of the quadrat in cm
  ) %>%
  ##############################################################################
  # Apply standard site naming
  ##############################################################################
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
    })) 

#Step 3 - separate 2024 and 2025 to do the site table join, then merge
reco_meta_2024 <- reco_meta %>% filter(year(survey_date_2024) == 2024)

quad_build_2024 <- quad_build2 %>% filter(year(survey_date) == 2024) %>%
                      left_join(reco_meta_2024, by = c("site"="site_name_2024", 
                                                       "site_type" = "site_type_2024",
                                                       "zone","survey_date"="survey_date_2024")) %>%
                      #now we must be very careful to adopt the new site names and types
                      select(survey_type, region, latitude, longitude,
                             site_official = site_name_2025, site_type_official = site_type_2025,
                             survey_date, zone, transect, quadrat, relief_cm,
                             risk_index, everything()) %>%
                      select(-latitude_old, -longitude_old, -survey_date_2025, -notes,
                             -relief, -risk_cm) %>%
                      # Drop rows where no official site name was found (these were practice sites)
                      filter(!is.na(site_official)) %>%
                      # Group and keep only the latest survey per site/year (some sites were resampled)
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
  # Group and keep only the latest survey per site/year (some sites were resampled)
  group_by(
    site, site_type, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() %>%
  select(-risk, -name_of_data_enterer, 
         -observer_buddy, -risk_cm, -site_name_2024, -site_type_2024, -survey_date_2024) %>%
  rename(site_official = site, site_type_official = site_type)


#join the two dfs together
ncol(quad_build_2024)
ncol(quad_build_2025)

quad_build3 <- rbind(quad_build_2024, quad_build_2025) %>%
                rename(site = site_official, site_type = site_type_official) 


#checking
#check if any instances where conceiled exceeds total counts
quad_build3 %>% filter(purple_urchin_conceiledm2 > purple_urchin_densitym2) 
quad_build3 %>% filter(red_urchin_conceiledm2 > red_urchin_densitym2) 

#do some tests
#check for any duplicate sites
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

#check for any duplicate sites
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


#check transects

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


#check quadrats
quadrat_counts <- quad_raw_2025 %>%
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


#check quadrat names
unique(quad_raw_2025$quadrat)


################################################################################
#Step 2 - process urchin size data

#Step 1 - join 2024 and 2025 data

urchin_build1 <- rbind(urchin_raw_2024, urchin_raw_2025)
#quickly check that it worked
nrow(urchin_build1)-((nrow(urchin_raw_2024)+nrow(urchin_raw_2025)))


#Step 2 - build raw size fq
urch_build2 <- urchin_build1 %>%
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
  rename(size_cm = size) %>% 
  select(-date)%>%
  ##############################################################################
  # Apply standard site naming
  ##############################################################################
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
    }))

#Step 3 - separate 2024 and 2025 to do the site table join, then merge

urch_build_2024 <- urch_build2 %>% filter(year(survey_date) == 2024) %>%
  left_join(reco_meta_2024, by = c("site"="site_name_2024", 
                                   "site_type" = "site_type_2024",
                                   "zone","survey_date"="survey_date_2024")) %>%
  #now we must be very careful to adopt the new site names and types
  select(survey_type, region, latitude, longitude,
         site_official = site_name_2025, site_type_official = site_type_2025,
         survey_date, zone, transect, depth_m, species, size_cm, count) %>%
  # Drop rows where no official site name was found (these were practice sites)
  filter(!is.na(site_official)) %>%
  # Group and keep only the latest survey per site/year (some sites were resampled)
  group_by(
    site_official, site_type_official, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() 

#sites dropped in 2024: OK because resmapled REC01 INCIP Shallow, REC04 BAR Deep,
#REC10 FOR Deep, REC01 INCIP Shallow, REC10 FOR Shallow, MAC01

urch_build_2025 <- urch_build2 %>% filter(year(survey_date) == 2025) %>%
  left_join(reco_meta_2025, by = c("site"="site_name_2025", 
                                   "site_type" = "site_type_2025",
                                   "zone","survey_date"="survey_date_2025")) %>%
  #now we must be very careful to adopt the new site names and types
  select(survey_type, region, latitude, longitude,
         site_official = site, site_type_official = site_type,
         survey_date, zone, transect, depth_m, species, size_cm, count) %>%
  # Drop rows where no official site name was found (these were practice sites)
  filter(!is.na(site_official)) %>%
  # Group and keep only the latest survey per site/year (some sites were resampled)
  group_by(
    site_official, site_type_official, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() 


#join the two dfs together
ncol(urch_build_2024)
ncol(urch_build_2025)

urch_build3 <- rbind(urch_build_2024, urch_build_2025) %>%
  rename(site = site_official, site_type = site_type_official) 


################################################################################
#Step 3 - process swath data

#Step 1 - join 2024 and 2025 data
kelp_build1 <- rbind(kelp_raw_2024, kelp_raw_2025)
#quickly check that it worked
nrow(kelp_build1)-((nrow(kelp_raw_2024)+nrow(kelp_raw_2025)))


#build kelp
kelp_build2 <- kelp_build1 %>%
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
  select(-depth_units, -depth, -date) %>%
  ##############################################################################
# Apply standard site naming
##############################################################################
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
  }))

#Step 3 - separate 2024 and 2025 to do the site table join, then merge

kelp_build_2024 <- kelp_build2 %>% filter(year(survey_date) == 2024) %>%
  left_join(reco_meta_2024, by = c("site"="site_name_2024", 
                                   "site_type" = "site_type_2024",
                                   "zone","survey_date"="survey_date_2024")) %>%
  #now we must be very careful to adopt the new site names and types
  select(survey_type, region, latitude, longitude,
         site_official = site_name_2025, site_type_official = site_type_2025,
         survey_date, zone, transect, depth_m, species, stipe_counts_macrocystis_only,
         count, subsample_meter) %>%
  # Drop rows where no official site name was found (these were practice sites)
  filter(!is.na(site_official)) %>%
  # Group and keep only the latest survey per site/year (some sites were resampled)
  group_by(
    site_official, site_type_official, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() 


kelp_build_2025 <- kelp_build2 %>% filter(year(survey_date) == 2025) %>%
  left_join(reco_meta_2025, by = c("site"="site_name_2025", 
                                   "site_type" = "site_type_2025",
                                   "zone","survey_date"="survey_date_2025")) %>%
  #now we must be very careful to adopt the new site names and types
  select(survey_type, region, latitude, longitude,
         site_official = site, site_type_official = site_type,
         survey_date, zone, transect, depth_m, species, stipe_counts_macrocystis_only,
         count, subsample_meter) %>%
  # Drop rows where no official site name was found (these were practice sites)
  filter(!is.na(site_official)) %>%
  # Group and keep only the latest survey per site/year (some sites were resampled)
  group_by(
    site_official, site_type_official, zone
  ) %>%
  filter(survey_date == max(survey_date, na.rm=TRUE)) %>%
  ungroup() 


#join the two dfs together
ncol(kelp_build_2024)
ncol(kelp_build_2025)

kelp_build3 <- rbind(kelp_build_2024, kelp_build_2025) %>%
  rename(site = site_official, site_type = site_type_official) 


##############################################################################
  #calculate macro density
##############################################################################

macro_build1 <- kelp_build3 %>% filter(species == "MACPYR") %>%
    #macro is not subsampled
    select(-subsample_meter, -count) %>%
    group_by(survey_date, site, site_type, zone, 
             transect)%>%
    summarize(n_macro_plants_20m2 = n(),
              macro_stipe_density_20m2 = mean(stipe_counts_macrocystis_only, na.rm =TRUE),
              macro_stipe_sd_20m2 = sd(stipe_counts_macrocystis_only, na.rm =TRUE),
              ) 

#add true zeros
macro_build2_2024 <- reco_meta_2024 %>%
  #create list of transects for each site
  select(site = site_name_2025, site_type = site_type_2025, zone, latitude, longitude, 
         survey_date = survey_date_2024)%>%
  mutate(transect = list(1:4)) %>%
  unnest(transect) %>%
  #add macro
  left_join(macro_build1 %>% filter(year(survey_date) == 2024)) %>%
  mutate(across(c(n_macro_plants_20m2, macro_stipe_density_20m2, macro_stipe_sd_20m2), ~ replace_na(.x, 0)))

macro_build2_2025 <- reco_meta_2025 %>%
  #create list of transects for each site
  select(site = site_name_2025, site_type = site_type_2025, zone, latitude, longitude, 
         survey_date = survey_date_2025)%>%
  mutate(transect = list(1:4)) %>%
  unnest(transect) %>%
  #add macro
  left_join(macro_build1 %>% filter(year(survey_date) == 2025)) %>%
  mutate(across(c(n_macro_plants_20m2, macro_stipe_density_20m2, macro_stipe_sd_20m2), ~ replace_na(.x, 0)))

macro_build3 <- rbind(macro_build2_2024, macro_build2_2025)


##############################################################################
#calculate density of all other algae.
#Note:: Macrocystis is not sub-sampled. All other are. We need to create a 
#scalar to extrapolate counts to the full transect. 
##############################################################################

#need to create scalar for subsample
kelp_build4 <- kelp_build3 %>% filter(species != "MACPYR") %>%
  select(-stipe_counts_macrocystis_only) %>%
  #first fix subsample -- note that some observers recorded the transect 
  #position rather than the meters sampled. If subsample is >10 then it is transect
  #position, so we need to fix this. 
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
         linear_meters_sampled = ifelse(is.na(linear_meters_sampled),
                                        10,
                                        linear_meters_sampled
         ),
         scalar = 10/linear_meters_sampled,
         scalar = ifelse(scalar == 0.0, 1, scalar),
         density_20m2 = count*scalar
  )%>%
  select(-linear_meters_sampled, -scalar, -count, -subsample_meter)%>%
  mutate(species = paste0("density20m2_", species)) %>%
  # Pivot wider by species
  pivot_wider(
    names_from = species, 
    values_from = density_20m2,
    values_fill = 0 #replace NA with true 0
  ) %>% 
  clean_names() 


#join with macro
kelp_build5 <- macro_build3 %>%
  left_join(kelp_build4) %>%
  mutate(across(14:21, ~ replace_na(.x, 0))) %>%
  select(-survey_type, -region, -depth_m) %>%
  select(site, site_type, latitude, longitude, zone, transect, 
         everything())


################################################################################
#Step 4 - export

write.csv(quad_build, file.path(datdir,"processed/recovery/recovery_quad.csv"), row.names = FALSE) #last write 7 April 2025
write.csv(urch_build, file.path(datdir,"processed/recovery/recovery_urch_sizefq.csv"), row.names = FALSE) #last write 28 March 2025
write.csv(kelp_build1, file.path(datdir,"processed/recovery/recovery_kelpswath.csv"), row.names = FALSE) #last write 28 March 2025











