

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org


################################################################################
#progress

#TO DO: update site names with new site table

#step 1 - complete 
#step 2 - complete
#step 3 - complete
#step 4 - drafted, needs QC

################################################################################
#steps still required

#. NEED TO RENAME SITES FOR EVEN BLOCKS -- SEE LINES 190-199


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
gs4_auth()

#set dir
datdir <- "/Volumes/seaotterdb$/kelp_recovery/data/MBA_kelp_forest_database"

#read reconciled data
quad_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=172495760#gid=172495760",
                         sheet = 2) %>% clean_names()


kelp_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1obf0FTO-w4sb5t5wqi1eVZL1Zby7G34vFZ1U7sNFm50/edit?gid=265242012#gid=265242012",
                       sheet = 3) %>% clean_names()

#site table
reco_meta <- read_csv(file.path(datdir, "processed/recovery_site_table.csv")) %>%
  select(site_new = site, site_type_new = site_type, site_old, site_type_old,
         zone, latitude, longitude, survey_date_2024)

################################################################################
#Step 1 - process quadrat data

#inspect
View(quad_raw)

quad_build <- quad_raw %>%
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
  #now apply scalar
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
  #calculate upc
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
  #clean up
  rename_with(~ str_replace(., "^upc_", "cov_")) %>%
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
    }))%>%
  #rename(date_surveyed = survey_date)%>%
  #get the latest survey date for each site, site_type, zone, and transect by
  #joining metadata
  #check what didn't work
  #fix dates
  ##############################################################################
  #join with site table
  ##############################################################################
  #join by old site names and site type. These were renamed in 2025
  left_join(reco_meta, by = c("site" = "site_old", "site_type"="site_type_old", 
                              "zone", "survey_date"="survey_date_2024")) %>%
  #drop sites that were resample
  filter(!is.na(site_new)) %>%
  #comment out the above to check what didn't match
  #anti_join(reco_meta, by = c("site" = "site_old", "site_type"="site_type_old", 
   #                          "zone", "survey_date"="survey_date_2024"))%>%
      #sites dropped: OK because resmapled REC01 INCIP Shallow, REC04 BAR Deep,
      #REC10 FOR Deep, REC10 FOR Shallow, MAC01
  #clean up
  select(-name_of_data_enterer, -site, -site_type, -observer_buddy) %>% 
  select(site = site_new, site_type = site_type_new, survey_date, latitude, 
         longitude, everything())
  
# Result: `quad_build` now contains only records with the latest survey dates

#checking
#check if any instances where conceiled exceeds total counts
quad_build %>% filter(purple_urchin_conceiledm2 > purple_urchin_densitym2) 
quad_build %>% filter(red_urchin_conceiledm2 > red_urchin_densitym2) 

#check for any duplicate sites
date_counts <- quad_build %>%
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


#check transects

transect_counts <- quad_build %>%
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
quadrat_counts <- quad_build %>%
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
unique(quad_build$quadrat)


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
    }))%>%
    #join with site table
    ##############################################################################
    #join by old site names and site type. These were renamed in 2025
    left_join(reco_meta, by = c("site" = "site_old", "site_type"="site_type_old", 
                                "zone", "survey_date"="survey_date_2024")) %>%
      #drop sites that were resample
      filter(!is.na(site_new)) %>%
      #comment out the above to check what didn't match
      #anti_join(reco_meta, by = c("site" = "site_old", "site_type"="site_type_old", 
      #                          "zone", "survey_date"="survey_date_2024"))%>%
      #sites dropped: OK because resmapled REC01 INCIP Shallow, REC04 BAR Deep,
      #REC10 FOR Deep, REC10 FOR Shallow, MAC01
      #clean up
      select(-name_of_data_enterer, -site, -site_type, -observer, -buddy) %>% 
      select(site = site_new, site_type = site_type_new, survey_date, latitude, 
             longitude, everything())

#sites dropped: OK because resmapled REC01 INCIP Shallow, REC04 BAR Deep,
#REC10 FOR Deep, REC01 INCIP Shallow, REC10 FOR Shallow, MAC01

################################################################################
#Step 3 - process swath data

#inspect
View(kelp_raw)

#build kelp
kelp_build <- kelp_raw %>%
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
  select(-depth_units, -depth, -date)
##############################################################################
  #calculate macro density
##############################################################################
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
macro_build1 <- reco_meta %>%
  #create list of transects for each site
  select(site, site_type, zone, latitude, longitude, survey_date)%>%
  mutate(transect = list(1:4)) %>%
  unnest(transect) %>%
  #add macro
  left_join(macro_density) %>%
  mutate(across(c(n_macro_plants_20m2, macro_stipe_density_20m2, macro_stipe_sd_20m2), ~ replace_na(.x, 0)))


##############################################################################
#calculate density of everyting else
##############################################################################


scalar <- kelp_build %>% filter(species != "MACPYR") %>%
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
                 #one meter sampled on each side of transect
                 area_sampled = linear_meters_sampled*2,
                 scalar = 20-area_sampled
                 )


kelp_density <- kelp_build %>% filter(species != "MACPYR") %>%
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
  clean_names() %>%
  select(-name_of_data_enterer, -observer, -buddy)


#join with macro
kelp_build1 <- macro_build1 %>%
  left_join(kelp_density) %>%
  mutate(across(12:19, ~ replace_na(.x, 0)))


################################################################################
#Step 4 - export

write.csv(quad_build, file.path(datdir,"processed/recovery/recovery_quad.csv"), row.names = FALSE) #last write 6 March 2025
write.csv(urch_build, file.path(datdir,"processed/recovery/recovery_urch_sizefq.csv"), row.names = FALSE) #last write 6 March 2025
write.csv(kelp_build1, file.path(datdir,"processed/recovery/recovery_kelpswath.csv"), row.names = FALSE) #last write 6 March 2025










