

################################################################################
# About
# 


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate)
gs4_auth()

#read urchin data
urch_dat_orig <- read_sheet("https://docs.google.com/spreadsheets/d/1Ih-hBXRtfXVMdxw5ibZnXy_dZErdcx5FfeKMSc0HEc4/edit?gid=0#gid=0") %>%
  clean_names()

################################################################################

################################################################################
#Step 1 - process dissection data


gonad_dat <- urch_dat_orig %>%
  # Replace missing values and clean up variables
  mutate(
    institution = as.factor(institution),
    name_of_data_enterer = as.character(name_of_data_enterer),
    date_collected = ymd(date_collected),
    date_fixed = ymd(date_fixed),
    date_processed = ymd(date_processed),
    site_number = as.factor(site_number),
    transect = as.factor(transect),
    treatment = as.factor(treatment),
    sex = as.factor(sex),
    test_height_mm = as.numeric(test_height_mm),
    test_diameter_mm = as.numeric(test_diameter_mm),
    animal_wet_mass_g = as.numeric(animal_wet_mass_g),
    animal_24hr_mass_g = as.numeric(animal_24hr_mass_g),
    gonad_mass_g = as.numeric(gonad_mass_g),
    soft_tissue_mass_g = as.numeric(soft_tissue_mass_g),
    notes = as.character(notes),
    date_entered = ymd_hms(date_entered)
  ) %>%
  # Select focal variables
  select(date_collected, date_fixed, site_number, transect, treatment, species, 
         sample_number, sex, test_height_mm, test_diameter_mm, animal_wet_mass_g,
         animal_24hr_mass_g, gonad_mass_g, soft_tissue_mass_g) %>%
  # Clean up site name
  mutate(
    site_number = gsub("-", "_", site_number),
    site_number = gsub("REC_", "REC", site_number),
    site_number = gsub("MAR_", "MAR", site_number)
  ) %>%
  # Do some initial site name cleaning
  mutate(site_number = case_when(
    site_number == "REC 012" ~ "REC10_FOR", #site name listed under transect
    site_number == "012" ~ "REC10_FOR", #site name listed under transect
    site_number == "REC 11_BAR" ~ "REC11_BAR",
    site_number == "DEEP" ~ "REC10_INCIP", #only site we did on this day
    site_number == ",AR06" ~ "MAR06",
    site_number == ",AR09" ~ "MAR06",
    site_number == "REC04" ~ "REC04_BAR",
    site_number == "REC06" ~ "MAR06",
    site_number == "REC3_BAR" ~ "REC03_BAR",
    site_number == "REC03INCIP" ~ "REC03_INCIP",
    site_number == "REC03_INCIIP" ~ "REC03_INCIP",
    site_number == "040" ~ "MAR08",
    site_number == "REC04_INCEP" ~ "REC04_INCIP",
    site_number == "REC4=FOR" ~ "REC04_FOR",
    site_number == "RED05_BAR" ~ "REC05_BAR",
    site_number == "RED05_INCID" ~ "REC05_INCIP",
    site_number == "004" ~ "REC02_INCIP",
    site_number == "RECD12_BAR" ~ "REC12_BAR",
    site_number == "RED12_BAR" ~ "REC12_BAR",
    site_number == "RED0_INCIP" ~ "REC01_INCIP",
    site_number == "REC1_BAR" ~ "REC01_BAR",
    site_number == "REC05=BAR" ~ "REC05_BAR",
    site_number == "REC05_INCID" ~ "REC05_INCIP",
    site_number == "REC0_INCIP" ~ "REC01_INCIP",
    TRUE ~ site_number  # Keep the original value if no condition matches
  )) %>%
  # Separate site_number into two parts and clean remaining underscores
  separate(site_number, into = c("site_number", "site_type"), sep = "_", remove = FALSE) %>%
  mutate(site_number = gsub("_.*", "", site_number)) %>%
  # Create survey_type column before site_number based on site_number prefix
  mutate(
    survey_type = case_when(
      startsWith(site_number, "MAR") ~ "Margin",
      startsWith(site_number, "REC") ~ "Recovery",
      TRUE ~ "Other"
    )
  ) %>%
  # Reorder to place survey_type before site_number
  relocate(survey_type, .before = site_number) %>%
  #fix transect
  mutate(transect = toupper(transect),
         transect = case_when(
           transect == "01" ~ "1", 
           transect == "03" ~ "3", 
           transect == "02" ~ "2", 
           TRUE ~ transect  
         )) %>%
  # Create zone column based on transect and place after site_type
  mutate(
    zone = case_when(
      transect == "SHALLOW" ~ "SHALLOW",
      transect == "DEEP" ~ "DEEP",
      TRUE ~ NA_character_
    ),
    transect = ifelse(transect %in% c("SHALLOW", "DEEP"), NA, transect) 
  ) %>%
  relocate(zone, .after = site_type) %>%
  #drop unknown entries
  filter(!transect %in% c("D", "REC10_FOR", "REC11-FOR","029")) %>%
  #fix species
  mutate(species = ifelse(species == "PUR","purple_urchin",species)) %>%
  # Replace "NULL" with NA and remove leading and trailing zeros in sample_number
  mutate(
    sample_number = ifelse(sample_number %in% c("NULL"), NA, sample_number),              # Replace "NULL" with NA
    sample_number = gsub("^0+|0+$", "", sample_number)         # Remove leading and trailing zeros
  )
  

####### OLD -- still working through code below

  # Add small constant to gonad data

  mutate(gonad_mass_g = gonad_mass_g + 0.000001) %>%
  # Calculate gonad index
  mutate(gonad_index = (gonad_mass_g / animal_24hr_mass_g) * 100) %>%
  filter(gonad_index < 100)%>%
  # Calculate mean gonad index for each site and transect
  group_by(date_collected, site_number, site_type, transect, species) %>%
  summarize(mean_GI = mean(gonad_index, na.rm = TRUE)) %>%
  # Pivot data wider by species
  pivot_wider(names_from = species, values_from = mean_GI) %>%
  mutate(site = factor(site_number),
         zone = factor(transect),
         site_type = factor(site_type),
         site_number = factor(site_number))