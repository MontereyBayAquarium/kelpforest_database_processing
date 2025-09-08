

################################################################################
# jossmith@mbayaq.org

#About
#This is the processing script used to clean raw entered dissection data. 
#The output is a cleaned data table


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate)
gs4_auth()

#set paths
datadir <- datdir <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database"

#read urchin data
urch_dat_orig <- read_sheet("https://docs.google.com/spreadsheets/d/1Ih-hBXRtfXVMdxw5ibZnXy_dZErdcx5FfeKMSc0HEc4/edit?gid=0#gid=0") %>%
  clean_names()

#load site tables
margin_meta <- read_csv(file.path(datadir, "processed/margin_site_table.csv"))
recovery_meta <- read_csv(file.path(datadir, "processed/recovery_site_table.csv")) 

################################################################################

################################################################################
#Step 1 - process dissection data 

#This is the full cleaned dissection data table. However, the site names are
#uncorrected. This database is useful for general sea urchin morphometrics
#and date*site is the unique identifier. For pairing data with field survey 
#data (e.g., margin surveys or recovery surveys), the parsed datasets in the
#next chunk should be used. 

gonad_dat_full <- urch_dat_orig %>%
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
  ###################
  # Clean up site name
  ###################
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
  # Apply standard site naming
  mutate(
    # Use a function within str_replace to process each match
    site_number = str_replace(site_number, "([A-Za-z]+)([0-9]+)", function(x) {
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
  ###################
  #clean transect
  ###################
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
  ###################
  #clean species
  ###################
  mutate(species = ifelse(species == "PUR","purple_urchin",species)) %>%
  #drop rows where all data are NA
  filter(!if_all(11:17, is.na))%>%
  ###################
  # clean sample number
  ###################
  mutate(
    sample_number = ifelse(sample_number %in% c("NULL"), NA, sample_number),              # Replace "NULL" with NA
    sample_number = gsub("^0+", "", sample_number),        # Remove leading and trailing zeros
    sample_number = as.numeric(na_if(sample_number, "NA"))) %>%
  #assign new sample numbers for all surveys that are not margin surveys. Sample
  #numbers were originally created by dissection volunteers but are irrelevant
  #EXCEPT for margin surveys, where the sample number is the tag number of a sea
  #urchin collected as a specific location on transect. 
  group_by(date_collected, date_fixed, survey_type, zone, transect, treatment, species) %>%
  mutate(
    sample_number_renumbered = if_else(
      survey_type == "Margin",
      sample_number,
      row_number()
    )
  ) %>%
  ungroup() %>%
  #drop old sample numbers
  select(-sample_number)%>%
  rename(sample_number = sample_number_renumbered) %>%
  ###################
  #remove outliers that were clearly entered wrong 
  ###################
  filter(test_height_mm < 100 | is.na(test_height_mm),
         test_diameter_mm < 100| is.na(test_diameter_mm),
         soft_tissue_mass_g < 80| is.na(soft_tissue_mass_g)) %>%
  #drop columns
  select(-animal_wet_mass_g) %>%
  ###################
  #remove outliers that were clearly entered wrong 
  ###################
  # Add small constant to gonad data
  mutate(gonad_mass_g = gonad_mass_g + 0.000001) %>%
  # Calculate gonad index
  mutate(gonad_index = (gonad_mass_g / animal_24hr_mass_g) * 100) %>%
  filter(gonad_index < 40)%>% #gonad index should never exceed 40, so these values are incorrect
                          #filtering below 40 results in max GI of 25% which is much more realistic
  #tidy up
  mutate(
    #apply standard site naming convention
    site_number = str_replace(site_number, "^([A-Za-z]{3})([0-9]+)$", "\\1_\\2"),
    zone = as.factor(str_to_sentence(zone))
  ) 

##Warnings ok
  

#check 
hist(gonad_dat_full$test_height_mm)
hist(gonad_dat_full$test_diameter_mm)
hist(gonad_dat_full$animal_24hr_mass_g)
hist(gonad_dat_full$gonad_mass_g)
hist(gonad_dat_full$soft_tissue_mass_g)
hist(gonad_dat_full$gonad_index)


################################################################################
#Step 2 - parse data for recovery surveys -- correct site names and filter 
#for join

gonad_dat_recovery_2024 <- gonad_dat_full %>%
  filter(survey_type == "Recovery") %>%
  filter(year(date_collected) == 2024) %>%
  left_join(., recovery_meta, by = c("date_collected" = "survey_date_2024",
                                     "site_number" = "site_name_2024",
                                     "site_type" = "site_type_2024",
                                     "zone"))
  
  #join recovery site table
  #NOTE: NAs in 'site_new' are because the site was sampled twice. These can 
  #get dropped later if merging with full databse. 
  left_join(., recovery_meta, by= c("survey_type","date_collected" = "survey_date_2024",
                                    "site_number" = "site_old", "site_type" = "site_type_old",
                                    "zone" = "zone")) %>%
  filter(survey_type == "Recovery",
         !(is.na(site_new))) %>%
  select(date_collected, date_fixed, survey_type, site = site_new,
         site_type = site_type_new, zone, species, sex, test_height_mm,
         test_diameter_mm, animal_24hr_mass_g, sample_number, gonad_index,
         latitude, longitude)


################################################################################
#Step 3 - parse data for margin surveys -- correct site names and filter 
#for join

gonad_dat_margin <- gonad_dat_full %>%
  filter(survey_type == "Margin") %>%
  mutate(transect = as.numeric(transect)) %>%
  left_join(., margin_meta, by= c("survey_type","date_collected" = "survey_date",
                                  "transect",
                                    "site_number" = "site_name_2024")) %>%
  filter(!(is.na(site_official))) %>%
  select(survey_type, date_collected, date_fixed, site = site_official,
         transect, heading_out, latitude, longitude, species, sample_number, sex, test_height_mm,
         test_diameter_mm, animal_24hr_mass_g, gonad_mass_g, gonad_index)


################################################################################
#export

write_csv(gonad_dat_full, file.path(datout, "dissection_data_cleaned.csv")) #last write 1 April 2025
write_csv(gonad_dat_recovery, file.path(datout, "dissection_data_recovery.csv")) #last write 1 April 2025
write_csv(gonad_dat_margin, file.path(datout, "dissection_data_margin.csv")) #last write 1 April 2025






