# ------------------------------------------------------------------------------
# Program Name: 07_Combine_Clean_Data.R
# Program Purpose: 
# 1. Combine results of manual screening and extraction from research team and student workshops
# 2. Complete final manual cleaning and add/create variables for analysis

# Note: Set working directory to local version of GitHub repository "Code" folder
# Note: This code used ClimActor version 0.0.1

# Author: Katherine Burley
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
# ------------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(tidytext)
library(igraph)
library(ggraph)
library("pdftools")
library(stringr)
library(tokenizers)
library(ClimActor)
library(rworldmap)
library(maps)
library(ggrepel)
library(ggpubr)
library(scales)

##### 1. Bring in Data #####
tracker <- read_excel("../Data/Manual_Extraction/Data_Screening_and_Extraction_Final.xlsx",
                      sheet = "Tracker", col_names = TRUE, range="A1:O290") # skip = 1

drop_downs <- read_excel("../Data/Manual_Extraction/Data_Screening_and_Extraction_Final.xlsx",
                         sheet = "Drop Downs", col_names = TRUE)

extr_other <- read_excel("../Data/Manual_Extraction/Data_Screening_and_Extraction_Final.xlsx",
                       sheet = "Dataset", col_names = TRUE, skip = 1)

extr_workshop <- read.csv("../Data/Manual_Extraction/Workshop_Survey_Responses_Clean_Final.csv",
                     header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

##### 2. Align and Combine #####
drop_downs <- drop_downs %>%
  select(-c("...1", "...7", "Intervention Type/Policy Instrument")) %>%
  rename("mitigation_strategy_cat" = "Mitigation Strategy",
         "subnat_type" = "Subnational Context",
         "sector" = "Sector",
         "impact_type" = "Type of Impact")

tracker <- tracker %>%
  mutate(across(everything(), as.character)) %>%
  mutate(workshop = ifelse(extracted == "Workshop", "Yes", "No")) %>%
  mutate(extracted = case_when(unique_id == "106351" ~ "No",
                               unique_id == "113145" ~ "No",
                               unique_id == "90709" ~ "No",
                               unique_id == "95478" ~ "No",
                               TRUE ~ extracted)) %>%
  mutate(extracted = replace(extracted, extracted == "Workshop", "Yes"))

extr_workshop <- extr_workshop %>%
  select(-c("observation", "group_id", "obs_num", "article_link", "more_impacts")) %>%
  mutate(across(everything(), as.character))

extr_other <- extr_other %>%
  select(-c("set_id", "year", "title", "authors", "include", "city.term", "region.term", "doi", "...42")) %>%
  mutate(across(everything(), as.character)) %>%
  filter(!unique_id %in% extr_workshop$unique_id)

# Combine extracted data
dataset <- extr_other %>%
  bind_rows(extr_workshop)

# Remove articles that were screened out during the extraction
to_remove <- unique(tracker$unique_id[tracker$extracted=="No"])
dataset <- dataset %>%
  filter(!unique_id %in% to_remove) 

##### 3. Categorize for Analysis #####

units_to_exclude <- c("kgCO2/100km", "kgCO2e ha−1 y−1", "kgCO2/m2-year", "tCO2eq/tMSW", "MgCO2eq/ha/year",
                      "kgCO2e/mi", "(tonnes CO2-eq.)/tonne recycled", "gCO2eq/meal", "kg CO2/m^2-year",
                      "kgCO2eq/kg of live weight", "kgCO2eq/tMSW", "	kgCO2e/kg-vegetable-year", "kgCO2eq/ha-year",
                      "7kg/hour.km", "Mg CO2e/ha", "kg CO2-eq/ha/year", "kgC02/m2", "kgCO2/m2", "kgCO2/person-day",
                      "kgCO2e/ha-year", "kgCO2e/kg-milk", "kgCO2eq/ton", "kgCO2eq/unit", "gCO2eq/MJ", "kgCO2eq/t waste",
                      "tCO2/MWh", "tonCO2/year/household", "CO2e Mg/ha-y", "g/km- vehicle", "kg/CO2eq per vehicle",
                      " kg- CO2 / km", "g/ km vehicle", "g/km", "g/km vehicle", "kg CO2-eq/GJ", "kgCO2/day/vehicle",
                      "MgCO2e/ha", "tons/km", "t/m^2", "kgCO2e ha−1 y−1", "kgCO2e/kg-vegetable-year", "t/m^2", "kg/hour.km")

units_ag <- c("kgCO2e ha−1 y−1", "MgCO2eq/ha/year", "kgCO2eq/ha-year", "Mg CO2e/ha", "kg CO2-eq/ha/year", 
              "kgCO2e/ha-year", "CO2e Mg/ha-y", "MgCO2e/ha", "kgCO2e ha−1 y−1")
units_waste <- c("tCO2eq/tMSW", "(tonnes CO2-eq.)/tonne recycled", "kgCO2eq/tMSW", "kgCO2eq/ton", "kgCO2eq/t waste")
units_bldgs <- c("kgCO2/m2-year", "kg CO2/m^2-year", "kgC02/m2", "kgCO2/m2", "t/m^2")


dataset <- dataset %>%
  mutate(per_unit_impacts = ifelse(units %in% units_to_exclude, "Yes", "No")) %>%
  mutate(units_available = ifelse(!is.na(study_subject_value), "Yes", "No")) %>%
  mutate(synthesis_group = case_when((subnational_wide == "Yes" & per_unit_impacts == "No") ~ "sg_1",
                                     (subnational_wide == "No" & per_unit_impacts == "No") ~ "sg_2",
                                     (subnational_wide == "No" & per_unit_impacts == "Yes") ~ "sg_3",
                                     (subnational_wide == "Yes" & per_unit_impacts == "Yes") ~ "sg_4")) %>%
  select(unique_id, subnational_wide, per_unit_impacts, everything()) 

# Move this section down 
primary_synthesis <- dataset %>%
  filter(synthesis_group == "sg_1" | (synthesis_group == "sg_4" & units_available=="Yes")) %>%
  group_by(unique_id, mitigation_strategy_cat, sector, per_unit_impacts, subnational_wide, units_available) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(mitigation_strategy_cat, sector) %>%
  summarise(total_studies = n())

dataset_sum_check <- dataset %>%
  group_by(unique_id, mitigation_strategy_cat, sector, per_unit_impacts, subnational_wide, units_available, synthesis_group) %>%
  summarise(n = n()) %>%
  ungroup()

group_sum <- dataset_sum_check %>%
  count(mitigation_strategy_cat, sector, synthesis_group) %>%
  pivot_wider(names_from = synthesis_group, values_from = n) %>%
  rename("no_group" = "NA") %>%
  filter(is.na(no_group))
  

##### 4. Bring in Actor Info with ClimActor #####

dataset_preca <- dataset

## Set Up Appropriate Column Names
names(dataset)
dataset$name <- dataset$subnational_name
dataset$country <- dataset$subnational_country
dataset$entity_type <- case_when(dataset$subnational_context == "City or Municipal Government" ~ "City",
                                 dataset$subnational_context == "Regional government" ~ "Region",
                                 dataset$subnational_context == "Regional Government" ~ "Region")
dataset <- rename_col(dataset)

## Standardize country names and ISO - need to fuzzify first
dataset <- clean_country_iso(dataset, country_dict, iso=3)

## Fuzzy match country names & clean ISO
dataset <- fuzzify_country(dataset, country_dict)
dataset <- clean_country_iso(dataset, country_dict, iso=3)

## Checks
sum(is.na(dataset$entity_type)) # No missing
table(dataset$entity_type)

sum(dataset$country %in% country_dict$right)
any(is.na(dataset$iso))

## Check for clean names
precleaned_names <- dataset$name
data <- clean_name(dataset, key_dict)
length(setdiff(precleaned_names, dataset$name))

## Phonetic matching
dataset <- phonetify_names(dataset, key_dict)

## Resolve entity types
dataset <- resolve_entity_types(dataset, key_dict)

## Bring in contextual data
dataset <- contextualize_data(dataset, contextuals, context = c("region", "population", "population_year", "lat", "lng", "area",
                                                                "initiatives_committed", "num_commit", "state"))

# Export this version
# write.csv(dataset, file = "../Data/Analysis/07_Combined_Data_postclimactor.csv",
#           fileEncoding = "UTF-8", row.names = F)

##### 5. Fill in Any Missing Contextual Info - START HERE #####

# Read in the dataset that has been cleaned with ClimActor
dataset <- read.csv("../Data/Analysis/07_Combined_Data_postclimactor.csv",
                    header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

to_scrape <- dataset %>%
  filter(is.na(population)) %>%
  distinct(name, iso, entity_type)

# write.csv(to_scrape, file = "../Data/Analysis/07_actorstoscrape.csv",
#           fileEncoding = "UTF-8", row.names = F)

scraped_data <- read.csv("../Data/Analysis/07_actorstoscrape_complete.csv",
                         header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

# Separate those with contextuals from ClimActor
dataset_context <- dataset %>%
  filter(!is.na(population))

# Bring in the new scraped data
dataset_nocontext <- dataset %>%
  filter(is.na(population)) %>%
  select(-c("region", "population", "population_year", "lat", "lng", "area", "initiatives_committed", "num_commit","state" )) %>%
  left_join(scraped_data, by=c("name", "entity_type", "iso"))

# Separate those without ClimActor or scraped contextuals to fill in manually
dataset_fillcontext <- dataset_nocontext %>%
  filter(is.na(population)) %>%
  mutate(population = case_when((name == "Schaghitcoke, NY" & entity_type == "City") ~ 7679,
                                (name == "Vicente Guerrero" & entity_type == "City") ~ 13876,
                                (name == "Kinmen County Government" & entity_type == "City") ~ 127723,
                                (name == "Greater Sydney" & entity_type == "City") ~ 5231147,
                                (name == "Jiangjiazhuang" & entity_type == "City") ~ 923,
                                (name == "Hong Kong" & entity_type == "City") ~ 7413070,
                                (name == "Macau" & entity_type == "Region") ~ 682300,
                                (name == "Brisbane" & entity_type == "City") ~ 2582007,
                                (name == "Layang-Layang village, Iskandar" & entity_type == "City") ~ 3000,
                                (name == "West Kotawaringin district" & entity_type == "City") ~ 272531,
                                (name == "Al Dhafra" & entity_type == "Region") ~ 202154,
                                (name == "Jabodetabek" & entity_type == "Region") ~ 31531549,
                                (name == "Wales" & entity_type == "City") ~ 3267501,
                                (name == "Magharvada" & entity_type == "City") ~ 1702,
                                (name == "Bhalot" & entity_type == "City") ~ 897,
                                (name == "Sitara" & entity_type == "City") ~ 1504,
                                (name == "Purkhawas" & entity_type == "City") ~ 1242,
                                (name == "Chomakot" & entity_type == "City") ~ 1036)) %>%
  mutate(population_year = case_when((name == "Schaghitcoke, NY" & entity_type == "City") ~ 2010,
                                     (name == "Vicente Guerrero" & entity_type == "City") ~ 2020,
                                     (name == "Kinmen County Government" & entity_type == "City") ~ 2014,
                                     (name == "Greater Sydney" & entity_type == "City") ~ 2021,
                                     (name == "Hong Kong" & entity_type == "City") ~ 2021,
                                     (name == "Macau" & entity_type == "Region") ~ 2021,
                                     (name == "Brisbane" & entity_type == "City") ~ 2021,
                                     (name == "West Kotawaringin district" & entity_type == "City") ~ 2021,
                                     (name == "Al Dhafra" & entity_type == "Region") ~ 2010,
                                     (name == "Jabodetabek" & entity_type == "Region") ~ 2021,
                                     (name == "Wales" & entity_type == "City") ~ 2022,
                                     (name == "Magharvada" & entity_type == "City") ~ 2011,
                                     (name == "Bhalot" & entity_type == "City") ~ 2011,
                                     (name == "Sitara" & entity_type == "City") ~ 2011,
                                     (name == "Purkhawas" & entity_type == "City") ~ 2011,
                                     (name == "Chomakot" & entity_type == "City") ~ 2011)) %>%
  mutate(lat = case_when((name == "Schaghitcoke, NY" & entity_type == "City") ~ 42.880556,
                         (name == "Vicente Guerrero" & entity_type == "City") ~ 30.726389,
                         (name == "Kinmen County Government" & entity_type == "City") ~ 24.44,
                         (name == "Shafar" & entity_type == "City") ~ 15.981667,
                         (name == "Greater Sydney" & entity_type == "City") ~ -33.867778,
                         (name == "Jiangjiazhuang" & entity_type == "City") ~ 31.325249416845757,
                         (name == "Sukaraksa" & entity_type == "Region") ~ -6.584167,
                         (name == "Macau" & entity_type == "City") ~ 22.166667,
                         (name == "Layang-Layang village, Iskandar" & entity_type == "City") ~ 1.8146684068329602,
                         (name == "West Kotawaringin district" & entity_type == "City") ~ -2.4,
                         (name == "Jabodetabek" & entity_type == "Region") ~ -6.175,
                         (name == "Wales" & entity_type == "City") ~ 52.3,
                         (name == "Sitara" & entity_type == "City") ~ 27.37355733165738,
                         (name == "Purkhawas" & entity_type == "City") ~ 26.152814991029977,
                         (name == "Chomakot" & entity_type == "City") ~ 25.092775890732547)) %>%
  mutate(lng = case_when((name == "Schaghitcoke, NY" & entity_type == "City") ~ -73.609722,
                         (name == "Vicente Guerrero" & entity_type == "City") ~ -115.990278,
                         (name == "Kinmen County Government" & entity_type == "City") ~ 118.33,
                         (name == "Shafar" & entity_type == "City") ~ 43.194167,
                         (name == "Greater Sydney" & entity_type == "City") ~ 151.21,
                         (name == "Jiangjiazhuang" & entity_type == "City") ~ 118.98558271076345,
                         (name == "Sukaraksa" & entity_type == "Region") ~ 106.525278,
                         (name == "Macau" & entity_type == "City") ~ 113.55,
                         (name == "Layang-Layang village, Iskandar" & entity_type == "City") ~ 103.476283585747,
                         (name == "West Kotawaringin district" & entity_type == "City") ~ 111.7333,
                         (name == "Jabodetabek" & entity_type == "Region") ~ 106.828611,
                         (name == "Wales" & entity_type == "City") ~ -3.6,
                         (name == "Sitara" & entity_type == "City") ~ 77.3425895343428,
                         (name == "Purkhawas" & entity_type == "City") ~ 72.7040240739808,
                         (name == "Chomakot" & entity_type == "City") ~ 76.10275047290644))

# Append
dataset_clean <- dataset_nocontext %>%
  filter(!is.na(population)) %>% # drop out the observations without population
  bind_rows(dataset_fillcontext) %>% # bring in the observations with filled data 
  bind_rows(dataset_context) %>%
  select(-c("X", "title","wiki_iso", "categories", "wiki_type", "area", "area_units", "region_hierarchy", "elevation",
            "gdp", "gdp_units", "gdp_year", "area_year", "initiatives_committed", "num_commit", "state", "region"))

# Merge in region from country_dict
region_map <- country_dict %>%
  distinct(iso, region)

dataset_clean <- dataset_clean %>%
  left_join(region_map, by="iso")

##### 6. Final Changes and Export #####

# Rename/Combine some Tier 2 Categories
dataset_clean <- dataset_clean %>%
  mutate(mitigation_stratcat_t2 = case_when((mitigation_strategy_cat == "Energy efficiency measures" & sector=="Electricity and heat") ~ "Energy system efficiencies",
                                            (mitigation_strategy_cat == "Smart grid, Smart meters, intelligent controls, thermostats, building information and monitoring systems" & sector=="Electricity and heat") ~ "Energy system efficiencies",
                                            (mitigation_strategy_cat == "Waste heat recovery" & sector=="Electricity and heat") ~ "Energy system efficiencies",
                                            mitigation_strategy_cat == "Waste segregation" ~ "Waste system management",
                                            mitigation_strategy_cat == "Water management system efficiencies" ~ "Water and wastewater system efficiencies",
                                            (mitigation_strategy_cat == "Demand side energy management, optimization, peak shifting/ shaving" & sector=="Waste") ~ "Water and wastewater system efficiencies",
                                            mitigation_strategy_cat == "Transition to low emissions fuels" ~ "Fuel switching and fuel efficiency",
                                            (mitigation_strategy_cat == "Smart grid, Smart meters, intelligent controls, thermostats, building information and monitoring systems" & sector== "Buildings") ~ "Smart meters, intelligent controls, thermostats, building information and monitoring systems",
                                            (mitigation_strategy_cat == "Smart grid, Smart meters, intelligent controls, thermostats, building information and monitoring systems" & sector== "Electricity and heat") ~ "Energy system efficiencies",
                                            TRUE ~ mitigation_strategy_cat)) %>%
  mutate(mitigation_stratcat_t2 = case_when(intervention %in% c("Use lights 10 minutes less each day", "temperature setbacks during lighter building load conditions", "Thermostat temperature setbacks") ~ "Demand side energy management, optimization, peak shifting/ shaving",
                                            TRUE ~ mitigation_stratcat_t2))

# Define Tier 1 Categories:
dataset_clean <- dataset_clean %>%
  mutate(mitigation_stratcat_t1 = case_when(mitigation_stratcat_t2 == "Agricultural land management" ~ "Agriculture and farm practices",
                                            mitigation_stratcat_t2 == "Biomass, biodiesel, biomass gasification, ethanol production" ~ "Agriculture and farm practices",
                                            mitigation_stratcat_t2 == "Urban agriculture" ~ "Agriculture and farm practices", 
                                            mitigation_stratcat_t2 == "Master planning, integrated planning, urban form, design, planning" ~ "Land use and development",
                                            mitigation_stratcat_t2 == "Transit oriented development" ~ "Land use and development",
                                            mitigation_stratcat_t2 == "Afforestation, greening" ~ "Land use and development",
                                            mitigation_stratcat_t2 == "Cool roof/facade, roof garden, green roof" ~ "Building construction and improvement",
                                            (mitigation_stratcat_t2 == "Energy efficiency measures" & sector == "Buildings") ~ "Building construction and improvement",
                                            mitigation_stratcat_t2 == "Green building, passive solar building design, net zero emission building, carbon neutral building" ~ "Building construction and improvement",
                                            mitigation_stratcat_t2 == "Retrofitting" ~ "Building construction and improvement",
                                            mitigation_stratcat_t2 == "Demand side energy management, optimization, peak shifting/ shaving" ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "Cogeneration, combined heat and power, tri-generation, combined heat, cooling, and power" & sector == "Buildings") ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "District heating/cooling, expansion" & sector == "Buildings") ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "Energy storage, batteries" & sector == "Buildings") ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "Other renewable energy" & sector == "Buildings") ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "Smart meters, intelligent controls, thermostats, building information and monitoring systems" & sector == "Buildings") ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "Solar PV energy" & sector == "Buildings") ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "Solar PV thermal, Solar PV cooling, Solar trigeneration CPVT" & sector == "Buildings") ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "Fuel switching and fuel efficiency" & sector == "Buildings") ~ "Building energy and heat systems",
                                            (mitigation_stratcat_t2 == "Geothermal heat pumps, ground source heat pumps" & sector == "Buildings") ~ "Building energy and heat systems",
                                            mitigation_stratcat_t2 == "Carbon capture and storage, carbon capture and sequestration" ~ "Clean energy generation",
                                            mitigation_stratcat_t2 == "Low-carbon energy generation" ~ "Clean energy generation",
                                            (mitigation_stratcat_t2 == "Other renewable energy" & sector == "Electricity and heat") ~ "Clean energy generation",
                                            (mitigation_stratcat_t2 == "Solar PV energy" & sector == "Electricity and heat") ~ "Clean energy generation",
                                            (mitigation_stratcat_t2 == "Wind energy" & sector == "Electricity and heat") ~ "Clean energy generation",
                                            (mitigation_stratcat_t2 == "Cogeneration, combined heat and power, tri-generation, combined heat, cooling, and power" & sector == "Electricity and heat") ~ "Energy system operations",
                                            (mitigation_stratcat_t2 == "District heating/cooling, expansion" & sector == "Electricity and heat") ~ "Energy system operations",
                                            (mitigation_stratcat_t2 == "Energy system efficiencies" & sector == "Electricity and heat") ~ "Energy system operations",
                                            (mitigation_stratcat_t2 == "Energy storage, batteries" & sector == "Electricity and heat") ~ "Energy system operations",
                                            mitigation_stratcat_t2 == "Circular economy, industrial symbiosis, use of recycled materials" ~ "Circular economy, industrial symbiosis, use of recycled materials",
                                            mitigation_stratcat_t2 == "Emissions trading, cap-and-trade, carbon tax" ~ "Emissions trading, cap-and-trade, carbon tax",
                                            mitigation_stratcat_t2 == "Non-motorized transport" ~ "Alternative transportation modes",
                                            mitigation_stratcat_t2 == "Public transport" ~ "Alternative transportation modes",
                                            mitigation_stratcat_t2 == "Ride sharing, carpooling" ~ "Alternative transportation modes",
                                            mitigation_stratcat_t2 == "Electric Mobility (EV, HEV)" & sector == "Transport" ~ "Clean vehicle transportation",
                                            (mitigation_stratcat_t2 == "Fuel switching and fuel efficiency" & sector == "Transport") ~ "Clean vehicle transportation",
                                            mitigation_stratcat_t2 == "Intelligent transportation system" ~ "Transportation system management",
                                            mitigation_stratcat_t2 == "Travel demand management" ~ "Transportation system management",
                                            mitigation_stratcat_t2 == "Composting and biological treatment of waste" ~ "Waste and water treatment practices",
                                            mitigation_stratcat_t2 == "Waste system management" ~ "Waste and water treatment practices",
                                            mitigation_stratcat_t2 == "Waste to energy, energy from waste" ~ "Waste and water treatment practices",
                                            mitigation_stratcat_t2 == "Water and wastewater system efficiencies" ~ "Waste and water treatment practices",
                                            (mitigation_stratcat_t2 == "Cogeneration, combined heat and power, tri-generation, combined heat, cooling, and power" & sector == "Industry") ~ "Industrial facility improvements",
                                            (mitigation_stratcat_t2 == "Electric Mobility (EV, HEV)" & sector == "Industry") ~ "Industrial facility improvements",
                                            (mitigation_stratcat_t2 == "Solar PV energy" & sector == "Industry") ~ "Industrial facility improvements",
                                            (mitigation_stratcat_t2 == "Fuel switching and fuel efficiency" & sector == "Industry") ~ "Industrial facility improvements",
                                            (mitigation_stratcat_t2 == "Waste heat recovery" & sector == "Industry") ~ "Industrial facility improvements"))

# Remove observations that could not be categorized (3 from one study) 
dataset_clean <- dataset_clean %>%
  filter(!is.na(mitigation_strategy_cat))

# FINAL MANUAL CLEANING 

# REMOVE ARTICLE: 39559 not subnational wide, uses sample of 50 households
dataset_clean$subnational_wide[dataset_clean$unique_id == "39559"] <- "No"
dataset_clean$synthesis_group[dataset_clean$unique_id == "39559"] <- "sg_2"

# Articles where population was pulled incorrectly
# 346836 - population of Piedade Brazil incorrectly set to 2. Update from Wikipedia
dataset_clean$population[(dataset_clean$unique_id == "346836" & dataset_clean$name == "Piedade")] <- 55542
# 226608
dataset_clean$population[(dataset_clean$unique_id == "226608" & dataset_clean$name == "Khulna")] <- 949229
dataset_clean$population[(dataset_clean$unique_id == "226608" & dataset_clean$name == "Rangpur")] <- 1031388
# 128045
dataset_clean$population[(dataset_clean$unique_id == "128045" & dataset_clean$name == "Chongqing")] <- 32054159
# 128249
dataset_clean$population[(dataset_clean$unique_id == "128249" & dataset_clean$name == "Tehran")] <- 9039000
# 99374
dataset_clean$population[(dataset_clean$unique_id == "99374" & dataset_clean$name == "Adelaide")] <- 1387290 
# 100129
dataset_clean$population[(dataset_clean$unique_id == "100129" & dataset_clean$name == "Sheffield")] <-556500 

# 93147 
dataset_clean$impact_time_frame[(dataset_clean$unique_id == "93147")] <-"2020-2050" 

# 206292 - incorrectly collected results for a national LCFS scenario rather than the CA scenario
dataset_clean <- dataset_clean %>%
  filter(!(unique_id == "206292" & emissions_reduction_impact == 394)) # only need one impact

dataset_clean$emissions_reduction_impact[dataset_clean$unique_id == "206292"] <- 26.9
dataset_clean$impact_time_period[dataset_clean$unique_id == "206292"] <- 2020
dataset_clean$intervention[dataset_clean$unique_id == "206292"] <- "Low carbon fuel policy, assuming optimistic technology adoption rates where lifecycle GHG emissions from transportation fuel use are reduced by 10% in 2020"

# 195004
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id == "195004" & dataset_clean$intervention == "Increased wind capacity (no nuclear, current transmission capacity, $100 carbon tax)")] <- 3.94

# 128045
dataset_clean$population[(dataset_clean$unique_id == "128045" & dataset_clean$name=="Guangdong")] <- 126012510
dataset_clean$subnational_context[(dataset_clean$unique_id == "128045" & dataset_clean$name=="Guangdong")] <- "Regional Government"
dataset_clean$subnational_context[(dataset_clean$unique_id == "128045" & dataset_clean$name=="Hubei")] <- "Regional Government"

# 179880 - study with multiple interventions - impacts for one intervention were recorded with the wrong units
# Re-recording these impacts to best align with the rest of the data
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Improved practices in fertilizer application for annuals and perennials" & dataset_clean$name=="Khuntil")] <- -0.51
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Improved practices in fertilizer application for annuals and perennials" & dataset_clean$name=="Magharvada")] <- 0.26
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Improved practices in fertilizer application for annuals and perennials" & dataset_clean$name=="Bhalot")] <- 0.07
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Improved practices in fertilizer application for annuals and perennials" & dataset_clean$name=="Bharu")] <- 0.06
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Improved practices in fertilizer application for annuals and perennials" & dataset_clean$name=="Sitara")] <- 0.06
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Improved practices in fertilizer application for annuals and perennials" & dataset_clean$name=="Purkhawas")] <- 0.06
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Improved practices in fertilizer application for annuals and perennials" & dataset_clean$name=="Chomakot")] <- 0.02

# Fixing other impacts from this study - perrenials had lower area compared to annuals
dataset_clean$study_subject_value[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Adoption of climate-resilient practices for perennial crops" & dataset_clean$name=="Khuntil")] <- 93
dataset_clean$study_subject_value[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Adoption of climate-resilient practices for perennial crops" & dataset_clean$name=="Purkhawas")] <- 15
dataset_clean$study_subject_value[(dataset_clean$unique_id == "179880" & dataset_clean$intervention == "Adoption of climate-resilient practices for perennial crops" & dataset_clean$name=="Bhalot")] <- 7.5

# 205460 - study used commas rather than decimals.
dataset_clean$emissions_reduction_impact[dataset_clean$unique_id == "205460"] <- 1269000 # (2.3848-1.1158)*1000000

# 110797 - results actually for a representative field and its unclear how many km or ha in IL are dedicated to bioenergy crops 
dataset_clean$subnational_wide[dataset_clean$unique_id == "110797"] <- "No"
dataset_clean$synthesis_group[dataset_clean$unique_id == "110797"] <- "sg_3"

# 137761 - units are tCO2/month so time frame should be 1/12
dataset_clean$impact_time_frame[dataset_clean$unique_id == "137761"] <- 1/12

# Exclude Scotland and Wales as subnationals - REMOVES 2 ARTICLES
dataset_clean <- dataset_clean %>%
  filter(name != "Wales") %>%
  filter(name != "Scotland") # Remove Wales and Scotland as subnationals

# Two observations missing synthesis group, these are subnational-wide
dataset_clean <- dataset_clean %>%
  mutate(synthesis_group = ifelse(is.na(synthesis_group), "sg_1", synthesis_group))

# 128045 - google docs formatting issue with the time period
dataset_clean$impact_time_period[dataset_clean$unique_id=="128045"] <- "2011-2015"

# Fix studies that were incorrectly marked as gov action or policy
dataset_clean <- dataset_clean %>%
  mutate(gov_action_or_policy = case_when(unique_id %in% c("161116", "139537", "99889", "110559", "178619", "172509", "142534") ~ "No",
                                          TRUE ~ gov_action_or_policy))

# 272101 - simpler calculation of impacts
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id=="272101" & dataset_clean$impact_time_period=="2020.0" & dataset_clean$intervention == "Current trend of building stock retrofitting with typical energy efficiency measures")] <- (12.4*0.014)
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id=="272101" & dataset_clean$impact_time_period=="2020.0" & dataset_clean$intervention == "Current trend of building stock retrofitting with cost-optimal measures based on EU framework")] <- (12.4*0.013)
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id=="272101" & dataset_clean$impact_time_period=="2030.0" & dataset_clean$intervention == "Current trend of building stock retrofitting with typical energy efficiency measures")] <- (12.4*0.043)
dataset_clean$emissions_reduction_impact[(dataset_clean$unique_id=="272101" & dataset_clean$impact_time_period=="2030.0" & dataset_clean$intervention == "Current trend of building stock retrofitting with cost-optimal measures based on EU framework")] <- (12.4*0.042)
dataset_clean$units[dataset_clean$unique_id=="272101"] <- "MtCO2/yr"
dataset_clean$synthesis_group[dataset_clean$unique_id=="272101"] <- "sg_1"

# 161116
dataset_clean$mitigation_stratcat_t1[dataset_clean$unique_id=="161116"& dataset_clean$intervention %in% c("Ground source heat pump (replacing electric furnace)",
                                                                                                          "Ground source heat pump (replacing NG furnace )",
                                                                                                          "Ground source heat pump for space cooling",
                                                                                                          "Ground source heat pump (replacing electric furnace)",
                                                                                                          "Ground source heat pump (replacing NG furnace )")] <- "Building energy and heat systems"
dataset_clean$mitigation_stratcat_t2[dataset_clean$unique_id=="161116"& dataset_clean$intervention %in% c("Ground source heat pump (replacing electric furnace)",
                                                                                                          "Ground source heat pump (replacing NG furnace )",
                                                                                                          "Ground source heat pump for space cooling",
                                                                                                          "Ground source heat pump (replacing electric furnace)",
                                                                                                          "Ground source heat pump (replacing NG furnace )")] <- "Geothermal heat pumps, ground source heat pumps"

dataset_clean$mitigation_stratcat_t1[dataset_clean$unique_id=="161116"& dataset_clean$intervention == "Waste heat recovery"] <- "Building energy and heat systems"
dataset_clean$mitigation_stratcat_t2[dataset_clean$unique_id=="161116"& dataset_clean$intervention == "Waste heat recovery"] <- "Waste heat recovery"

# 177676 - impacts are for Greater London and not just the City of Westminster
dataset_clean$name[dataset_clean$unique_id=="177676"] <- "London"
dataset_clean$population[dataset_clean$unique_id=="177676"] <- 8799800

# Fix study that should be marked as gov action or policy
dataset_clean <- dataset_clean %>%
  mutate(gov_action_or_policy = case_when((unique_id == "218967" & intervention %in% 
                                            c("S1.1 LDV efficiency/GHG standards, fleet efficiency to 40MPG by 2040",
                                              "S1.2 LDV ZEV deployment, 6% of fleet by 2025",
                                              "S1.3 LDV VMT reductions, 7.6% below S0 in 2020",
                                              "S1.4 HDV efficiency standards, HHD diesel ~7mpg, MHD gasoline ~11mpg, MHD diesel ~14mpg by 2020",
                                              "S1.5 LCFS target, 10% reduction in GHG emissions by 2020 (22% biofuel gasoline, 5% biofuel diesel)",
                                              "S1.6 IEPR building efficiency (includes AB1109, AB1470), 5-8% normalized reduction in NG and -1-+3% change in electricity in 2020", 
                                              "S1.7 Title 24 new buildings and retrofits, 10% better efficiency for new construction and retrofits, 0.3%/year retrofit rate",
                                              "S1.8 RPS 33% target",
                                              "S1.9 Imported coal phase-out",
                                              "S1.10 Once-through cooling phase-out (eliminates 15.6 GW of capacity by 2030)",
                                              "S1.11 20x20 water conservation, reduce residential and commercial water use 20% per capita in 2020",
                                              "S1.12 Landfill methane capture, reduce GHG emissions 1.5MtCO2e/year in 2020, equal to 10% reduction in gross emissions",
                                              "S1.13 Sustainable forests, sequestration of 5MtCO2e/yr in forests in 2020")) ~ "Yes",
                                          TRUE ~ gov_action_or_policy)) %>%
  mutate(policy_note = case_when((unique_id == "218967" & intervention %in% 
                                             c("S1.1 LDV efficiency/GHG standards, fleet efficiency to 40MPG by 2040",
                                               "S1.2 LDV ZEV deployment, 6% of fleet by 2025",
                                               "S1.3 LDV VMT reductions, 7.6% below S0 in 2020",
                                               "S1.4 HDV efficiency standards, HHD diesel ~7mpg, MHD gasoline ~11mpg, MHD diesel ~14mpg by 2020",
                                               "S1.5 LCFS target, 10% reduction in GHG emissions by 2020 (22% biofuel gasoline, 5% biofuel diesel)",
                                               "S1.6 IEPR building efficiency (includes AB1109, AB1470), 5-8% normalized reduction in NG and -1-+3% change in electricity in 2020", 
                                               "S1.7 Title 24 new buildings and retrofits, 10% better efficiency for new construction and retrofits, 0.3%/year retrofit rate",
                                               "S1.8 RPS 33% target",
                                               "S1.9 Imported coal phase-out",
                                               "S1.10 Once-through cooling phase-out (eliminates 15.6 GW of capacity by 2030)",
                                               "S1.11 20x20 water conservation, reduce residential and commercial water use 20% per capita in 2020",
                                               "S1.12 Landfill methane capture, reduce GHG emissions 1.5MtCO2e/year in 2020, equal to 10% reduction in gross emissions",
                                               "S1.13 Sustainable forests, sequestration of 5MtCO2e/yr in forests in 2020")) ~ "State policies listed in Table B1",
                                          TRUE ~ policy_note)) %>%
  mutate(policy_page = case_when((unique_id == "218967" & intervention %in% 
                                    c("S1.1 LDV efficiency/GHG standards, fleet efficiency to 40MPG by 2040",
                                      "S1.2 LDV ZEV deployment, 6% of fleet by 2025",
                                      "S1.3 LDV VMT reductions, 7.6% below S0 in 2020",
                                      "S1.4 HDV efficiency standards, HHD diesel ~7mpg, MHD gasoline ~11mpg, MHD diesel ~14mpg by 2020",
                                      "S1.5 LCFS target, 10% reduction in GHG emissions by 2020 (22% biofuel gasoline, 5% biofuel diesel)",
                                      "S1.6 IEPR building efficiency (includes AB1109, AB1470), 5-8% normalized reduction in NG and -1-+3% change in electricity in 2020", 
                                      "S1.7 Title 24 new buildings and retrofits, 10% better efficiency for new construction and retrofits, 0.3%/year retrofit rate",
                                      "S1.8 RPS 33% target",
                                      "S1.9 Imported coal phase-out",
                                      "S1.10 Once-through cooling phase-out (eliminates 15.6 GW of capacity by 2030)",
                                      "S1.11 20x20 water conservation, reduce residential and commercial water use 20% per capita in 2020",
                                      "S1.12 Landfill methane capture, reduce GHG emissions 1.5MtCO2e/year in 2020, equal to 10% reduction in gross emissions",
                                      "S1.13 Sustainable forests, sequestration of 5MtCO2e/yr in forests in 2020")) ~ "State policies listed in Table B1",
                                 TRUE ~ policy_page))

# Align subnational context
dataset_clean <- dataset_clean %>%
  mutate(subnational_context = ifelse(subnational_context=="Regional government", "Regional Government", subnational_context))

dataset_clean$subnational_context[dataset_clean$subnational_context == "City or Municipal Government" & dataset_clean$name=="British Columbia"] <- "Regional Government" 

# EXPORT
write.csv(dataset_clean, file = "../Data/Analysis/07_Combined_Data_Clean.csv",
          fileEncoding = "UTF-8", row.names = F)


