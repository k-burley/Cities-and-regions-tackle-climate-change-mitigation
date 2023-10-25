# ------------------------------------------------------------------------------
# Program Name: 11_Meta_Regression.R
# Program Purpose: Perform meta-regression  

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
library(ggrepel)
library(ggpubr)
library(scales)
library(boot)
library(mosaic)
library(rms)
library(metafor)
library(robumeta)
library(plm)
library(lmtest)
library(sandwich)
library(stats)
library(clubSandwich)
library(stargazer)

##### 1. Bring in Data #####
# Full Dataset
dataset_orig <- read.csv("../Data/Analysis/07_Combined_Data_Clean.csv",
                         header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

dataset_orig <- dataset_orig %>%
  mutate(global_south = case_when(region %in% c("Europe", "Eastern Europe and Central Asia", "North America") ~ 0,
                                  region %in% c("Middle East and North Africa", "Sub-Saharan Africa", "South Asia") ~ 1,
                                  country %in% c("Japan", "South Korea", "Mexico", "Australia") ~ 0,
                                  TRUE ~ 1)) %>%
  mutate(region_grp = case_when(region %in% c("Europe", "Eastern Europe and Central Asia") ~ "Europe and Central Asia",
                                region %in% c("East Asia and the Pacific", "South Asia") ~ "Asia and the Pacific",
                                region %in% c("Sub-Saharan Africa", "Middle East and North Africa") ~ "Africa and the Middle East",
                                region == "North America" ~ "North America",
                                region == "Latin America and Caribbean" ~ "Latin America and Caribbean")) %>%
  mutate(subnational_size = case_when(dplyr::between(population, 0,299999) ~ "Small",
                                      dplyr::between(population, 300000,999999) ~ "Medium",
                                      dplyr::between(population, 1000000,9999999) ~ "Large",
                                      population > 10000000 ~ "Mega")) %>%
  mutate(method_clean = case_when(methodology == "Field experiment" ~ "Experimental",
                                  methodology == "Experiment (pilot)" ~ "Experimental",
                                  methodology == "Experiment" ~ "Experimental",
                                  methodology == "Experiment/Pre-test Post-test control group design" ~ "Experimental",
                                  methodology == "Emission accounting" ~ "Emissions accounting",
                                  methodology == "Emissons accounting" ~ "Emissions accounting",
                                  methodology == "Lifecycle emissions accounting"~ "Emissions accounting",
                                  methodology == "Emissions accounting with survey" ~ "Emissions accounting",
                                  methodology == "Emissions accounting, calculations based on estimated parameters" ~ "Emissions accounting",
                                  methodology == "Emissions accounting, unclear" ~ "Emissions accounting",
                                  methodology == "CHP plant and DH network simulation, life cycle assessment and economic feasibility analysis" ~ "Energy systems modeling",
                                  methodology == "consequential life cycle assessment" ~"Life cycle assessment",
                                  methodology == "Electric, heating, and cooling potential applied to emissions factors" ~ "Emissions accounting",
                                  methodology == "Electricity consumption simulation" ~ "Building energy modeling",
                                  methodology == "Electricity generation estimation and emissions quantification" ~ "Energy systems modeling",
                                  methodology == "Electricity system simulation modeling" ~ "Energy systems modeling",
                                  methodology == "Energy and emissions simulation" ~ "Building energy modeling",
                                  methodology == "Energy and hot water demand simulation modeling" ~ "Building energy modeling",
                                  methodology == "energy consumption analysis" ~ "Building energy modeling",
                                  methodology == "Energy consumption analysis" ~ "Building energy modeling",
                                  methodology == "Energy flow modeling" ~ "Building energy modeling",
                                  methodology == "Energy simulation" ~ "Energy systems modeling",
                                  methodology == "Energy simulation and analysis" ~ "Building energy modeling",
                                  methodology == "Energy symbiosis systems with simulation" ~ "Building energy modeling",
                                  methodology == "Energy mix optimization" ~ "Energy systems modeling",
                                  methodology == "Energy systems optimization modeling" ~ "Energy systems modeling",
                                  methodology == "Energy systems simulation and optimization" ~ "Energy systems modeling",
                                  methodology == "Exergy analysis and energy systems modeling" ~ "Energy systems modeling",
                                  methodology == "Experimental reactor configuration and environmental impact assessment" ~ "Experimental",
                                  methodology == "GAMS" ~ "Energy systems modeling",
                                  methodology == "Heating energy consumption analysis" ~ "Building energy modeling",
                                  methodology == "LCA methodology" ~ "Life cycle assessment",
                                  methodology == "Life cycle CO2 analysis" ~ "Life cycle assessment",
                                  methodology == "Lifecycle assessment" ~"Life cycle assessment",
                                  methodology == "LEAP model" ~ "Energy systems modeling",
                                  methodology == "Linear programming optimization" ~ "Energy systems modeling",
                                  methodology == "Simulation analysis" ~ "Energy systems modeling",
                                  methodology %in% c("Simulation and optimization", "Simulation by agent-based model", "Simulation of avoided trips",
                                                     "Simulation of bus replacements", "Simulation of emissions from replaced buses", 
                                                     "Simulation of increasing recycling of organic waste", "Simulation of reducing undifferentiated waste to landfills",
                                                     "Simulation using GIS to determine suitable crop growth area, and life cycle analysis to determine potential emissions reductions impact") ~ "Simulation",
                                  methodology == "Simulation of emissions reductions from electricity usage from retrofitting of buildings and replacement of lightbulbs" ~ "Building energy modeling",
                                  methodology == "Simulation of emissions reductions from more efficient heating system" ~ "Energy systems modeling",
                                  methodology == "Simulation of increasing biogas recovery to produce electricity" ~ "Energy systems modeling",
                                  methodology == "Simulation of increasing installing solar PV panels to produce electricity" ~ "Emissions accounting",
                                  methodology == "Techno-economic energy system optimization" ~ "Energy systems modeling",
                                  methodology %in% c("Traffic demand modeling", "Transport demand forecast modeling and emissions factors", "Transportation energy systems modeling",
                                                     "Transportation modeling", "Transportation simulation modeling", "Travel demand modeling") ~ "Transportation modeling",
                                  methodology == "Whole building energy analysis" ~ "Building energy modeling",
                                  methodology == "Whole year transient modeling" ~ "Energy systems modeling",
                                  TRUE ~ methodology)) %>%
  mutate(method_clean = case_when(grepl("Simulation", methodology) & model %in% c("RETScreen", "LEAP Model", 
                                                                                  "A commercial software TRNSYS", 
                                                                                  "Alberta grid allocation model") ~ "Energy systems modeling",
                                  TRUE ~ method_clean))

# Standardized Dataset
std_data_orig <- read.csv("../Data/Analysis/08_Standardized_Impacts.csv",
                          header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

std_data_orig <- std_data_orig %>%
  mutate(global_south = case_when(region %in% c("Europe", "Eastern Europe and Central Asia", "North America") ~ 0,
                                  region %in% c("Middle East and North Africa", "Sub-Saharan Africa", "South Asia") ~ 1,
                                  country %in% c("Japan", "South Korea", "Mexico", "Australia") ~ 0,
                                  TRUE ~ 1)) %>%
  mutate(region_grp = case_when(region %in% c("Europe", "Eastern Europe and Central Asia") ~ "Europe and Central Asia",
                                region %in% c("East Asia and the Pacific", "South Asia") ~ "Asia and the Pacific",
                                region %in% c("Sub-Saharan Africa", "Middle East and North Africa") ~ "Africa and the Middle East",
                                region == "North America" ~ "North America",
                                region == "Latin America and Caribbean" ~ "Latin America and Caribbean")) %>%
  mutate(subnational_size = case_when(dplyr::between(population, 0,299999) ~ "Small",
                                      dplyr::between(population, 300000,999999) ~ "Medium",
                                      dplyr::between(population, 1000000,9999999) ~ "Large",
                                      population > 10000000 ~ "Mega")) %>%
  mutate(method_clean = case_when(methodology == "Field experiment" ~ "Experimental",
                                  methodology == "Experiment (pilot)" ~ "Experimental",
                                  methodology == "Experiment" ~ "Experimental",
                                  methodology == "Experiment/Pre-test Post-test control group design" ~ "Experimental",
                                  methodology == "Emission accounting" ~ "Emissions accounting",
                                  methodology == "Emissons accounting" ~ "Emissions accounting",
                                  methodology == "Lifecycle emissions accounting"~ "Emissions accounting",
                                  methodology == "Emissions accounting with survey" ~ "Emissions accounting",
                                  methodology == "Emissions accounting, calculations based on estimated parameters" ~ "Emissions accounting",
                                  methodology == "Emissions accounting, unclear" ~ "Emissions accounting",
                                  methodology == "CHP plant and DH network simulation, life cycle assessment and economic feasibility analysis" ~ "Energy systems modeling",
                                  methodology == "consequential life cycle assessment" ~"Life cycle assessment",
                                  methodology == "Electric, heating, and cooling potential applied to emissions factors" ~ "Emissions accounting",
                                  methodology == "Electricity consumption simulation" ~ "Building energy modeling",
                                  methodology == "Electricity generation estimation and emissions quantification" ~ "Energy systems modeling",
                                  methodology == "Electricity system simulation modeling" ~ "Energy systems modeling",
                                  methodology == "Energy and emissions simulation" ~ "Building energy modeling",
                                  methodology == "Energy and hot water demand simulation modeling" ~ "Building energy modeling",
                                  methodology == "energy consumption analysis" ~ "Building energy modeling",
                                  methodology == "Energy consumption analysis" ~ "Building energy modeling",
                                  methodology == "Energy flow modeling" ~ "Building energy modeling",
                                  methodology == "Energy simulation" ~ "Energy systems modeling",
                                  methodology == "Energy simulation and analysis" ~ "Building energy modeling",
                                  methodology == "Energy symbiosis systems with simulation" ~ "Building energy modeling",
                                  methodology == "Energy mix optimization" ~ "Energy systems modeling",
                                  methodology == "Energy systems optimization modeling" ~ "Energy systems modeling",
                                  methodology == "Energy systems simulation and optimization" ~ "Energy systems modeling",
                                  methodology == "Exergy analysis and energy systems modeling" ~ "Energy systems modeling",
                                  methodology == "Experimental reactor configuration and environmental impact assessment" ~ "Experimental",
                                  methodology == "GAMS" ~ "Energy systems modeling",
                                  methodology == "Heating energy consumption analysis" ~ "Buildinga energy modeling",
                                  methodology == "LCA methodology" ~ "Life cycle assessment",
                                  methodology == "Life cycle CO2 analysis" ~ "Life cycle assessment",
                                  methodology == "Lifecycle assessment" ~"Life cycle assessment",
                                  methodology == "LEAP model" ~ "Energy systems modeling",
                                  methodology == "Linear programming optimization" ~ "Energy systems modeling",
                                  methodology == "Simulation analysis" ~ "Energy systems modeling",
                                  methodology %in% c("Simulation and optimization", "Simulation by agent-based model", "Simulation of avoided trips",
                                                     "Simulation of bus replacements", "Simulation of emissions from replaced buses", 
                                                     "Simulation of increasing recycling of organic waste", "Simulation of reducing undifferentiated waste to landfills",
                                                     "Simulation using GIS to determine suitable crop growth area, and life cycle analysis to determine potential emissions reductions impact") ~ "Simulation",
                                  methodology == "Simulation of emissions reductions from electricity usage from retrofitting of buildings and replacement of lightbulbs" ~ "Building energy modeling",
                                  methodology == "Simulation of emissions reductions from more efficient heating system" ~ "Energy systems modeling",
                                  methodology == "Simulation of increasing biogas recovery to produce electricity" ~ "Energy systems modeling",
                                  methodology == "Simulation of increasing installing solar PV panels to produce electricity" ~ "Emissions accounting",
                                  methodology == "Techno-economic energy system optimization" ~ "Energy systems modeling",
                                  methodology %in% c("Traffic demand modeling", "Transport demand forecast modeling and emissions factors", "Transportation energy systems modeling",
                                                     "Transportation modeling", "Transportation simulation modeling", "Travel demand modeling") ~ "Transportation modeling",
                                  methodology == "Whole building energy analysis" ~ "Building energy modeling",
                                  methodology == "Whole year transient modeling" ~ "Energy systems modeling",
                                  TRUE ~ methodology)) %>%
  mutate(method_clean = case_when((grepl("Simulation", methodology) & model %in% c("RETScreen", "LEAP Model",
                                                                                   "A commercial software TRNSYS",
                                                                                   "Alberta grid allocation model")) ~ "Energy systems modeling",
                                  TRUE ~ method_clean)) %>%
  mutate(method_group = case_when(method_clean == "Emissions accounting" ~ "Emissions accounting",
                                  method_clean == "Life cycle assessment" ~ "Life cycle assessment",
                                  method_clean %in% c("Energy systems modeling", "Building energy modeling") ~ "Energy modeling",
                                  TRUE ~ "Other"))

##### Population and Emissions Data #####

### Source: World Emissions Clock - 2022
weclock_emissions <- read_excel("../Data/Original/Contextual Data/countries_worldemissionsclock_2022.xlsx",
                                col_names=TRUE)

weclock_emissions <- weclock_emissions %>%
  gather(variable, weclock_tons_co2e, ghg_ag:ghg_tr) %>%
  mutate(sector = case_when(variable == "ghg_ag" ~ "Agriculture and forestry",
                            variable == "ghg_bg" ~ "Buildings",
                            variable == "ghg_eh" ~ "Electricity and heat",
                            variable == "ghg_in" ~ "Industry",
                            variable == "ghg_tr" ~ "Transport"))

### Source: Climate Watch- 2019
# AFOLU
cw_afolu <- read.csv("../Data/Original/Contextual Data/ghg-emissions_afolu_total.csv",
                     header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))
cw_afolu <- cw_afolu %>%
  select(Country.Region, unit, X2019) %>%
  mutate(sector = "Agriculture and forestry") %>%
  rename(country = Country.Region) %>%
  mutate(country = case_when(country == "United States" ~ "United States of America",
                             country == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
                             TRUE ~ country)) %>%
  filter(country %in% unique(dataset_orig$country)) # Taiwan and Hong Kong not available

# Buildings
cw_bldg <- read.csv("../Data/Original/Contextual Data/ghg-emissions_bldg_total.csv",
                     header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))
cw_bldg <- cw_bldg %>%
  select(Country.Region, unit, X2019) %>%
  mutate(sector = "Buildings") %>%
  rename(country = Country.Region) %>%
  mutate(country = case_when(country == "United States" ~ "United States of America",
                             country == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
                             TRUE ~ country)) %>%
  filter(country %in% unique(dataset_orig$country)) # Taiwan and Hong Kong not available

# Electricity and Heat
cw_elec <- read.csv("../Data/Original/Contextual Data/ghg-emissions_elec_total.csv",
                    header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))
cw_elec <- cw_elec %>%
  select(Country.Region, unit, X2019) %>%
  mutate(sector = "Electricity and heat") %>%
  rename(country = Country.Region) %>%
  mutate(country = case_when(country == "United States" ~ "United States of America",
                             country == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
                             TRUE ~ country)) %>%
  filter(country %in% unique(dataset_orig$country)) # Taiwan and Hong Kong not available

# Industry
cw_ind <- read.csv("../Data/Original/Contextual Data/ghg-emissions_ind_total.csv",
                    header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))
cw_ind <- cw_ind %>%
  select(Country.Region, unit, X2019) %>%
  mutate(sector = "Industry") %>%
  rename(country = Country.Region) %>%
  mutate(country = case_when(country == "United States" ~ "United States of America",
                             country == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
                             TRUE ~ country)) %>%
  filter(country %in% unique(dataset_orig$country)) # Taiwan and Hong Kong not available

# Transportation
cw_trans <- read.csv("../Data/Original/Contextual Data/ghg-emissions_trans_total.csv",
                   header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))
cw_trans <- cw_trans %>%
  select(Country.Region, unit, X2019) %>%
  mutate(sector = "Transport") %>%
  rename(country = Country.Region) %>%
  mutate(country = case_when(country == "United States" ~ "United States of America",
                             country == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
                             TRUE ~ country)) %>%
  filter(country %in% unique(dataset_orig$country)) # Taiwan and Hong Kong not available

# Waste
cw_waste <- read.csv("../Data/Original/Contextual Data/ghg-emissions_waste_total.csv",
                     header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))
cw_waste <- cw_waste %>%
  select(Country.Region, unit, X2019) %>%
  mutate(sector = "Waste") %>%
  rename(country = Country.Region) %>%
  mutate(country = case_when(country == "United States" ~ "United States of America",
                             country == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
                             TRUE ~ country)) %>%
  filter(country %in% unique(dataset_orig$country)) # Taiwan and Hong Kong not available

# Combine
cw_emissions <- cw_afolu %>%
  rbind(cw_bldg) %>%
  rbind(cw_elec) %>%
  rbind(cw_ind) %>% 
  rbind(cw_trans) %>%
  rbind(cw_waste) %>%
  rename(cw_mtco2e = X2019) %>%
  mutate(cw_mtco2e = as.numeric(cw_mtco2e))

rm(cw_afolu, cw_bldg, cw_elec, cw_ind, cw_trans, cw_waste)

### Population (Source: World Bank)
population <- read.csv("../Data/Original/Contextual Data/API_SP.POP.TOTL_DS2_en_csv_v2_5182108.csv",
                       header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"), skip=4)

population <- population %>%
  select(Country.Code, X2019, X2021) %>%
  filter(Country.Code %in% unique(dataset_orig$iso)) %>%
  rename(iso = Country.Code)

# Create Dataset for Meta-Regression
std_data_forreg <- std_data_orig %>%
  left_join(weclock_emissions[c("iso", "sector", "weclock_tons_co2e")], by=c("sector","iso")) %>%
  left_join(cw_emissions[c("country", "sector", "cw_mtco2e")], by=c("sector","country")) %>%
  left_join(population, by="iso") %>%
  mutate(X2021 = ifelse(iso=="TWN", 23894394, X2021)) %>% # don't need 2019 bc Taiwan is not in CW data
  mutate(sector_orig = sector) %>%
  mutate(weclock_pc = weclock_tons_co2e/X2021,
         cw_pc = (cw_mtco2e*1000000)/X2019) %>% # since data from 2019
  filter(mitigation_stratcat_t1!="Industrial facility improvements") %>%
  mutate(years_projected = impact_time_period_end-year,
         years_projected = ifelse(is.na(years_projected),5,years_projected), # Unclear time frame equated to projecting 5 years in advance
         years_projected = case_when(years_projected<0 ~ 0, # truncate so that we don't have negative years
                                     TRUE ~ years_projected)) %>%
  mutate(sector = ifelse(mitigation_stratcat_t1 %in% c("Emissions trading, cap-and-trade, carbon tax", "Circular economy, industrial symbiosis","Land use and development"), "Cross-Sectoral", sector)) %>%
  mutate(region_grp = relevel(as.factor(region_grp), ref="North America"))

#### 2. Estimate Mega-Regressions ####

# Function to Calculate Clustered Standard Errors
se_cluster <- function(x) {
  coeftest(x, vcov = vcovCR(x, cluster=std_data_forreg$unique_id, type="CR1"))[, "Std. Error"] 
}

# Estimate Models
mdls_we_cw_comp <- list(
  m1 = plm(er_impact_tco2_annual_pc ~ impact_type + subnational_size + subnational_context + region_grp + method_group,
           data = std_data_forreg,
           effect = "individual",
           model = "within",
           index = "mitigation_stratcat_t1"),
  m2 = plm(er_impact_tco2_annual_pc ~ impact_type + subnational_size + subnational_context + global_south + method_group,
           data = std_data_forreg,
           effect = "individual",
           model = "within",
           index = "mitigation_stratcat_t1"),
  m3 = plm(er_impact_tco2_annual_pc ~ years_projected + subnational_size + subnational_context + region_grp + method_group,
           data = std_data_forreg,
           effect = "individual",
           model = "within",
           index = "mitigation_stratcat_t1"),
  m4 = plm(er_impact_tco2_annual_pc ~ years_projected + subnational_size + subnational_context + global_south + method_group,
           data = std_data_forreg,
           effect = "individual",
           model = "within",
           index = "mitigation_stratcat_t1"),
  m5 = plm(er_impact_tco2_annual_pc ~ years_projected + subnational_size + subnational_context + region_grp + method_group + weclock_pc,
           data = std_data_forreg,
           effect = "individual",
           model = "within",
           index = "mitigation_stratcat_t1"),
  m6 = plm(er_impact_tco2_annual_pc ~ years_projected + subnational_size + subnational_context + region_grp + method_group + cw_mtco2e,
           data = std_data_forreg,
           effect = "individual",
           model = "within",
           index = "mitigation_stratcat_t1"),
  m7 = plm(er_impact_tco2_annual_pc ~ years_projected + subnational_size + subnational_context + region_grp + method_group + cw_pc,
           data = std_data_forreg,
           effect = "individual",
           model = "within",
           index = "mitigation_stratcat_t1")
)

# Table for Models Across All Sectors with Cluster Errors
# stargazer(
#   mdls_we_cw_comp, type = "html", single.row = TRUE, report = "vc*p", 
#   column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 3 + WE Clock", "Model 3 + CW", "Model 3 + PC-CW"),
#   se = lapply(mdls_we_cw_comp, se_cluster), out = "../Data/Results/models_we_cw_comp.html"
# )

##### 3. Scatter Plots to Investigate Associations ####
ggplot(std_data_forreg, aes(x=er_impact_tco2_annual_pc, y=weclock_tons_co2e, color=sector)) +
  geom_point() +
  geom_smooth(method=lm,  linetype="dashed",
              color="navy", fill="blue")

# WE Clock PC X WE Clock Total
ggplot(std_data_forreg, aes(x=weclock_tons_co2e, y=weclock_pc, color=sector)) +
  geom_point() +
  geom_smooth(method=lm,  linetype="dashed",
              color="navy", fill="blue")

# WE Clock X ER Impacts
ggplot(std_data_forreg, aes(x=weclock_pc, y=er_impact_tco2_annual_pc, color=region_grp)) +
  geom_point() +
  geom_smooth(method=lm,  linetype="dashed",
              color="navy", fill="blue")

# North America vs. Asia
ggplot() +
  geom_point(std_data_forreg[std_data_forreg$region_grp=="Asia and the Pacific",], mapping=aes(x=weclock_pc, y=er_impact_tco2_annual_pc), color="blue") +
  geom_smooth(std_data_forreg[std_data_forreg$region_grp=="Asia and the Pacific",], mapping=aes(x=weclock_pc, y=er_impact_tco2_annual_pc), 
              method=lm,  linetype="dashed",
              color="navy", fill="blue") +
  geom_point(std_data_forreg[std_data_forreg$region_grp=="North America",], mapping=aes(x=weclock_pc, y=er_impact_tco2_annual_pc), color="red") +
  geom_smooth(std_data_forreg[std_data_forreg$region_grp=="North America",], mapping=aes(x=weclock_pc, y=er_impact_tco2_annual_pc), 
              method=lm,  linetype="dashed",
              color="darkred", fill="red")  


ggplot(std_data_forreg, mapping=aes(x=sector_orig, y=cw_pc)) +
  geom_boxplot(aes(color=sector_orig)) +
  coord_flip()

ggplot(std_data_forreg, mapping=aes(x=sector_orig, y=weclock_pc)) +
  geom_boxplot(aes(color=sector_orig)) +
  coord_flip()
