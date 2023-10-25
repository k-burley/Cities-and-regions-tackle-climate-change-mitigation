# ------------------------------------------------------------------------------
# Program Name: 10_Descriptive_Analysis.R
# Program Purpose: Descriptive analysis and figures, based on full dataset

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
# library(ClimActor)
# install.packages("pdftools")
library("pdftools")
library(stringr)
# install.packages("tokenizers")
library(tokenizers)
library(ClimActor)

library(rworldmap)
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

library(maps)

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
std_data_orig <- read.csv("../Data/Analysis/11_Standardized_Impacts_022723.csv",
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


##### 2. Descriptive Analysis & Visuals #####


# How many unique actors?
length(unique(paste(dataset_orig$name, dataset_orig$iso, dataset_orig$subnational_context)))

table(dataset_orig$subnational_context)

table(dataset_orig$region)

region_sum <- dataset_orig %>%
  count(region) %>%
  mutate(total_obs=nrow(dataset_orig),
         pct = n/total_obs*100)

ggplot(region_sum, aes(x = n, y=reorder(region, n), fill=region)) +
  geom_bar(stat="identity") + # , fill = sector
  geom_text(aes(label = n),
            hjust = 0, nudge_x = .5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Sector") +
  xlab("Observations") +
  labs(title="Full Dataset")

# What are the study types?
flagged <- read.csv("C../Data/Analysis_Data/02_Flagged_Only.csv",
                    header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))
flagged <- flagged %>%
  filter(unique_id %in% dataset_orig$unique_id) %>%
  mutate(Document.Type = case_when(Document.Type == "J" ~ "Article",
                                   Document.Type == "C" ~ "Conference Paper",
                                   TRUE ~ Document.Type))
  

table(flagged$Document.Type)

# How many studies does each strat cat appear in?
studies_by_strat <- dataset_orig %>%
  distinct(unique_id, mitigation_stratcat_t1) %>%
  group_by(mitigation_stratcat_t1) %>%
  summarise(total_studies = n())

synth_1_summ <- std_data_orig %>%
  mutate(sector = ifelse(mitigation_stratcat_t1 %in% c("Emissions trading, cap-and-trade, carbon tax", "Circular economy, industrial symbiosis","Land use and development"), "Cross-Sectoral", sector)) %>%
  mutate(sector = ifelse(sector == "Agriculture and forestry", "AFOLU", sector)) %>%
  group_by(unique_id, mitigation_stratcat_t1, sector) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(mitigation_stratcat_t1, sector) %>%
  summarise(total_studies_std = n()) %>%
  left_join(studies_by_strat, by="mitigation_stratcat_t1")

t2_studies <- dataset_orig %>%
  distinct(unique_id, mitigation_stratcat_t1, mitigation_stratcat_t2, sector) %>% 
  count(mitigation_stratcat_t1, mitigation_stratcat_t2, sector)

# Basic Stats
nrow(dataset_orig) # Observations
length(unique(dataset_orig$unique_id)) # Studies
actors <- dataset_orig %>%
  distinct(name, iso, subnational_context, subnational_name, subnational_country)

# T1 Strategies
sector_sum <- dataset_orig %>%
  count(sector) %>%
  mutate(total_obs=nrow(dataset_orig),
         pct = n/total_obs*100)

sector_count <- ggplot(sector_sum, aes(x = n, y=reorder(sector, n), fill=sector)) +
  geom_bar(stat="identity") + # , fill = sector
  geom_text(aes(label = n),
            hjust = 0, nudge_x = .5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Sector") +
  xlab("Observations") +
  labs(title="Full Dataset")

sector_sum$ymax = cumsum(sector_sum$pct) # Compute the cumulative percentages (top of each rectangle)
sector_sum$ymin = c(0, head(sector_sum$ymax, n=-1)) # Compute the bottom of each rectangle
sector_sum$labelPosition <- (sector_sum$ymax + sector_sum$ymin) / 2 # Set label position

sector_donut <- ggplot(sector_sum, aes(x=pct, ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = sector)) +
  geom_rect() +
  coord_polar(theta="y") + # Makes it a pie plot
  xlim(c(2, 4)) + # Makes it a doughnut plot
  theme_void() +
  geom_text(x=4.25, aes(y=labelPosition, label=format(round(pct, 1), nsmall = 1)), size=3.6) +
  guides(fill = guide_legend(title = "Sector")) +
  labs(title="% of Total Observations") +
  theme(plot.title=element_text(hjust=0.5))

sector_donut

sector_count

# T1 Strategies
t1_sum <- dataset_orig %>%
  mutate(mitigation_stratcat_t1 = ifelse(mitigation_stratcat_t1=="Circular economy, industrial symbiosis, use of recycled materials",
                                         "Circular economy, industrial symbiosis",
                                         mitigation_stratcat_t1)) %>% # Make name shorter for plots 
  mutate(sector = case_when(mitigation_stratcat_t1 == "Circular economy, industrial symbiosis" ~ "Cross-Sectoral",
                            mitigation_stratcat_t1 == "Emissions trading, cap-and-trade, carbon tax" ~ "Cross-Sectoral",
                            TRUE ~ sector)) %>%
  count(mitigation_stratcat_t1, sector) # 

t1_count <- ggplot(t1_sum, aes(x = n, y=reorder(mitigation_stratcat_t1, n), fill=sector)) +
  geom_bar(stat="identity") + # , fill = sector
  geom_text(aes(label = n),
            hjust = 0, nudge_x = .5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Mitigation Strategy (Tier 1)") +
  xlab("Observations") +
  labs(title="Full Dataset")

t1_count

# T1 Strategies by City
t1_sum <- dataset_orig %>%
  mutate(mitigation_stratcat_t1 = ifelse(mitigation_stratcat_t1=="Circular economy, industrial symbiosis, use of recycled materials",
                                         "Circular economy, industrial symbiosis",
                                         mitigation_stratcat_t1)) %>% # Make name shorter for plots 
  mutate(sector = case_when(mitigation_stratcat_t1 == "Circular economy, industrial symbiosis" ~ "Cross-Sectoral",
                            mitigation_stratcat_t1 == "Emissions trading, cap-and-trade, carbon tax" ~ "Cross-Sectoral",
                            TRUE ~ sector)) %>%
  count(mitigation_stratcat_t1, sector, subnational_context)  

t1_count_city <- ggplot(t1_sum[t1_sum$subnational_context=="City or Municipal Government",], aes(x = n, y=reorder(mitigation_stratcat_t1, n), fill=sector)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label = n),
            hjust = 0, nudge_x = .5, size=2.5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Mitigation Strategy") +
  xlab("Observations") +
  labs(title="City or Municipality (n=779)") +
  theme(legend.position = "none",
        axis.title =element_text(size=8),
        axis.text =element_text(size=7)) 

t1_count_city

# T1 Strategies by Region
t1_count_region <- ggplot(t1_sum[t1_sum$subnational_context=="Regional Government",], aes(x = n, y=reorder(mitigation_stratcat_t1, n), fill=sector)) +
  geom_bar(stat="identity") + # , fill = sector
  geom_text(aes(label = n),
            hjust = 0, nudge_x = .5, size=2) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Mitigation Strategy") +
  xlab("Observations") +
  labs(title="Region (n=634)") +
  theme(legend.position = "right",
        axis.title.y =element_blank(),
        axis.text =element_text(size=7)) 

t1_count_region

# Combine
ggarrange(t1_count_city, t1_count_region, widths=c(45,55))

# T2 Strategies
t2_sum <- dataset_orig %>%
  count(mitigation_stratcat_t2, sector) %>% # , subnational_context
  group_by(mitigation_stratcat_t2) %>%
  mutate(strat_total = sum(n)) %>%
  ungroup()

t2_count <- ggplot(t2_sum, aes(x = n, y=reorder(mitigation_stratcat_t2, strat_total), fill=sector)) +
  geom_bar(stat="identity") + # , fill = sector
  # geom_text(aes(label = strat_total),
  #           hjust = 0, nudge_x = .5) + # Figure out bar labels
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Mitigation Strategy (Tier 2)") +
  xlab("Observations") +
  labs(title="Full Dataset")

t2_count

# Methodology
method_sum <- dataset_orig %>%
  group_by(unique_id, methodology) %>%
  summarise(n = n()) %>%
  group_by(methodology) %>%
  summarise(total_studies = n()) # , subnational_context

dataset_orig <- dataset_orig %>%
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
  mutate(method_clean = case_when(grepl("Simulation", dataset_orig$methodology) & dataset_orig$model %in% c("RETScreen", "LEAP Model", 
                                                                                                             "A commercial software TRNSYS", 
                                                                                                             "Alberta grid allocation model") ~ "Energy systems modeling",
                                  TRUE ~ method_clean))

method_sum2 <- dataset_orig %>%
  group_by(unique_id, method_clean) %>%
  summarise(n = n()) %>%
  group_by(method_clean) %>%
  summarise(total_studies = n()) %>%
  ungroup() %>%
  mutate(all_studies = sum(total_studies)) 

errors_sum <- dataset_orig %>%
  group_by(unique_id, any_error_metrics) %>%
  summarise(n = n()) %>%
  group_by(any_error_metrics) %>%
  summarise(total_studies = n()) %>%
  ungroup() %>%
  mutate(all_studies = sum(total_studies)) 


# Negative Reductions 
negative <- dataset_orig %>%
  mutate(obs = 1) %>%
  group_by(mitigation_stratcat_t2) %>%
  mutate(strat_total = sum(obs)) %>%
  ungroup() %>%
  filter(emissions_reduction_impact < 0) %>%
  count(mitigation_stratcat_t2, strat_total) %>%
  mutate(pct = (n/strat_total)*100) %>%
  mutate(mitigation_stratcat_t2 = case_when(mitigation_stratcat_t2 == "Cogeneration, combined heat and power, tri-generation, combined heat, cooling, and power" ~ "Cogeneration, combined heat and power...",
                                            TRUE ~ mitigation_stratcat_t2))

negative_count <- ggplot(negative, aes(x = n, y=reorder(mitigation_stratcat_t2, n))) +
  geom_bar(stat="identity", fill="dodgerblue4") +
  geom_text(aes(label = n),
            hjust = 0.4, nudge_x = .5, size=3.5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Mitigation Strategy (Tier 2)") +
  xlab("Observations") +
  labs(title="# of Observations") +
  theme(
        axis.text =element_text(size=9)
        ) 

negative_count

negative_pct <- ggplot(negative, aes(x = pct, y=reorder(mitigation_stratcat_t2, n))) +
  geom_bar(stat="identity", fill="tomato") +
  geom_text(aes(label = format(round(pct, 1), nsmall = 1)),
            hjust = 0.3, nudge_x = .5, size=3.5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Mitigation Strategy (Tier 2)") +
  xlab("Percent (%)") +
  labs(title="% of Total Observations") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

negative_pct

ggarrange(negative_count, negative_pct, widths=c(1.5,1))

negative_check <- dataset_orig %>%
  filter(emissions_reduction_impact < 0)

# Most Common Countries
country_sum <- dataset_orig %>%
  distinct(unique_id, iso) %>%
  count(iso)

##### 3. Test Vars for Meta-Regression #####
# Strategy Category
std_data_orig <- std_data_orig %>%
  group_by(mitigation_stratcat_t1) %>%
  mutate(median = median(er_impact_tco2_annual_pc)) %>%
  ungroup()
  
ggplot(data = std_data_orig, aes(x=reorder(mitigation_stratcat_t1, median), y=er_impact_tco2_annual_pc)) +
  geom_boxplot() + # outlier.shape=="NA"
  coord_flip()

ggplot(data = std_data_orig[std_data_orig$mitigation_stratcat_t1=="Agriculture and farm practices",], 
       aes(x=er_impact_tco2_annual_pc)) +
  geom_histogram()

# What about T2 strategies?
ggplot(data = std_data_orig[std_data_orig$mitigation_stratcat_t1=="Clean energy generation",], 
       aes(x=mitigation_stratcat_t2, y=er_impact_tco2_annual_pc)) +
  geom_boxplot() + 
  coord_flip()
table(std_data_orig$mitigation_stratcat_t2[std_data_orig$mitigation_stratcat_t1=="Clean energy generation"])


ggplot(data = std_data_orig[std_data_orig$mitigation_stratcat_t1=="Energy system operations",], 
       aes(x=mitigation_stratcat_t2, y=er_impact_tco2_annual_pc)) +
  geom_boxplot() + 
  coord_flip()

table(std_data_orig$mitigation_stratcat_t2[std_data_orig$mitigation_stratcat_t1=="Energy system operations"])

ggplot(data = std_data_orig[std_data_orig$mitigation_stratcat_t1=="Clean vehicle transportation",], 
       aes(x=mitigation_stratcat_t2, y=er_impact_tco2_annual_pc)) +
  geom_boxplot() + 
  coord_flip()

ggplot(data = std_data_orig[std_data_orig$mitigation_stratcat_t1=="Land use and development",], 
       aes(x=mitigation_stratcat_t2, y=er_impact_tco2_annual_pc)) +
  geom_boxplot() + 
  coord_flip()

# Test out distributions by study
test <- std_data_orig %>%
  group_by(mitigation_stratcat_t1, unique_id) %>%
  summarise(obs = n(),
            mn_er = mean(er_impact_tco2_annual_pc))

ggplot(data = test, aes(x=mitigation_stratcat_t1, y=mn_er)) +
  geom_boxplot() + # outlier.shape=="NA"
  coord_flip()

ggplot(data = test[test$mitigation_stratcat_t1=="Energy system operations",], 
       aes(x=mn_er)) +
  geom_histogram()
  
table(std_data_orig$mitigation_stratcat_t1)

# Context
ggplot(data = std_data_orig, aes(x=subnational_context, y=er_impact_tco2_annual_pc)) +
  geom_boxplot()
table(std_data_orig$subnational_context)

# Impact Type
ggplot(data = std_data_orig, aes(x=impact_type, y=er_impact_tco2_annual_pc)) +
  geom_boxplot()
table(std_data_orig$impact_type)

# Global Region
ggplot(data = std_data_orig, aes(x=region, y=er_impact_tco2_annual_pc)) +
  geom_boxplot() +
  coord_flip()

table(std_data_orig$region)
  # Combine into larger breakouts?
  # Global north vs. global south?

# Global North vs. Global South
ggplot(data = std_data_orig, aes(x=as.factor(global_south), y=er_impact_tco2_annual_pc)) +
  geom_boxplot() +
  coord_flip()

# Subnational Size
ggplot(data = std_data_orig, aes(x=subnational_size, y=er_impact_tco2_annual_pc)) +
  geom_boxplot()

table(std_data_orig$subnational_size)

# % of Achieved Impacts by Strategy?
achieved_pct <- dataset_orig %>%
  mutate(achieved_flag = case_when(impact_type == "Achieved/Ex Post" ~ 1,
                                   impact_type == "Potential/Ex Ante" ~ 0)) %>%
  group_by(mitigation_stratcat_t1) %>%
  summarise(total = n(),
            achieved = sum(achieved_flag)) %>%
  mutate(pct = (achieved/total)*100)

achieved_pct <- std_data_orig %>%
  # distinct(unique_id, mitigation_stratcat_t1, impact_type) %>%
  mutate(achieved_flag = case_when(impact_type == "Achieved/Ex Post" ~ 1,
                                   impact_type == "Potential/Ex Ante" ~ 0)) %>%
  group_by(mitigation_stratcat_t1) %>%
  summarise(total = n(),
            achieved = sum(achieved_flag)) %>%
  mutate(pct = (achieved/total)*100)
# Methodology? Would to group these as well

boxplot_comp <- ggplot(std_data_orig, mapping=aes(x=mitigation_stratcat_t1, y=er_impact_tco2_annual_pc, fill=impact_type)) +
  geom_boxplot() +
  coord_flip() + 
  theme_bw() +
  ggtitle("Impact Distribution") +
  xlab("Mitigation Strategy Category") +
  ylab("Effect Size (tCO2/year/capita)") +
  labs(fill="Impact Type") +
  ggtitle("Impact Distribution") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=7),
        legend.position = c(0.8, 0.2),
        legend.key.size = unit(0.2, "cm"))

boxplot_comp

achieved_sum <- std_data_orig %>%
  group_by(mitigation_stratcat_t1, impact_type) %>%
  summarise(mean = mean(er_impact_tco2_annual_pc),
            max = max(er_impact_tco2_annual_pc),
            min = min(er_impact_tco2_annual_pc),
            count = n()) %>%
  mutate(mitigation_stratcat_t1 = factor(mitigation_stratcat_t1,
                                         levels = c("Emissions trading, cap-and-trade, carbon tax",
                                                    "Alternative transportation modes",
                                                    "Circular economy, industrial symbiosis",
                                                    "Waste and water treatment practices",
                                                    "Land use and development",
                                                    "Building construction and improvement",
                                                    "Agriculture and farm practices",
                                                    "Energy system operations",
                                                    "Building energy and heat systems",
                                                    "Clean energy generation",
                                                    "Clean vehicle transportation",
                                                    "Industrial facility improvements",
                                                    "Transportation system management")))


range <- ggplot(achieved_sum[achieved_sum$mitigation_stratcat_t1!="Industrial facility improvements",], mapping=aes(x=mitigation_stratcat_t1, y=mean, color=impact_type)) + 
  geom_point(position=position_dodge(width=1)) +
  coord_flip() +
  theme_bw() +
  geom_crossbar(aes(ymin=min, ymax=max), position=position_dodge(width=1), fatten=2) +
  xlab("Mitigation Strategy Category") +
  ylab("Effect Size (tCO2/year/capita)") +
  theme(axis.title =element_text(size=12),
        axis.text =element_text(size=10),
        legend.position = "none") # turn off legend for combo plot

range

obs <- ggplot(achieved_sum[achieved_sum$mitigation_stratcat_t1!="Industrial facility improvements",], aes(x = count, y=mitigation_stratcat_t1, fill=impact_type)) +
  geom_bar(stat="identity", position="dodge") + # , fill = sector
  geom_text(aes(label = count, group=impact_type), position = position_dodge(width = .9), hjust = 0, size=3) + 
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Mitigation Strategy") +
  xlab("Observations") +
  theme(legend.position = "right",
        axis.title.y =element_blank(),
        axis.text.y =element_blank(),
        axis.text =element_text(size=7)) 
obs

ggarrange(range, obs)
  
# Test ClimActor
test <- dataset_orig %>%
  distinct(subnational_name, subnational_context, subnational_country) %>%
  mutate(entity_type = case_when(subnational_context=="City or Municipal Government" ~ "City",
                                 TRUE ~ "Region")) %>%
  rename("name" = "subnational_name",
         "country" = "subnational_country")

test <- clean_country_iso(test, country_dict)
test <- fuzzify_country(test, country_dict)
test <- contextualize_data(test, contextuals)


##### 4. Instruments Heat Table #####
instruments_table <- read_excel("../Data/Original/instruments_table_forimport2.xlsx") # From actions analysis, manually formatted

ins_table_long <- instruments_table %>%
  rename("Instrument_Type" = "Sector") %>%
  pivot_longer(cols = "AFOLU (1%)":"Cross Sectoral (17%)", names_to = "Sector", values_to = "Count")

ggplot(ins_table_long, aes(Sector, Instrument_Type)) +
  geom_tile(aes(fill = Count)) + 
  scale_fill_gradient(low = "white", high = "navyblue") +
  ylab("Policy Instrument Type") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Strategies Heat Map
strategies_table <- dataset_orig %>%
  mutate(sector_final = case_when(mitigation_stratcat_t1 %in% c("Emissions trading, cap-and-trade, carbon tax", 
                                                                "Circular economy, industrial symbiosis, use of recycled materials",
                                                                "Land use and development") ~ "Cross-Sectoral",
                                  TRUE ~ sector)) %>%
  count(region_grp, sector_final)

ggplot(strategies_table, aes(region_grp, sector_final)) +
  geom_tile(aes(fill = n)) + 
  scale_fill_gradient(low = "white", high = "cornflowerblue") +
  xlab("Global Region") +
  ylab("Sector") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

##### 5. Map #####

latlng <- read_excel("../Data/Original/Missing_Lat_Lng.xlsx")

latlng <- latlng %>%
  rename(lat2 = lat,
         lng2 = lng) %>%
  distinct(name, entity_type, iso, lat2, lng2) %>%
  filter(!(name=="Emilia-Romagna" & lng2=="10.956944"))

dataset_orig <- dataset_orig %>%
  left_join(latlng, by=c("name", "entity_type", "iso")) %>%
  mutate(lat = case_when(is.na(lat) ~ lat2,
                         TRUE ~ lat)) %>%
  mutate(lng = case_when(is.na(lng) ~ lng2,
                         TRUE ~ lng)) %>%
  select(-c(lat2, lng2))

# World Map Data
world <- map_data("world")

locations <- dataset_orig %>%
  count(name, country, iso, lat, lng, entity_type, subnational_context) %>%
  mutate(region = country) %>%
  mutate(region = recode(region, "United States of America" = "USA", "United Kingdom" = "UK"))

# What are the most common actors?
locations_by_study <- dataset_orig %>%
  count(unique_id, name, iso, subnational_context) %>%
  group_by(name, iso, subnational_context) %>%
  summarise(total_studies = n(),
            total_obs = sum(n))

length(locations_by_study$name[locations_by_study$total_studies>1])

world_subset <- inner_join(world, locations, by="region")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
  legend.box = "vertical"
)

world_plot <- ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), color="white", fill="gray70") +
  coord_fixed(1.3) +
  geom_point(data=locations, aes(x=lng, y=lat, color=subnational_context), shape=16, size=2.5, alpha=1) +
  scale_color_manual(name="Subnational Context",values=c("tomato","dodgerblue4")) +
  ggtitle("Cities and regions appearing in studies") +
  plain

world_plot

