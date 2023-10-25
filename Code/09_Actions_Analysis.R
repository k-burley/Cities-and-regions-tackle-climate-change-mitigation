# ------------------------------------------------------------------------------
# Program Name: 09_Actions_Analysis.R
# Program Purpose: Identify and analyze government actions

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

library(boot)
library(mosaic)
library(rms)
library(metafor)
library(robumeta)

##### 1. Bring in Data #####
dataset_orig <- read.csv("../Data/Analysis/07_Combined_Data_Clean.csv",
                    header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

# Fill in Lat/Lng where necessary
latlng <- read_excel("../Data/Original/Contextual Data/Missing_Lat_Lng.xlsx")

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

# Subset to gov policies and actions
dataset <- dataset_orig %>%
  filter(gov_action_or_policy=="Yes") %>%
  select(c(unique_id, intervention, sector, impact_type, methodology, subnational_context, name, iso, gov_action_or_policy,
           policy_note, policy_page, mitigation_stratcat_t1, mitigation_stratcat_t2, lat, lng, country, region)) %>%
  arrange(name, mitigation_stratcat_t2) %>%
  rename(global_region = region)

dataset$policy_note[dataset$unique_id=="195004"] <- "To encourage a speedy transition, the government will implement an economy-wide carbon tax of $20 per tonne CO2 (tCO2) beginning in 2017 and increase to $30/tCO2 in 2018"


std_data_orig <- read.csv("../Data/Analysis/08_Standardized_Impacts.csv",
                     header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

std_data <- std_data_orig %>%
  filter(gov_action_or_policy=="Yes")

# Unique Actions by Study
actions_by_study <- dataset %>%
  distinct(unique_id, name, country, iso, global_region, subnational_context, sector, mitigation_stratcat_t1, mitigation_stratcat_t2, 
           policy_note, policy_page, lat, lng) %>%
  filter(!(unique_id=="195004" & mitigation_stratcat_t1 == "Clean energy generation")) %>%
  filter(!(unique_id=="200722" & policy_note == "A new policy focusing on municipal sold waste (MSW) source-separated collection was launched in Hangzhoi city since 2010."))
  # Unique by actor, strategy/sector, and the policy note

# EXPORT for Additional Data Collection  
# write.xlsx(actions_by_study, file = "../Data/Analysis/09_Gov_Actions_To_Collect.xlsx",
#            sheetName="Export")

##### Bring in Final Actions Dataset - START HERE! ##### 
gov_actions_orig <- read_excel("../Data/Manual_Extraction/Gov_Actions_Final.xlsx")

gov_actions <- gov_actions_orig %>%
  filter(policy_status != "Under Consideration") %>% # Going to focus on planned and implemented actions
  mutate(sector = case_when(mitigation_stratcat_t1 %in% c("Circular economy, industrial symbiosis, use of recycled materials",
                                                          "Emissions trading, cap-and-trade, carbon tax",
                                                          "Land use and development") ~ "Cross-Sectoral",
                            TRUE ~ sector))
# Fix some lat lons
gov_actions$lat[gov_actions$name == "California"] <- 35.458611
gov_actions$lng[gov_actions$name == "California"] <- -119.355278

gov_actions$lat[gov_actions$name == "Hubei"] <- 30.593400
gov_actions$lng[gov_actions$name == "Hubei"] <- 114.304600

# How many studies?
length(unique(gov_actions$unique_id)) 
  # 49 studies

# How many actors?
actors <- gov_actions %>%
  distinct(unique_id, name, country, iso, global_region, subnational_context, lat, lng) %>%
  count(name, country, iso, global_region, subnational_context, lat, lng)
  # 50 actors

# What kind of strategies? - unique by actor, strategy, and study/policy note
strategies <- gov_actions %>%
  count(name, country, iso, global_region, mitigation_stratcat_t1, mitigation_stratcat_t2, subnational_context, sector, lat, lng, policy_note, broader_goal) 

# Check for instruments table counts
ins_table_check <- gov_actions %>%
  group_by(sector, policy_instrument) %>%
  summarise(count = n())

##### 2. Visuals #####
# Sector
sector_sum <- strategies %>%
  count(sector, subnational_context)

sector_count <- ggplot(sector_sum, aes(x = n, y=reorder(sector, n), fill=subnational_context)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = n)) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Sector") +
  xlab("Actions") +
  labs(title="Subnational Actions")

sector_count

# T1
t1_sum <- strategies %>%
  mutate(mitigation_stratcat_t1 = ifelse(mitigation_stratcat_t1=="Circular economy, industrial symbiosis, use of recycled materials",
                                         "Circular economy, industrial symbiosis",
                                         mitigation_stratcat_t1)) %>% # Make name shorter for plots
  count(mitigation_stratcat_t1, sector) 

actions_rank_t1 <- t1_sum %>%
  filter(mitigation_stratcat_t1 != "Industrial facility improvements") %>%
  mutate(rank_act = rank(-n, ties.method="max"))

write.csv(actions_rank_t1, "../Data/Analysis/12_actions_ranking.csv", row.names=FALSE)

t1_count_act <- ggplot(t1_sum, aes(x = n, y=reorder(mitigation_stratcat_t1, n), fill=sector)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label = n),
            hjust = 0, nudge_x = .5) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1),
    axis.title.y = element_blank()) +
  xlab("Actions") +
  labs(title="Mitigation Strategy Categories")

t1_count_act


# T2
t2_sum <- strategies %>%
    group_by(mitigation_stratcat_t2, sector) %>%
    summarise(total_studies = n()) %>%
    group_by(mitigation_stratcat_t2) %>%
    mutate(strat_total = sum(total_studies)) %>%
    ungroup()

t2_count_act <- ggplot(t2_sum, aes(x = total_studies, y=reorder(mitigation_stratcat_t2, strat_total), fill=sector)) +
  geom_bar(stat="identity") + 
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1),
    axis.title.y = element_blank()) +
  xlab("Actions") +
  labs(title="Mitigation Strategies")

t2_count_act
  
#### 3. Map Actions ####
dataset_orig <- read.csv("../Data/Analysis/10_Combined_Data_Clean_022723.csv",
                         header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))
world <- map_data("world")

length(unique(paste0(dataset_orig$name, dataset_orig$iso, dataset_orig$subnational_context)))

# All Locations
locations_all <- dataset_orig %>%
  mutate(sector = case_when(mitigation_stratcat_t1 %in% c("Circular economy, industrial symbiosis, use of recycled materials",
                                                          "Emissions trading, cap-and-trade, carbon tax",
                                                          "Land use and development") ~ "Cross-Sectoral",
                            TRUE ~ sector)) %>%
  distinct(name, country, iso, subnational_context, entity_type, lat, lng, sector)

latlng <- read_excel("../Data/Orig/Missing_Lat_Lng.xlsx")

latlng <- latlng %>%
  rename(lat2 = lat,
         lng2 = lng) %>%
  distinct(name, entity_type, iso, lat2, lng2) %>%
  filter(!(name=="Emilia-Romagna" & lng2=="10.956944"))

locations_all <- locations_all %>%
  left_join(latlng, by=c("name", "entity_type", "iso")) %>%
  mutate(lat = case_when(is.na(lat) ~ lat2,
                         TRUE ~ lat)) %>%
  mutate(lng = case_when(is.na(lng) ~ lng2,
                         TRUE ~ lng)) %>%
  select(-c(lat2, lng2)) %>%
  distinct(name, country, iso, subnational_context, lat, lng, sector) %>%
  mutate(region = country) %>%
  mutate(region = recode(region, "United States of America" = "USA", "United Kingdom" = "UK"))
  
# Locations of Actions
locations_act <- gov_actions %>%
  count(sector, lat, lng, name, iso, subnational_context, country, global_region) %>%
  mutate(region = country) %>%
  mutate(region = recode(region, "United States of America" = "USA", "United Kingdom" = "UK"))

world_subset <- inner_join(world, locations_all, by="region")

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

world_plot_act <- ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), color="white", fill="gray90") + # color="white", fill="gray90"
  coord_fixed(1.3) +
  geom_point(data=locations_all, aes(x=lng, y=lat, color=sector, shape=subnational_context), size=2.75, alpha=1, stroke=1.25) + # , position="jitter"
  scale_color_manual(name="Sector", guide="legend", 
                     labels=c("Agriculture and forestry", "Buildings", "Cross-Sectoral", "Electricity and heat", "Industry", "Transport", "Waste"),
                     values=c("#D55E00", "#E69F00","#C77CFF",  "#009E73","#000000", "#56B4E9", "#FF61CC")) +
  scale_shape_manual(name="Subnational Context", guide="legend", labels=c("City", "Region"), values=c(21,23)) +
  geom_point(data=locations_act, aes(x=lng, y=lat, color=sector, fill=sector, shape=subnational_context), size=2.75, alpha=1) + # fill="cyan4",
  scale_fill_manual(name="Sector", guide="legend", 
                    labels=c("Agriculture and forestry", "Buildings", "Cross-Sectoral", "Electricity and heat", "Industry", "Transport", "Waste"),
                    values=c("#D55E00", "#E69F00","#C77CFF", "#009E73","#000000", "#56B4E9", "#FF61CC")) +
  plain

world_plot_act


##### 4. Stats #####
# Most Common Country?
country_sum <- strategies %>%
  count(iso)

# Most Common Region?
region_sum <- strategies %>%
  count(global_region)

# Demand vs. Supply vs. Mixed?
supply_demand <- gov_actions %>%
  count(supply_or_demand) %>%
  mutate(total = sum(n),
         pct = n/total)

# Policy "smart packaging"
table(gov_actions$broader_goal)
packaging_sum <- gov_actions %>%
  count(broader_goal, sector) %>%
  pivot_wider(id_cols=sector, names_from=broader_goal, values_from=n) %>%
  replace(is.na(.), 0) %>%
  mutate(total = Yes + No,
         pct = Yes/total)

# By Location
sector_by_region <- strategies %>%
  count(sector, global_region) %>%
  group_by(global_region) %>%
  mutate(region_max = max(n))

sec_by_reg_bar <- ggplot(sector_by_region, aes(global_region, n, fill=sector)) +   
  geom_bar(aes(fill = sector), position = "dodge", stat="identity") + 
  geom_text(aes(x=global_region, label=n), position = position_dodge(width = 0.9), vjust=-0.75) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1),
    axis.title.x = element_blank()) +
  ylab("Actions") +
  labs(title="Sector of Actions by Global Region") 

sec_by_reg_bar

sector_by_instrument <- gov_actions %>%
  count(sector, policy_instrument) %>%
  group_by(policy_instrument) %>%
  mutate(ins_max = max(n))

sec_by_ins_bar <- ggplot(sector_by_instrument, aes(sector, n, fill=policy_instrument)) +   
  geom_bar(aes(fill = policy_instrument), position = "dodge", stat="identity") + 
  geom_text(aes(x=sector, label=n), position = position_dodge(width = 0.9), vjust=-0.75) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1),
    axis.title.x = element_blank()) +
  ylab("Actions") +
  labs(title="Sector of Actions by Policy Instrument") 

sec_by_ins_bar

# Table of Policy Instruments
instrument_sum <- gov_actions %>%
  count(sector, policy_instrument)

gov_actions_table <- gov_actions %>%
  arrange(sector, policy_instrument)

# Most Common Instrument Types by Actor Type
actor_type_instruments <- gov_actions %>%
  count(subnational_context, policy_instrument) %>%
  group_by(subnational_context) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/total)
