# ------------------------------------------------------------------------------
# Program Name: 12_Sankey_Diagram.R
# Program Purpose: 
# Plot sankey diagram of the frequency of emissions reduction impacts in the full dataset by strategy, sector, and category

# Note: Set working directory to local version of GitHub repository "Code" folder
# Note: This code used ClimActor version 0.0.1

# Author: Katherine Burley
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
# ------------------------------------------------------------------------------

# Load package
# library(plyr)
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
library(networkD3)
library(sankeyD3)

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

require(devtools)
# install_github("Displayr/flipPlots", dependencies = NA)
library(flipPlots)

# 1. Bring in Data ----
dataset_orig <- read.csv("../Data/Analysis/07_Combined_Data_Clean.csv",
                         header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

# 2. Format Data ----
sankey <- dataset_orig %>%
  select(unique_id, emissions_reduction_impact, units, impact_time_period, intervention, mitigation_stratcat_t1, mitigation_stratcat_t2, sector) %>%
  mutate(sector_final = case_when(mitigation_stratcat_t1 %in% c("Emissions trading, cap-and-trade, carbon tax", 
                                                                "Circular economy, industrial symbiosis, use of recycled materials",
                                                                "Land use and development") ~ "Cross-Sectoral",
                                  TRUE ~ sector))

sankey_table <- sankey %>%
  count(mitigation_stratcat_t2, mitigation_stratcat_t1, sector, sector_final)

sankey$one <- rep(1,nrow(sankey))

sankey_d3 <- sankey %>%
  mutate(mitigation_stratcat_t1 = case_when(mitigation_stratcat_t1=="Circular economy, industrial symbiosis, use of recycled materials" ~ "Circular economy",
                                            mitigation_stratcat_t1=="Emissions trading, cap-and-trade, carbon tax" ~ "Market based mechanisms",
                                            TRUE ~ mitigation_stratcat_t1),
         mitigation_stratcat_t2 = case_when(mitigation_stratcat_t2=="Biomass, biodiesel, biomass gasification, ethanol production" ~ "Biomass cultivation",
                                            mitigation_stratcat_t2=="Circular economy, industrial symbiosis, use of recycled materials"~ "Industrial symbiosis, use of recycled materials",
                                            mitigation_stratcat_t2=="Cogeneration, combined heat and power, tri-generation, combined heat, cooling, and power" ~  "Cogeneration, CHP",
                                            mitigation_stratcat_t2=="Composting and biological treatment of waste" ~ "Composting, biological treatment of waste",
                                            mitigation_stratcat_t2=="Cool roof/facade, roof garden, green roof" ~ "Cool roof, green roof",
                                            mitigation_stratcat_t2=="Demand side energy management, optimization, peak shifting/ shaving" ~ "Demand side energy management",
                                            mitigation_stratcat_t2=="District heating/cooling, expansion" ~ "District heating/cooling",
                                            mitigation_stratcat_t2=="Emissions trading, cap-and-trade, carbon tax" ~ "Emissions trading, carbon tax",
                                            mitigation_stratcat_t2=="Geothermal heat pumps, ground source heat pumps"~"Geothermal, ground source heat pumps",
                                            mitigation_stratcat_t2=="Green building, passive solar building design, net zero emission building, carbon neutral building" ~ "Green building practices",
                                            mitigation_stratcat_t2=="Master planning, integrated planning, urban form, design, planning"~"Master planning, urban form",
                                            mitigation_stratcat_t2=="Smart meters, intelligent controls, thermostats, building information and monitoring systems" ~ "Intelligent controls, monitoring systems",
                                            mitigation_stratcat_t2=="Solar PV thermal, Solar PV cooling, Solar trigeneration CPVT" ~ "Solar PV cooling/heating",
                                            TRUE ~ mitigation_stratcat_t2))

# Make 2 dataframes for a 3 level Sankey Diagram
left = data.frame(sankey_d3["mitigation_stratcat_t2"], sankey_d3["sector"], sankey_d3$one)
colnames(left) = c("Source", "Target", "Num")

right = data.frame(sankey_d3["sector"], sankey_d3["mitigation_stratcat_t1"], sankey_d3$one)
colnames(right) = c("Source", "Target", "Num")

# Now summarize and collapse to unique values to calculate edges
library(plyr)
l <- ddply(left, .(Source,Target), summarize, Value=sum(Num))
r <- ddply(right, .(Source,Target), summarize, Value=sum(Num))

# Calculate Nodes lookup table
nodes <- c(as.character(l$Source), as.character(l$Target), as.character(r$Target))
nodes <- data.frame(unique(as.factor(nodes)))
colnames(nodes) = c("Source")
nodes$ID <- seq.int(from = 0, to = nrow(nodes) - 1)
nodes <- nodes[,c("ID","Source")]

# Now map Node lookup table numbers to source and target
# Merge index onto Source
l_edges <- merge(l, nodes, by.x = "Source")
l_edges$source = l_edges$ID
r_edges <- merge(r, nodes, by.x = "Source")
r_edges$source = r_edges$ID

# Merge index onto Target
names(nodes) = c("ID", "Target")
l_edges2 <- l_edges[,c("Source","Target","Value","source")]
r_edges2 <- r_edges[,c("Source","Target","Value","source")]
l_edges <- merge(l_edges2, nodes, by.x = "Target")
r_edges <- merge(r_edges2, nodes, by.x = "Target")

# rename everything
names(l_edges) <- c("osrc", "otgt", "value", "source", "target")
names(r_edges) <- c("osrc", "otgt", "value", "source", "target")
names(nodes) = c("ID", "name")

nodes$node_group <- as.character(nodes$name)
nodes$node_group[nodes$name %in% c("Circular economy", "Market based mechanisms", "Land use and development")] <- "Cross-Sectoral"

# Combine into one big final data frame
edges <- rbind(l_edges, r_edges)
edges$group1 <- "type"
edges$group2 <- seq(1,nrow(edges),1)
edges$group <- paste(edges$group1, edges$group2, sep="")
edges <- edges %>%
  select(-c(group1, group2))

# Set colors
# Resources: https://stackoverflow.com/questions/46616321/modify-networkd3-sankey-plot-with-user-defined-colors,https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html, https://github.com/holtzy/R-graph-gallery/blob/master/322-custom-colours-in-sankey-diagram.Rmd

nodes <- nodes %>%
  mutate(group = case_when(name %in% c("Afforestation, greening", "Master planning, urban form", "Transit oriented development", "Land use and development") ~ "group_1",
                           name %in% c("Agricultural land management", "Biomass cultivation", "Urban agriculture", "Agriculture and forestry", "Agriculture and farm practices") ~ "group_2",
                           name %in% c("Industrial symbiosis, use of recycled materials", "Circular economy") ~ "group_3",
                           name %in% c("Cogeneration, CHP", "District heating/cooling", "Electric Mobility (EV, HEV)", "Energy storage, batteries", "Fuel switching and fuel efficiency", "Other renewable energy","Solar PV energy", "Waste heat recovery") ~ "group_4",
                           name %in% c("Composting, biological treatment of waste", "Waste system management", "Waste to energy, energy from waste", "Water and wastewater system efficiencies", "Waste", "Waste and water treatment practices") ~ "group_5",
                           name %in% c("Cool roof, green roof", "Demand side energy management", "Energy efficiency measures", "Geothermal, ground source heat pumps", "Green building practices", "Retrofitting", "Intelligent controls, monitoring systems", "Solar PV cooling/heating", "Buildings", "Building construction and improvement", "Building energy and heat systems") ~ "group_6",
                           name %in% c("Emissions trading, carbon tax", "Market based mechanisms") ~ "group_7",
                           name %in% c("Energy system efficiencies", "Low-carbon energy generation", "Wind energy", "Electricity and heat", "Clean energy generation", "Energy system operations") ~ "group_8",
                           name %in% c("Intelligent transportation system", "Non-motorized transport", "Public transport","Ride sharing, carpooling", "Travel demand management", "Transport", "Alternative transportation modes", "Clean vehicle transportation", "Transportation system management") ~ "group_9",
                           name %in% c("Industry", "Industrial facility improvements") ~ "group_10"))


edges <- edges %>%
  mutate(lgroup = case_when(group %in% c("type1", "type4", "type23","type41", "type43", "type63", "type64", "type65") ~ "lgroup_1",
                            group %in% c("type2", "type3", "type5", "type53") ~ "lgroup_2",
                            group %in% c("type6", "type7", "type8", "type9", "type11", "type12", "type13", "type14", "type15", "type16", "type17","type18", "type19", "type20", "type21", "type55", "type56") ~ "lgroup_3",
                            group %in% c("type10", "type29", "type33", "type66", "type67", "type68") ~ "lgroup_4",
                            group %in% c("type22", "type24", "type25", "type26", "type27", "type28", "type30", "type31", "type59", "type61") ~ "lgroup_5",
                            group %in% c("type32",  "type35", "type36", "type37", "type38", "type62") ~ "lgroup_6",
                            group %in% c("type34", "type52", "type57", "type58") ~ "lgroup_7",
                            group %in% c("type39", "type40", "type42", "type44", "type45", "type46", "type47", "type54", "type60", "type69") ~ "lgroup_8",
                            group %in% c("type48", "type49", "type50", "type51", "type70") ~ "lgroup_9"))

node_color <- 'd3.scaleOrdinal(["firebrick","#F8766D", "orange","yellowgreen","#00BA38", "#00BFC4", "#C77CFF", "#619CFF", "#FF61CC", "firebrick","#F8766D","yellow","#FF61CC","orange","yellowgreen","#00BA38", "#C77CFF","#619CFF","#00BFC4"])'

# 3. Plot Sankey Diagram  ----
sn <- sankeyNetwork(Links = edges, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize = 14, nodeWidth = 30, fontFamily = "arial black",
                    LinkGroup="lgroup", NodeGroup="group", colourScale = node_color)

# Note: Can use saveNetwork() to export diagram to html. 
