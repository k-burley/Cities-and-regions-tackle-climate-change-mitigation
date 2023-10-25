# ------------------------------------------------------------------------------
# Program Name: 08_Synthesis_Analysis.R
# Program Purpose: Conduct bootstrapping analysis and sensitivity tests, generate plots

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
library(tokenizers)
library(ClimActor)

library(rworldmap)
library(maps)
library(ggrepel)
library(ggpubr)
library(scales)
library(RColorBrewer)
library(gridExtra)

library(boot)
library(mosaic)
library(rms)
library(metafor)
library(robumeta)

library(plm)
library(ggbeeswarm)
library(ggdist)
library(ggridges)

##### 1. Bring in Data #####
tracker <- read_excel("../Data/Manual_Extraction/Data_Screening_and_Extraction_Final.xlsx",
                      sheet = "Tracker", col_names = TRUE, range="A1:T290") # skip = 1

dataset <- read.csv("../Data/Analysis/07_Combined_Data_Clean.csv",
                    header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

perunit_units <- c("kgCO2/100km", "kgCO2e ha−1 y−1", "kgCO2/m2-year", "tCO2eq/tMSW", "MgCO2eq/ha/year",
                      "kgCO2e/mi", "(tonnes CO2-eq.)/tonne recycled", "gCO2eq/meal", "kg CO2/m^2-year",
                      "kgCO2eq/kg of live weight", "kgCO2eq/tMSW", "	kgCO2e/kg-vegetable-year", "kgCO2eq/ha-year",
                      "7kg/hour.km", "Mg CO2e/ha", "kg CO2-eq/ha/year", "kgC02/m2", "kgCO2/m2", "kgCO2/person-day",
                      "kgCO2e/ha-year", "kgCO2e/kg-milk", "kgCO2eq/ton", "kgCO2eq/unit", "gCO2eq/MJ", "kgCO2eq/t waste",
                      "tCO2/MWh", "tonCO2/year/household", "CO2e Mg/ha-y", "g/km- vehicle", "kg/CO2eq per vehicle",
                      " kg- CO2 / km", "g/ km vehicle", "g/km", "g/km vehicle", "kg CO2-eq/GJ", "kgCO2/day/vehicle",
                      "MgCO2e/ha", "tons/km", "t/m^2", "kgCO2e ha−1 y−1", "kgCO2e/kg-vegetable-year", "t/m^2", "kg/hour.km")


##### 2. Standardize Impacts #####
synth_1_orig <- dataset %>%
  filter(synthesis_group == "sg_1" | (synthesis_group == "sg_4" & units_available=="Yes")) %>%
  filter(!(unique_id %in% c("333893", "71904"))) %>% # remove studies whose units do not match their per-unit impacts
  filter(unique_id != "346836") %>% # this study has major outliers which may indicate incorrect info - authors decided to remove
  filter(!(unique_id == "11360" & population<5000)) %>%
    # this study claims to only evaluate places with pop > 5000, but 4 locations have had pop < 5000 for many years. removing because under the study assumptions, the per-capita ER for these locations will be inflated
  filter(!is.na(population)) %>% # remove studies missing population data - need for the standardization
  mutate(across(c("impact_time_frame", "study_subject_value"), as.numeric))

# Convert Per-Unit Impacts to Standard Impacts (SG 4)
synth_1_sg4 <- synth_1_orig %>%
  filter(synthesis_group=="sg_4") %>%
  mutate(study_subject_type = case_when(unique_id == "136984" ~ "Organic Waste",
                                        unique_id == "220575" ~ "Meals",
                                        TRUE ~ study_subject_type))

# Check Combinations of impact units & study subject units 
synth_1_sg4_combos <- synth_1_sg4 %>%
  count(units, impact_time_frame, study_subject_units)

# Calculate standard impacts from per-unit impacts
synth_1_sg4 <- synth_1_sg4 %>%
  mutate(emissions_reduction_impact_orig = emissions_reduction_impact,
         units_orig = units) %>%
  mutate(emissions_reduction_impact = case_when((units == "CO2e Mg/ha-y" & study_subject_units == "km^2") ~ emissions_reduction_impact_orig*study_subject_value*100,
                                      (units == "gCO2eq/meal" & study_subject_units == "meals/year") ~ emissions_reduction_impact_orig*study_subject_value,
                                      (units == "kgCO2/m2" & study_subject_units == "m^2") ~ emissions_reduction_impact_orig*study_subject_value,
                                      (units == "kgCO2e ha-1 y-1" & study_subject_units == "km^2") ~ emissions_reduction_impact_orig*study_subject_value*100,
                                      (units == "kgCO2eq/t waste" & study_subject_units == "tons/year") ~ emissions_reduction_impact_orig*study_subject_value,
                                      (units == "kgCO2eq/tMSW" & study_subject_units == "tons/year") ~ emissions_reduction_impact_orig*study_subject_value,
                                      (units == "kgCO2eq/ton" & study_subject_units == "Mg/year") ~ emissions_reduction_impact_orig*study_subject_value,
                                      (units == "kgCO2eq/unit" & study_subject_units == "units") ~ emissions_reduction_impact_orig*study_subject_value,
                                      (units == "MgCO2eq/ha/year" & study_subject_units == "hectares") ~ emissions_reduction_impact_orig*study_subject_value,
                                      (units == "tCO2eq/tMSW" & study_subject_units == "tons/year") ~ emissions_reduction_impact_orig*study_subject_value)) %>%
  mutate(units  = case_when((units == "CO2e Mg/ha-y" & study_subject_units == "km^2") ~ "CO2e Mg/y",
                                      (units_orig == "gCO2eq/meal" & study_subject_units == "meals/year") ~ "gCO2eq/year",
                                      (units_orig == "kgCO2/m2" & study_subject_units == "m^2") ~ "kgCO2",
                                      (units_orig == "kgCO2e ha-1 y-1" & study_subject_units == "km^2") ~ "kgCO2e y-1",
                                      (units_orig == "kgCO2eq/t waste" & study_subject_units == "tons/year") ~ "kgCO2eq/year",
                                      (units_orig == "kgCO2eq/tMSW" & study_subject_units == "tons/year") ~ "kgCO2eq/year",
                                      (units_orig == "kgCO2eq/ton" & study_subject_units == "Mg/year") ~ "kgCO2eq/year", # Mg = ton
                                      (units_orig == "kgCO2eq/unit" & study_subject_units == "units") ~ "kgCO2eq", # checked article - these are units/year
                                      (units_orig == "MgCO2eq/ha/year" & study_subject_units == "hectares") ~ "MgCO2eq/year",
                                      (units_orig == "tCO2eq/tMSW" & study_subject_units == "tons/year") ~ "tCO2eq/year")) %>%
  mutate(impact_time_frame = 1) # Resolves missing and 145890 which should be set to 1 year

# Combine SG 1 and SG 4
synth_1 <- synth_1_orig %>%
  filter(synthesis_group != "sg_4") %>%
  bind_rows(synth_1_sg4) %>%
  filter(!is.na(impact_time_frame))

# Develop Weights
tracker <- tracker %>% select(unique_id,year)

synth_1 <- synth_1 %>%
  left_join(tracker) %>%
  mutate(score_1 = ifelse(impact_type=="Achieved/Ex Post", 1, 0),
         impact_time_period = case_when(impact_time_period == "Data from Jan 2015 to May 2019" ~ "2015-2019",
                                        impact_time_period == "From 2016" ~ "2016",
                                        impact_time_period == "Min 25 years since 2014" ~ "2014",
                                        impact_time_period == "The household data collection was performed in 2005" ~ "2005",
                                        impact_time_period == "2011 (data from the 2011 census was used in the calculations) " ~ "2011",
                                        TRUE ~ impact_time_period),
         impact_time_period = gsub("\\.0", "", impact_time_period),
         impact_time_period_beg = as.numeric(str_sub(impact_time_period, 1, 4)),
         impact_time_period_end = as.numeric(str_sub(impact_time_period, -4, -1)),
         impact_time_period = as.numeric(impact_time_period),
         score_2 = year-impact_time_period_end,
         score_2 = ifelse(is.na(score_2),-5,score_2), # Unclear time frame equated to project 5 years in advance
         score_2 = case_when(score_2>0 ~ 1,
                             TRUE ~ 1+(score_2/100)),
         score_3 = ifelse(methodology %in% c("Unclear", "Emissions accounting", "Emissions accounting with survey"), 0, 1),
         score_4 = case_when(any_error_metrics=="Yes" ~ 1,
                             any_error_metrics=="No" ~ 0,
                             is.na(any_error_metrics) ~ 0), # 1 study, manually checked and has no error metrics
         score_sum = score_1 + score_2 + score_3 + score_4,
         weight1 = score_sum/max(score_sum),
         weight2 = score_2) %>%
  select(-c(score_1, score_2, score_3, score_4, score_sum, impact_time_period, impact_time_period_beg, 
            any_error_metrics, emissions_factors, emissions_factors_page, survey, survey_respondents, 
            study_subject_general, study_subject_detail))

# Standardize Across Units
units_tco2 <- c("tonnes CO2", "tons CO2", "tCO2", "tCO2/year", "ton CO2e/year", "tCO2e", "tCO2eq/year", "tons", 
                "tonneCO2/day", "tonnes CO2e", "tCO2e/y", "tons CO2-eq", "tCO2e/year", "ton CO2-eq", "tonne CO2/year",
                "metric tons CO2e/yr", "ton GHG", "tonCO2e", "tonnes CO2eq", "tons CO2e/year", "	tonsCO2eq/annum", 
                "t/year", "tCO2/yr", "ton CO2", "ton CO2-e", "tonnes Co2e", "tons CO2-equivalent/year", "tonsCO2e",
                "t CO2", "t/a", "TCO2/year", "tCO2eq", "tCO2eq/month", "ton CO2/year", "ton CO2eq", "tonCO2/year",
                "tonne CO2", "Tonnes Co2e", "Tonnes CO2e", "tonsCO2/year", "Annual metric tons of greenhouse gas emission CO2",
                "metric ton CO2 eq", "metric tons CO2eq/year", "metric tons/year", "metric tons/yr", "t CO2e", "tCO2-eq",
                "tCO2/mon", "tCO2e/a", "ton CO2-eq/a", "ton CO2-eq/day", "tonCO2", "tons CO2eq", "tons equivalent CO2",
                "tonsCO2eq/annum", "1000 kgCO2e/year (ton/year)", "ton CO2e", "tCO2E", "Mg CO2-eq", "Mg/year", "MgCO2eq",
                "t CO2 eq", "CO2e Mg/y", "MgCO2eq/year", "MT CO2-eq") # also includes megagrams

units_Mtco2 <- c("MtCO2", "million tones CO2eq", "MtCO2e/yr", "mtCO2/year", "MtCO2e", "million tCO2",
                 "MMT CO2-eq", "MtCO2e/year", "MT CO2E", "million tons CO2", "MtCO2eq", "MtCO2/yr", 
                 "million metric tonnes CO2E/year", "MtCO2 (Million tons)", "MtCO2/year", 
                 "million tonnes CO2e", "MMTCO2e/year", "Mt/year of GHG emissions", "MtC", "Mtons CO2 equivalent",
                 "Million tonnes CO2eq", "Mt CO2-eq", "Mt CO2 yr-1", "Mt/yr", "MT CO2")

units_ktco2 <- c("kt CO2e", "ktCO2e", "ktonneCO2e/year", "ktCO2/year", "1000 tons CO2", "1000tCO2", "ktCO2/a", 
                 "1000 tCO2e", "ktCO2e/year", "ktCO2eq", "kiloton", "kilotonnes of CO2", "kt CO2 annually ",
                 "ktCO2", "ktonCO2", "GgCO2/year", "GgCO2eq/year", "GgCO2-eq/y") # also include gigagrams

units_kgco2 <- c("kgCO2e", "kg CO2/day", "kgCO2/year", "kgCO2eq", "kgCO2eq/year", "kgCO2/day", "kgCO2e/year", 
                 "kg CO2 equivalent", "kg CO2", "kg CO2/year", "kgCO2/mon", "kgCO2e/y", "kgCO2eq/day", "kgCO2",
                 "kgCO2e y-1")

synth_1 <- synth_1 %>%
  mutate(unit_conversion = case_when(units %in% units_tco2 ~ 1,
                                     units %in% units_Mtco2 ~ 1000000,
                                     units %in% units_ktco2 ~ 1000,
                                     units %in% units_kgco2 ~ 0.001,
                                     units == "10^4t CO2eq" ~ 10000,
                                     units == "10^5 tons CO2" ~ 100000,
                                     units == "10000 tCO2/year" ~ 10000,
                                     units == "billions kg CO2 eq" ~ 1000000,
                                     units == "Million kg CO2/25 years" ~ 1000,
                                     units == "million kgCO2-eq" ~ 1000,
                                     units == "million pounds of CO2" ~ 453.59237,
                                     units == "MkgCO2e" ~ 1000,
                                     units == "TgCO2e/year" ~ 1000000,
                                     units == "gCO2eq/year" ~ 0.000001))


# Standardize impacts
synth_1_std <- synth_1 %>%
  mutate(er_impact_tco2 = emissions_reduction_impact*unit_conversion) %>% # convert to standard units
  mutate(er_impact_tco2_annual = er_impact_tco2/impact_time_frame) %>% # convert to annual
  mutate(er_impact_tco2_annual_pc = er_impact_tco2_annual/population) %>% # convert to per-capita
  mutate(short_strat = str_trunc(mitigation_stratcat_t2, 23)) %>%
  mutate(outlier = ifelse(er_impact_tco2_annual_pc>15, 1, 0)) %>%
  mutate(across(c("unit_conversion", "er_impact_tco2", "er_impact_tco2_annual", "emissions_reduction_impact", 
                  "emissions_reduction_impact_orig"), format, scientific=FALSE, digits=2, nsmall=2)) %>%
  select(-c("lat", "lng", "population_year")) %>%
  mutate(mitigation_stratcat_t1 = ifelse(mitigation_stratcat_t1=="Circular economy, industrial symbiosis, use of recycled materials",
                                         "Circular economy, industrial symbiosis",
                                         mitigation_stratcat_t1)) %>% # Make name shorter for plots 
  mutate(er_impact_std_w1 = er_impact_tco2_annual_pc*weight1,
         er_impact_std_w2 = er_impact_tco2_annual_pc*weight2)
  

summary(synth_1_std$er_impact_tco2_annual_pc)
rm(synth_1, synth_1_sg4, synth_1_sg4_combos)

# Look at Weights
weight1 <- hist(synth_1_std$weight1, xlab="Weight", main="Weight 1 Distribution")
ggsave("G:/.shortcut-targets-by-id/0B5xR42gpgPVzSDJCS3lBSGdqdTQ/Non-state and Subnational Action  Workstream 3/2021 Materials/Global Climate Action - IKEA/GCA Component 1/Data/Figures/weight1_hist.pdf",
       dpi=600)

weight2 <- hist(synth_1_std$weight2, xlab="Weight", main="Weight 2 Distribution")
ggsave("G:/.shortcut-targets-by-id/0B5xR42gpgPVzSDJCS3lBSGdqdTQ/Non-state and Subnational Action  Workstream 3/2021 Materials/Global Climate Action - IKEA/GCA Component 1/Data/Figures/weight2_hist.pdf",
       dpi=600)

par(mfrow=c(1,2))
hist(synth_1_std$weight1, xlab="Weight", main="Weight 1")
hist(synth_1_std$weight2, xlab="Weight", main="Weight 2")

# EXPORT
# write.csv(synth_1_std, file = "../Data/Analysis/08_Standardized_Impacts.csv",
#           fileEncoding = "UTF-8", row.names = F)

##### 3. Synthesize Impacts - START HERE #####
synth_1_std <- read.csv("../Data/Analysis/08_Standardized_Impacts.csv",
                        header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings=c("", "NA"))

synth_1_std <- synth_1_std %>%
  mutate(sector = ifelse(mitigation_stratcat_t1 %in% c("Emissions trading, cap-and-trade, carbon tax", "Circular economy, industrial symbiosis","Land use and development"), "Cross-Sectoral", sector)) %>%
  mutate(sector = ifelse(sector == "Agriculture and forestry", "AFOLU", sector))

synth_1_summ <- synth_1_std %>%
  group_by(unique_id, mitigation_stratcat_t1, sector) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(mitigation_stratcat_t1, sector) %>%
  summarise(total_studies = n()) %>%
  filter(total_studies>=8) 
  

##### Check Against country-Level PC Emissions #####
wb_pc_emissions <- read.csv("../Data/Original/Contextual Data/API_EN.ATM.CO2E.PC_DS2_WorldBank_PCEmissions.csv",
                            skip=4, header=T, as.is=T, stringsAsFactors = FALSE, 
                            encoding = "UTF-8", na.strings=c("", "NA"))

wb_pc_emissions <- wb_pc_emissions %>%
  select(Country.Code, X2019) %>%
  rename(iso = Country.Code)

check <- synth_1_std %>%
  left_join(wb_pc_emissions, by="iso") %>%
  mutate(X2019 = case_when(is.na(X2019) ~ 11.72,
                            TRUE ~ X2019)) %>%
  mutate(check_pce = er_impact_tco2_annual_pc/X2019)
  # Taiwan not included in World Bank data, using data from https://www.worldometers.info/co2-emissions/taiwan-co2-emissions/

# Check outliers (>1)
View(check[check$check_pce>1,]) # 1.2% of observations, in Rwanda, Bangladesh, and Indonesia

##### Tidyverse and Mosaic - Simple Bootstrapping #####

# Create empty DF for results
results_tidy_bs <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(results_tidy_bs) <- c('mitigation_stratcat_t1','sector','studies','obs', 
                               'mn', 'sd', 'n', 'se', 'lower.ci', 'upper.ci')
results_tidy_bs <- results_tidy_bs %>%
  mutate(mitigation_stratcat_t1 = as.character(mitigation_stratcat_t1),
         sector = as.character(sector),
         studies = as.numeric(studies),
         obs = as.numeric(obs),
         mn = as.numeric(mn),
         sd = as.numeric(sd),
         n = as.integer(n),
         se = as.numeric(se),
         lower.ci = as.numeric(lower.ci),
         upper.ci = as.numeric(upper.ci))

strategies <- unique(synth_1_summ$mitigation_stratcat_t1)
n <- 0

# Loop through each tier 1 mitigation strategy
for (s in strategies) {
  set.seed(1234)
  print(s)
  n = n+1
  print(n)
  
  data_bs <- synth_1_std[synth_1_std$mitigation_stratcat_t1==s,]
  sec <- unique(data_bs$sector)
  print(paste("# Observations:", nrow(data_bs), sep=" "))
  print(paste("# Studies All:", length(unique(data_bs$unique_id)), sep=" "))
  
  bootstrap = do(10000)*mean(~er_impact_tco2_annual_pc, data=mosaic::resample(data_bs))
  
  results <- bootstrap %>%
    summarise(mn = mean(mean),
              sd = sd(mean),
              n = n()) %>%
    mutate(mitigation_stratcat_t1 = s,
           sector = sec,
           studies = length(unique(data_bs$unique_id)),
           obs = nrow(data_bs)) %>%
    mutate(se = sd/sqrt(studies),
           lower.ci = mn - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mn + qt(1 - (0.05 / 2), n - 1) * se)
  
  results_tidy_bs <- results_tidy_bs %>%
    bind_rows(results)
}

# Plot Results - Sectoral
results_tidy_bs_s <- results_tidy_bs %>%
  filter(sector!="Cross-Sectoral") %>%
  mutate(rank = rank(-mn))

coefplot_bs_s <- ggplot(results_tidy_bs_s, 
                        mapping=aes(x=reorder(mitigation_stratcat_t1, mn), y=mn)) + 
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci, color=sector)) +
  geom_point() +
  geom_text(aes(label=prettyNum(mn, digits=3)), vjust=-1, size=2.5) +
  coord_flip() +
  theme_bw() +
  # xlab("Mitigation Strategy Category") +
  ylab("Mean Effect Size (tCO2/year/capita)") +
  labs(color="Sector") +
  ggtitle("Bootstrap Mean and 95% Confidence Interval") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=7),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.key.size = unit(0.2, "cm")) 

coefplot_bs_s

boxplot_temp <- synth_1_std %>%
  filter(sector != "Cross-Sectoral") %>%
  left_join(results_tidy_bs_s[c("mitigation_stratcat_t1", "rank")], by="mitigation_stratcat_t1") %>%
  filter(!is.na(rank))

boxplot_s <- ggplot(boxplot_temp, mapping=aes(x=reorder(mitigation_stratcat_t1, -rank), y=er_impact_tco2_annual_pc)) +
  geom_boxplot(aes(color=sector)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("Impact Distribution") +
  xlab("Mitigation Strategy Category") +
  ylab("Effect Size (tCO2/year/capita)") +
  labs(color="Sector") +
  ggtitle("Impact Distribution") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=7),
        legend.position = c(0.8, 0.2),
        legend.key.size = unit(0.2, "cm"))

combo_bs_s <- grid.arrange(boxplot_s, coefplot_bs_s, hists_bs_s, layout_matrix = matrix(c(1, 3, 2, 3), nrow = 2))

combo_bs_s

combo_bs_s <- annotate_figure(combo_bs_s, top = text_grob("Standard Non-Parametric Bootstrap",
                                          face = "bold", size = 12))

# Export Figure
combo_bs_s

# Plot Results - Cross-Sectoral
results_tidy_bs_c <- results_tidy_bs %>%
  filter(sector=="Cross-Sectoral") %>%
  mutate(rank = rank(-mn))

coefplot_bs_c <- ggplot(results_tidy_bs_c, 
                        mapping=aes(x=reorder(mitigation_stratcat_t1, mn), y=mn)) + 
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci, color=sector)) +
  geom_point() +
  geom_text(aes(label=prettyNum(mn, digits=3)), vjust=-1, size=2.5) +
  coord_flip() +
  theme_bw() +
  ylab("Mean Effect Size (tCO2/year/capita)") +
  labs(color="Sector") +
  ggtitle("Bootstrap Mean and 95% Confidence Interval") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=7),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.key.size = unit(0.2, "cm")) 

coefplot_bs_c

boxplot_temp <- synth_1_std %>%
  filter(sector == "Cross-Sectoral") %>%
  left_join(results_tidy_bs_c[c("mitigation_stratcat_t1", "rank")], by="mitigation_stratcat_t1") %>%
  filter(!is.na(rank))

boxplot_c <- ggplot(boxplot_temp, mapping=aes(x=reorder(mitigation_stratcat_t1, -rank), y=er_impact_tco2_annual_pc)) +
  geom_boxplot(aes(color=sector)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("Impact Distribution") +
  xlab("Mitigation Strategy Category") +
  ylab("Effect Size (tCO2/year/capita)") +
  labs(color="Sector") +
  ggtitle("Impact Distribution") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=7),
        legend.position = c(0.8, 0.2),
        legend.key.size = unit(0.2, "cm"))

boxplot_c

combo_bs_c <- grid.arrange(boxplot_c, coefplot_bs_c, hists_bs_c, layout_matrix = matrix(c(1, 3, 2, 3), nrow = 2))

combo_bs_c <- annotate_figure(combo_bs_c, top = text_grob("Standard Non-Parametric Bootstrap",
                                                          face = "bold", size = 12))

rm(boxplot_temp)

##### Tidyverse and Mosaic - Cluster Bootstrapping #####

cluster.bootstrap = function(df, cluster_var, effectsize_var) {
  rep_table <- table(resample(unique(df[[cluster_var]])), dnn = c(cluster_var))
  rep_table_df <- as.data.frame(rep_table)

  rep_table_df[[cluster_var]] <- as.integer(as.character(rep_table_df[[cluster_var]]))

  bootstrap_df <- df[,c(cluster_var, effectsize_var)]
  bootstrap_df <- bootstrap_df[bootstrap_df[[cluster_var]] %in% rep_table_df[[cluster_var]],]
  bootstrap_df <- merge(bootstrap_df, rep_table_df, by="unique_id")

  bootstrap_df_lng <- bootstrap_df %>%
    uncount(Freq, .remove=FALSE)

  obs = mean(~ bootstrap_df[[effectsize_var]]) # , data=bootstrap_df
  obs
}

# Calculate across full dataset
boot_cluster_all = do(10000)*cluster.bootstrap(df=synth_1_std, cluster_var = "unique_id", effectsize_var = "er_impact_tco2_annual_pc")

boot_cluster_all <- boot_cluster_all %>%
  rename(mean_est = cluster.bootstrap) 

results_all <- boot_cluster_all %>%
  summarise(mn = mean(mean_est),
            sd = sd(mean_est),
            n = n()) %>%
  mutate(mitigation_stratcat_t1 = "all",
         sector = "all",
         studies = length(unique(synth_1_std$unique_id)),
         obs = nrow(boot_cluster_all)) %>%
  mutate(se = sd/sqrt(studies),
         lower.ci = mn - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mn + qt(1 - (0.05 / 2), n - 1) * se)

# Estimate Examples - British Columbia
bc_ag_red = results_all$mn[1] * 5000879
bc_new_em = 63401127-bc_ag_red
bc_pct_red = ((63401127-bc_new_em)/63401127)*100

# Estimate Examples - Buenos Aires
ba_ag_red = results_all$mn[1] * 3120612
ba_new_em = 13008741-ba_ag_red
ba_pct_red = ((13008741-ba_new_em)/13008741)*100
rm()

# Create empty DF for summary results
results_tidy_cbs <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(results_tidy_cbs) <- c('mitigation_stratcat_t1','sector','studies','obs', 
                                'mn', 'sd', 'n', 'se', 'lower.ci', 'upper.ci')
results_tidy_cbs <- results_tidy_cbs %>%
  mutate(mitigation_stratcat_t1 = as.character(mitigation_stratcat_t1),
         sector = as.character(sector),
         studies = as.numeric(studies),
         obs = as.numeric(obs),
         mn = as.numeric(mn),
         sd = as.numeric(sd),
         n = as.integer(n),
         se = as.numeric(se),
         lower.ci = as.numeric(lower.ci),
         upper.ci = as.numeric(upper.ci))

strategies <- unique(synth_1_summ$mitigation_stratcat_t1)
n <- 0

# Create empty DF for all bootstrap results
boot_cbs <- data.frame(matrix(ncol=3, nrow=0))
colnames(boot_cbs) <- c('mitigation_stratcat_t1','sector','mean_est')
boot_cbs <- boot_cbs %>%
  mutate(mitigation_stratcat_t1 = as.character(mitigation_stratcat_t1),
         sector = as.character(sector),
         mean_est = as.numeric(mean_est))

# Loop through each tier 1 mitigation strategy
for (s in strategies) {
  set.seed(1234)
  print(s)
  n = n+1
  print(n)
  
  data_bs <- synth_1_std[synth_1_std$mitigation_stratcat_t1==s,]
  sec <- unique(data_bs$sector)
  print(paste("# Observations:", nrow(data_bs), sep=" "))
  print(paste("# Studies All:", length(unique(data_bs$unique_id)), sep=" "))
  
  boot_cluster = do(10000)*cluster.bootstrap(df=data_bs, cluster_var = "unique_id", effectsize_var = "er_impact_tco2_annual_pc")
  boot_cluster <- boot_cluster %>%
    rename(mean_est = cluster.bootstrap) 
  
  # Summary Statistics
  results <- boot_cluster %>%
    summarise(mn = mean(mean_est),
              sd = sd(mean_est),
              n = n()) %>%
    mutate(mitigation_stratcat_t1 = s,
           sector = sec,
           studies = length(unique(data_bs$unique_id)),
           obs = nrow(data_bs)) %>%
    mutate(se = sd/sqrt(studies),
           lower.ci = mn - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mn + qt(1 - (0.05 / 2), n - 1) * se)
  
  results_tidy_cbs <- results_tidy_cbs %>%
    bind_rows(results)
  
  # All Bootstrap Results
  boot_cluster <- boot_cluster %>%
    mutate(mitigation_stratcat_t1 = s,
           sector = sec)

  boot_cbs <- boot_cbs %>%
    bind_rows(boot_cluster)
  
}

results_tidy_cbs <- results_tidy_cbs %>%
  mutate(rank = rank(-mn)) %>%
  mutate(figure_name = paste0(mitigation_stratcat_t1, " (n=", obs, ")"))

### PREP DATA FOR GRAPHS
# Distribution - Original Data 
boxplot_temp <- synth_1_std %>%
  left_join(results_tidy_cbs[c("mitigation_stratcat_t1", "rank", "obs")], by="mitigation_stratcat_t1") %>%
  mutate(mitigation_stratcat_t1 = case_when(mitigation_stratcat_t1=="Circular economy, industrial symbiosis" ~ "Circular economy",
                                            mitigation_stratcat_t1=="Emissions trading, cap-and-trade, carbon tax" ~ "Market-based mechanisms",
                                            TRUE ~ mitigation_stratcat_t1)) %>%
  filter(!is.na(rank)) %>%
  mutate(figure_name = paste0(mitigation_stratcat_t1, " (n=", obs, ")"))

# Distribution - Bootstrap Data 
boot_cbs <- boot_cbs %>%
  left_join(results_tidy_cbs[c("mitigation_stratcat_t1", "rank", "obs")], by="mitigation_stratcat_t1") %>%
  mutate(mitigation_stratcat_t1 = case_when(mitigation_stratcat_t1=="Circular economy, industrial symbiosis" ~ "Circular economy",
                                            mitigation_stratcat_t1=="Emissions trading, cap-and-trade, carbon tax" ~ "Market-based mechanisms",
                                            TRUE ~ mitigation_stratcat_t1)) %>%
  filter(!is.na(rank)) %>%
  mutate(figure_name = paste0(mitigation_stratcat_t1, " (n=", obs, ")"))

#### Plot Results - Sectoral and Cross-Sectoral

# GGDIST
eye_s <- ggplot(boot_cbs[boot_cbs$sector!="Cross-Sectoral",], mapping=aes(x=reorder(figure_name, -rank), y=mean_est, 
                                                                          color = sector, fill = after_scale(colorspace::lighten(color, .6)))) +
  stat_eye(normalize="groups") +
  coord_flip() +
  scale_color_manual(name="Sector", guide="legend", 
                     labels=c("AFOLU", "Buildings", "Electricity and heat", "Transport", "Waste"),
                     values=c("#D55E00", "#FFB000","#009E73","#56B4E9", "#FF61CC")) +  
  theme_bw() +
  ggtitle("Bootstrap Mean Distribution") +
  xlab(" ") +
  ylab("Mean Effect Size (tCO2/year/capita)") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=9),
        legend.position = c(0.8, 0.2),
        legend.key.size = unit(0.2, "cm"))


# Coefficient Plot
coefplot_cbs_s <- ggplot(results_tidy_cbs[results_tidy_cbs$sector!="Cross-Sectoral",], mapping=aes(x=reorder(figure_name, mn), y=mn)) + 
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci, color=sector, lwd="0.4pt", width=0.25)) +
  geom_point() +
  geom_text(aes(label=prettyNum(mn, digits=3)), vjust=-1, size=3) +
  coord_flip() +
  scale_color_manual(name="Sector", guide="legend", 
                     labels=c("AFOLU", "Buildings", "Electricity and heat", "Transport", "Waste"),
                     values=c("#D55E00", "#FFB000","#009E73","#56B4E9", "#FF61CC")) + 
  theme_bw() +
  # xlab("Mitigation Strategy Category") +
  ylab("Mean Effect Size (tCO2/year/capita)") +
  labs(color="Sector") +
  ggtitle("Bootstrap Mean and 95% Confidence Interval") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=7),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") 

# Option 2: Bootstrap Eye + Coefficient Plot
combo_cbs_s <- ggarrange(eye_s, coefplot_cbs_s, widths=c(3,2))
combo_cbs_s <- annotate_figure(combo_cbs_s, top = text_grob("Cluster Non-Parametric Bootstrap",
                                                             face = "bold", size = 12))
combo_cbs_s

# GGDIST
eye_c <- ggplot(boot_cbs[boot_cbs$sector=="Cross-Sectoral",], 
                mapping=aes(x=reorder(figure_name, -rank), y=mean_est,
                            color = sector, fill = after_scale(colorspace::lighten(color, .6)))) +
  stat_eye(normalize="groups") +
  coord_flip() +
  scale_color_manual(name="Sector", guide="legend", 
                     labels=c("Cross-Sectoral"),
                     values=c("#C77CFF")) + 
  theme_bw() +
  ggtitle("Bootstrap Mean Distribution") +
  xlab(" ") +
  ylab("Mean Effect Size (tCO2/year/capita)") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=9),
        legend.position = c(0.8, 0.2),
        legend.key.size = unit(0.2, "cm"))

# Coefficient Plot
coefplot_cbs_c <- ggplot(results_tidy_cbs[results_tidy_cbs$sector=="Cross-Sectoral",], mapping=aes(x=reorder(figure_name, mn), y=mn)) + 
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci, color=sector, lwd="0.4pt", width=0.25)) +
  geom_point() +
  geom_text(aes(label=prettyNum(mn, digits=3)), vjust=-1, size=3) +
  coord_flip() +
  scale_color_manual(name="Sector", guide="legend", 
                     labels=c("Cross-Sectoral"),
                     values=c("#C77CFF")) + 
  theme_bw() +
  # xlab("Mitigation Strategy Category") +
  ylab("Mean Effect Size (tCO2/year/capita)") +
  labs(color="Sector") +
  ggtitle("Bootstrap Mean and 95% Confidence Interval") +
  theme(axis.title =element_text(size=8),
        axis.text =element_text(size=7),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") 

coefplot_cbs_c

# Option 2: Bootstrap Eye + Coefficient Plot
combo_cbs_c <- ggarrange(eye_c, coefplot_cbs_c, widths=c(3,2))
combo_cbs_c <- annotate_figure(combo_cbs_c, top = text_grob("Cluster Non-Parametric Bootstrap",
                                                              face = "bold", size = 12))
combo_cbs_c

##### Simple Ranking #####
results_tidy_cbs <- results_tidy_cbs %>%
  select(-rank) %>%
  mutate(cv = (sd/mn),
         rank_er = rank(-mn),
         rank_un = rank(cv),
         sum_rks = rank_er + rank_un,
         rank_fin = rank(sum_rks))

rank_df <- results_tidy_cbs %>%
  pivot_longer(cols=c("rank_er", "rank_un"), names_to = "rank_type", values_to= "rank") %>%
  mutate(rank_type = case_when(rank_type == "rank_er" ~ "rank_2er",
                               rank_type == "rank_un" ~ "rank_1un")) %>%
  mutate(mitigation_stratcat_t1 = case_when(mitigation_stratcat_t1=="Circular economy, industrial symbiosis" ~ "Circular economy",
                                            mitigation_stratcat_t1=="Emissions trading, cap-and-trade, carbon tax" ~ "Market-based mechanisms",
                                            TRUE ~ mitigation_stratcat_t1))

ggplot(rank_df, aes(x = rank, y=reorder(mitigation_stratcat_t1, -rank_fin), fill=rank_type)) +
  geom_bar(aes(fill=rank_type),stat="identity", position="stack") + 
  geom_text(aes(label = rank), position=position_stack(vjust=0.5), colour="white") + 
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("") +
  xlab("Sum of Ranks") +
  labs(title = "Mitigation Strategy Category Ranking") +
  scale_fill_discrete(name = "Ranking Type", breaks=c('rank_2er', 'rank_1un'), labels = c("Effectiveness Rank", "Uncertainty Rank"))

##### Sensitivity Check - Weight 1 #####

# Create empty DF for results
results_tidy_cbs_w1 <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(results_tidy_cbs_w1) <- c('mitigation_stratcat_t1','sector','studies','obs', 
                                   'mn', 'sd', 'n', 'se', 'lower.ci', 'upper.ci')
results_tidy_cbs_w1 <- results_tidy_cbs_w1 %>%
  mutate(mitigation_stratcat_t1 = as.character(mitigation_stratcat_t1),
         sector = as.character(sector),
         studies = as.numeric(studies),
         obs = as.numeric(obs),
         mn = as.numeric(mn),
         sd = as.numeric(sd),
         n = as.integer(n),
         se = as.numeric(se),
         lower.ci = as.numeric(lower.ci),
         upper.ci = as.numeric(upper.ci))

strategies <- unique(synth_1_summ$mitigation_stratcat_t1)
n <- 0

# Loop through each tier 1 mitigation strategy
for (s in strategies) {
  set.seed(1234)
  print(s)
  n = n+1
  print(n)
  
  data_bs <- synth_1_std[synth_1_std$mitigation_stratcat_t1==s,]
  sec <- unique(data_bs$sector)
  print(paste("# Observations:", nrow(data_bs), sep=" "))
  print(paste("# Studies All:", length(unique(data_bs$unique_id)), sep=" "))
  
  boot_cluster = do(10000)*cluster.bootstrap(df=data_bs, cluster_var = "unique_id", effectsize_var = "er_impact_std_w1")
  boot_cluster <- boot_cluster %>%
    rename(mean_est = cluster.bootstrap) 
  
  results <- boot_cluster %>%
    summarise(mn = mean(mean_est),
              sd = sd(mean_est),
              n = n()) %>%
    mutate(mitigation_stratcat_t1 = s,
           sector = sec,
           studies = length(unique(data_bs$unique_id)),
           obs = nrow(data_bs)) %>%
    mutate(se = sd/sqrt(studies),
           lower.ci = mn - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mn + qt(1 - (0.05 / 2), n - 1) * se)
  
  results_tidy_cbs_w1 <- results_tidy_cbs_w1 %>%
    bind_rows(results)
  
}

##### Sensitivity Check - Weight 2 #####

# Create empty DF for results
results_tidy_cbs_w2 <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(results_tidy_cbs_w2) <- c('mitigation_stratcat_t1','sector','studies','obs', 
                                   'mn', 'sd', 'n', 'se', 'lower.ci', 'upper.ci')
results_tidy_cbs_w2 <- results_tidy_cbs_w2 %>%
  mutate(mitigation_stratcat_t1 = as.character(mitigation_stratcat_t1),
         sector = as.character(sector),
         studies = as.numeric(studies),
         obs = as.numeric(obs),
         mn = as.numeric(mn),
         sd = as.numeric(sd),
         n = as.integer(n),
         se = as.numeric(se),
         lower.ci = as.numeric(lower.ci),
         upper.ci = as.numeric(upper.ci))

strategies <- unique(synth_1_summ$mitigation_stratcat_t1)
n <- 0

# Loop through each tier 1 mitigation strategy
for (s in strategies) {
  set.seed(1234)
  print(s)
  n = n+1
  print(n)
  
  data_bs <- synth_1_std[synth_1_std$mitigation_stratcat_t1==s,]
  sec <- unique(data_bs$sector)
  print(paste("# Observations:", nrow(data_bs), sep=" "))
  print(paste("# Studies All:", length(unique(data_bs$unique_id)), sep=" "))
  
  boot_cluster = do(10000)*cluster.bootstrap(df=data_bs, cluster_var = "unique_id", effectsize_var = "er_impact_std_w2")
  boot_cluster <- boot_cluster %>%
    rename(mean_est = cluster.bootstrap) 
  
  results <- boot_cluster %>%
    summarise(mn = mean(mean_est),
              sd = sd(mean_est),
              n = n()) %>%
    mutate(mitigation_stratcat_t1 = s,
           sector = sec,
           studies = length(unique(data_bs$unique_id)),
           obs = nrow(data_bs)) %>%
    mutate(se = sd/sqrt(studies),
           lower.ci = mn - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mn + qt(1 - (0.05 / 2), n - 1) * se)
  
  results_tidy_cbs_w2 <- results_tidy_cbs_w2 %>%
    bind_rows(results)
  
}

# Compare
results_tidy_bs <- results_tidy_bs %>%
  mutate(method = "Standard",
         rank = rank(-mn))

results_tidy_cbs <- results_tidy_cbs %>%
  mutate(method = "Cluster",
         rank = rank(-mn))

results_tidy_cbs_w1 <- results_tidy_cbs_w1 %>%
  mutate(method = "Weight1",
         rank = rank(-mn))

results_tidy_cbs_w2 <- results_tidy_cbs_w2 %>%
  mutate(method = "Weight2",
         rank = rank(-mn))

results_compare <- results_tidy_cbs %>%
  bind_rows(results_tidy_bs) %>%
  bind_rows(results_tidy_cbs_w1) %>%
  bind_rows(results_tidy_cbs_w2) %>%
  mutate(mitigation_stratcat_t1 = case_when(mitigation_stratcat_t1 == "Emissions trading, cap-and-trade, carbon tax" ~ "Market-based mechanisms",
                                            mitigation_stratcat_t1 == "Circular economy, industrial symbiosis" ~ "Circular economy",
                                            TRUE ~ mitigation_stratcat_t1))

# Coefplot
coef_comp <- ggplot(results_compare, mapping=aes(x=reorder(mitigation_stratcat_t1, mn), y=mn, color=method)) + 
  geom_point(position=position_dodge(width=0.5)) +
  # scale_color_manual(name="Method",values=c("green4","hotpink","royalblue2","orange")) +
  # geom_text(aes(label=prettyNum(mn, digits=3)), vjust=-1.4, size=3) +
  coord_flip() +
  theme_bw() +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci), position=position_dodge(width=0.5)) +
  xlab("Mitigation Strategy Category") +
  ylab("Mean Effect Size (tCO2/year/capita)") +
  theme(axis.title =element_text(size=12),
        axis.text =element_text(size=10),
        legend.position = "none") # turn off legend for combo plot

coef_comp

# Bar Plot Ranks
rank_comp <- ggplot(results_compare, aes(x = rank, y=reorder(mitigation_stratcat_t1, -rank), fill=method)) +
  geom_bar(stat="identity", position="dodge") + # , fill = "steelblue"
  # scale_fill_manual(name="Method",values=c("green4","hotpink","royalblue2","orange")) +
  guides(fill=guide_legend(title="Method")) +
  geom_text(aes(label = rank, group=method), 
            position = position_dodge(width = .9), hjust=-0.25, size=3) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "gray", fill=NA, linewidth=1)) +
  ylab("Mitigation Strategy") +
  xlab("Rank") +
  theme_bw() +
  theme(
    axis.title.x =element_text(size=12),
    axis.text.y = element_blank(),
    axis.title.y = element_blank() #remove y axis labels
  )
rank_comp

# Export Figure
method_comp <- ggarrange(coef_comp, rank_comp)
method_comp

##### Sensitivity Check - Diff Seed #####

# Create empty DF for results
results_tidy_cbs_seed <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(results_tidy_cbs_seed) <- c('mitigation_stratcat_t1','sector','studies','obs', 
                                   'mn', 'sd', 'n','se', 'lower.ci', 'upper.ci')

results_tidy_cbs_seed <- results_tidy_cbs_seed %>%
  mutate(mitigation_stratcat_t1 = as.character(mitigation_stratcat_t1),
         sector = as.character(sector),
         studies = as.numeric(studies),
         obs = as.numeric(obs),
         mn = as.numeric(mn),
         sd = as.numeric(sd),
         n = as.integer(n),
         se = as.numeric(se),
         lower.ci = as.numeric(lower.ci),
         upper.ci = as.numeric(upper.ci))

strategies <- unique(synth_1_summ$mitigation_stratcat_t1)
n <- 0

# Loop through each tier 1 mitigation strategy
for (s in strategies) {
  set.seed(4321)
  print(s)
  n = n+1
  print(n)
  
  data_bs <- synth_1_std[synth_1_std$mitigation_stratcat_t1==s,]
  sec <- unique(data_bs$sector)
  print(paste("# Observations:", nrow(data_bs), sep=" "))
  print(paste("# Studies All:", length(unique(data_bs$unique_id)), sep=" "))
  
  boot_cluster = do(10000)*cluster.bootstrap(df=data_bs, cluster_var = "unique_id", effectsize_var = "er_impact_std_w2")
  boot_cluster <- boot_cluster %>%
    rename(mean_est = cluster.bootstrap) 
  
  results <- boot_cluster %>%
    summarise(mn = mean(mean_est),
              std.err = sd(mean_est),
              n = n()) %>%
    mutate(mitigation_stratcat_t1 = s,
           sector = sec,
           studies = length(unique(data_bs$unique_id)),
           obs = nrow(data_bs)) %>%
    mutate(se = sd/sqrt(studies),
           lower.ci = mn - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mn + qt(1 - (0.05 / 2), n - 1) * se)
  
  results_tidy_cbs_seed <- results_tidy_cbs_seed %>%
    bind_rows(results)
  
}

results_tidy_cbs_seed <- results_tidy_cbs_seed %>%
  mutate(method = "Different Seed",
         rank = rank(-mn))

results_comp2 <- results_tidy_cbs %>%
  bind_rows(results_tidy_cbs_seed)

# Coefplot 
coef_comp2 <- ggplot(results_comp2, mapping=aes(x=reorder(mitigation_stratcat_t1, mn), y=mn, color=method)) + 
  geom_point(position=position_dodge(width=0.5)) +
  # scale_color_manual(name="Method",values=c("green4","hotpink","royalblue2","orange")) +
  # geom_text(aes(label=prettyNum(mn, digits=3)), vjust=-1.4, size=3) +
  coord_flip() +
  theme_bw() +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci), position=position_dodge(width=0.5)) +
  xlab("Mitigation Strategy Category") +
  ylab("Mean Effect Size (tCO2/year/capita)") +
  theme(axis.title =element_text(size=12),
        axis.text =element_text(size=10)) # turn off legend for combo plot

coef_comp2

# Simple Distributions

# Loop through strategies to create simple histograms
n <- 0

for (s in strategies) {
  n = n+1
  data_ss <- synth_1_std[synth_1_std$mitigation_stratcat_t1==s,]
  
  # Histogram of Observations
  assign(paste0("simple_hist",n,sep=""), 
         ggplot(data_ss) +
           geom_histogram(aes(x=er_impact_tco2_annual_pc), 
                          bins=50, fill="thistle", col="black",
                          breaks = seq(-3, 9, by = 1)) + 
           theme_minimal() + 
           ylab("Freq") +
           ggtitle(s) +
           theme(plot.title =element_text(size=7,face="bold"),
                 axis.title =element_text(size=8)))
}

hists_ss <- ggarrange(simple_hist1, simple_hist2, simple_hist3, simple_hist4, 
                      simple_hist5, simple_hist6, simple_hist7, simple_hist8, 
                      simple_hist9, simple_hist10, simple_hist11, simple_hist12, 
                      ncol=3, nrow=4)

rm(simple_hist1, simple_hist2, simple_hist3, simple_hist4, 
   simple_hist5, simple_hist6, simple_hist7, simple_hist8, 
   simple_hist9, simple_hist10, simple_hist11, simple_hist12)

synth_1_std <- synth_1_std %>%
  group_by(mitigation_stratcat_t1) %>%
  mutate(median = median(er_impact_tco2_annual_pc)) %>%
  ungroup()

boxplot_ss <- ggplot(data = synth_1_std, aes(x=reorder(mitigation_stratcat_t1, median), y=er_impact_tco2_annual_pc)) +
  geom_boxplot() + # outlier.shape=="NA"
  coord_flip() 

combo_ss <- ggarrange(boxplot_ss, hists_ss)

combo_ss <- annotate_figure(combo_ss, top = text_grob("Simple Distributions",
                                                      face = "bold", size = 12))
combo_ss

