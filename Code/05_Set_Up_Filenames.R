# ------------------------------------------------------------------------------
# Program Name: 05_Set_Up_Filenames.R
# Program Purpose: Selects sample for manual screening test, incorporates manual test results into dataset for validation 

# Note: Set working directory to local version of GitHub repository "Code" folder
# Note: This code used ClimActor version 0.0.1

# Author: Katherine Burley
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
# ------------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(readxl)
library(tidytext)
library(igraph)
library(ggraph)

#1. Set Up Filenames to Assign to Downloaded Articles
data <- read.csv("../Data/Analysis/04_Abstract_Filtering_Results.csv",
                      header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8")

data <- data %>%
  mutate(auth = gsub(" .*$", "", tolower(Authors))) %>%
  mutate(auth = gsub("[^a-zA-Z0-9]", "", auth)) %>%
  mutate(file_name = paste0("id_",as.character(unique_id),"_",auth,"_", as.character(Year))) 
  
to_download <- data %>%
  subset(select=c("unique_id", "DOI", "file_name"))

write.csv(to_download, "../Data/Analysis/05_Article_Download_List_All.csv",
          fileEncoding = "UTF-8", row.names = F)

################################################################################

#2. Select Random Sample

downloads <- list.files("C:/Article_Downloads",
                 pattern="\\.pdf$")

downloads <- gsub(pattern="\\.pdf$", "", downloads)

data_downloads <- data %>%
  filter(file_name %in% downloads) %>%
  filter(Year>=2015)

## Pull Sample
set.seed(456)
sample <- sample_n(data_downloads, 200, replace=F)

sample <- sample %>%
  subset(select=c("unique_id", "Year", "Title", "Authors")) %>%
  arrange(unique_id, Year, Title, Authors)

write.csv(sample, "../Data/Analysis/05_Articles_Sample.csv",
          fileEncoding = "UTF-8", row.names = F)


################################################################################

#2. Select Another Random Sample for OOS Testing

downloads <- list.files("C:/Article_Downloads",
                        pattern="\\.pdf$") # Location of PDFs on local computer
downloads <- gsub(pattern="\\.pdf$", "", downloads)


reviewed_sample <- list.files("C:/Data_Extraction_Sample_50",
                             pattern="\\.pdf$") # Location of PDFs on local computer
reviewed_sample <- gsub(pattern="\\.pdf$", "", reviewed_sample)


data_downloads <- data %>%
  filter(file_name %in% downloads) %>% # from the full articles
  filter(!file_name %in% reviewed_sample) %>% # not in the sample we've already reviewed
  filter(Year>=2015)

set.seed(456)
sample_2 <- sample_n(data_downloads, 50, replace=F)

sample_2 <- sample_2 %>%
  subset(select=c("unique_id", "Year", "Title", "Authors")) %>%
  arrange(unique_id, Year, Title, Authors)

write.csv(sample_2, "../Data/Analysis/05_Articles_Sample_OOS_Test.csv",
          fileEncoding = "UTF-8", row.names = F)
