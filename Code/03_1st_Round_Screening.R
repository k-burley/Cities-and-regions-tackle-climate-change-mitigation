# ------------------------------------------------------------------------------
# Program Name: 03_1st_Round_Screening.R
# Program Purpose: Selects sample for manual screening test, incorporates manual test results into dataset for validation 

# Note: Set working directory to local version of GitHub repository "Code" folder
# Note: This code used ClimActor version 0.0.1

# Author: Katherine Burley
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
# ------------------------------------------------------------------------------

### 0. Set Up

  ## Clear R Environment
  rm(list = ls())
  R.version
  
  ## Libraries 
  library(readr)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(stringi)
  library(readxl)
  
  #install.packages("writexl")
  library(writexl)
  
  ## Install/Download ClimActor
  # install.packages("devtools")
  library(devtools)
  # devtools::install_github("datadrivenenvirolab/ClimActor", build_vignettes = T)
  library(ClimActor)
  # vignette("climactor_vignette")
  
  
### 1. Select Sample
  ## Bring in data
  flagged_data <- read.csv("../Data/Analysis/02_Flagged_Only.csv", 
           stringsAsFactors = F)
  
  ## Flag Region Terms vs. City Terms (to check stratification)
  flagged_data$Subnat.Type <- NA
  flagged_data$Subnat.Type[!is.na(flagged_data$Region.Term)] <- "Region"
  flagged_data$Subnat.Type[!is.na(flagged_data$City.Term)] <- "City"
  table(flagged_data[["Subnat.Type"]])
  
  ## Pull Sample
  set.seed(456)
  sample <- sample_n(flagged_data, 50, replace=F)
  table(sample[["Source"]])
  table(sample[["Subnat.Type"]])
  
  ## Clean Up
  sample <- subset(sample, select=-c(Standard.Title, Standard.Authors, Standard.Source.Title, Subnational.Term, Open.Access))
  sample$Include <- "Yes" # to be used in processing
  sample <- sample[, c("unique_id", "Year", "Title", "Authors", "Abstract", "Include", "Subnat.Type", "City.Term", 
                            "Region.Term","Author.Keywords", "Index.Keywords", "Source", "Source.Title", "Document.Type", 
                            "DOI", "Link")]
  
  ## Export
  write.csv(sample, "03_First_Round_Sample.csv",
            fileEncoding = "UTF-8", row.names=F)
  
### 2. Combine Reviewed Samples 
  
  ## Bring in Individual Samples from 3 Reviewers
    sample_a <- read_excel("../Data/Testing/03_First_Round_Sample_A.xlsx")  
    sample_a <- rename(sample_a, Include.A = Include)
    
    sample_b <- read_excel("../Data/Testing/03_First_Round_Sample_B.xlsx") 
    sample_b <- rename(sample_b, Include.B = Include)
    
    sample_c <- read_excel("../Data/Testing/03_First_Round_Sample_C.xlsx")  
    sample_c <- rename(sample_c, Include.C = Include)
    
    sample_comp <- merge(sample_a, sample_b[, c("unique_id", "Include.B")], by = "unique_id", all=TRUE)
    sample_comp <- merge(sample_comp, sample_c[, c("unique_id", "Include.C")], by = "unique_id", all=TRUE)
  
  ## Combine
    sample_comp <- subset(sample_comp, select=c("unique_id", "Year", "Title", "Authors", "Abstract", "Include.KB", "Include.IF", "Include.SA"))
  
  ## Summarize Results
    sample_comp$Include.A.Ind <- 0
    sample_comp$Include.A.Ind[sample_comp$Include.A == "Yes"] <- 1
    sample_comp$Include.B.Ind <- 0
    sample_comp$Include.B.Ind[sample_comp$Include.B == "Yes"] <- 1  
    sample_comp$Include.C.Ind <- 0
    sample_comp$Include.C.Ind[sample_comp$Include.C == "Yes"] <- 1   
    
    sample_comp$Include.Sum <- sample_comp$Include.A.Ind + sample_comp$Include.B.Ind + sample_comp$Include.C.Ind
    sample_comp$Discrepancy <- as.numeric(sample_comp$Include.Sum %in% c(1,2))
    
    sample_comp <- subset(sample_comp, select=-c(Include.A.Ind, Include.B.Ind, Include.C.Ind))
    write_xlsx(sample_comp, "../Data/Testing/03_Sample_Comparison.xlsx")

### 3. Export Full List
    
    ## Bring in full data
    flagged_data <- read.csv("../Data/Analysis/02_Flagged_Only.csv", 
                             stringsAsFactors = F)
    
    ## Flag Region Terms vs. City Terms (to check stratification)
    flagged_data$Subnat.Type <- NA
    flagged_data$Subnat.Type[!is.na(flagged_data$Region.Term)] <- "Region"
    flagged_data$Subnat.Type[!is.na(flagged_data$City.Term)] <- "City"  
    
    ## Clean Up
    flagged_data <- subset(flagged_data, select=-c(Standard.Title, Standard.Authors, Standard.Source.Title, Subnational.Term, Open.Access))
    flagged_data$Include <- "" # to be used in processing
    flagged_data$Reviewer <- "" # to be used in processing
    
    ## Bring in the complete sample
    complete_sample <- read_excel("../Data/Testing/03_Sample_Comparison_Final.xlsx")
    complete_sample <- subset(complete_sample, select=c("unique_id", "Include.Final"))
    
    ## Merge results from sample
    flagged_data <- merge(flagged_data, complete_sample, by="unique_id", all.x=TRUE)
    flagged_data <- flagged_data[order(flagged_data$unique_id),]
    flagged_data$Include <- flagged_data$Include.Final
    flagged_data$Include[is.na(flagged_data$Include)] <- ""
    flagged_data$Reviewer[!is.na(flagged_data$Include.Final)] <- "Sample"
    flagged_data <- subset(flagged_data, select=-c(Include.Final))
    
    flagged_data <- flagged_data[, c("unique_id", "Year", "Title", "Authors", "Abstract", "Include", "Reviewer", "Subnat.Type", 
                                     "City.Term", "Region.Term","Author.Keywords", "Index.Keywords", "Source", "Source.Title", 
                                     "Document.Type", "DOI", "Link")]
    

    ## Split and Export
    flagged_data1 <- flagged_data[1:39786,]
    # write.csv(flagged_data1, "03_First_Round_Data1.csv",
    #           fileEncoding = "UTF-8", row.names=F)  
    
    flagged_data2 <- flagged_data[39787:79572,]
    # write.csv(flagged_data2, "03_First_Round_Data2.csv",
    #           fileEncoding = "UTF-8", row.names=F)
