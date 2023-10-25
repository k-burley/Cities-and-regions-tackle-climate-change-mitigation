# ------------------------------------------------------------------------------
# Program Name: 02_Filtering.R
# Program Purpose: Flag articles with a city or region term in the title or abstract

# Note: Set working directory to local version of GitHub repository "Code" folder
# Note: This code used ClimActor version 0.0.1

# Author: Katherine Burley
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
# ------------------------------------------------------------------------------

# 0. Set Up ----

  ## Clear R Environment ----
  rm(list = ls())
  R.version

  ## Libraries ----
  library(readr)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(stringi)
  
  ## Install/Download ClimActor ----
  # install.packages("devtools")
  library(devtools)
  # devtools::install_github("datadrivenenvirolab/ClimActor", build_vignettes = T)
  library(ClimActor)
  # vignette("climactor_vignette")

# 1. Clean Up Key Dict Data From ClimActor (Version 0.0.1) ----
table(key_dict[["entity_type"]])

  ## 1.1 REGIONS ----
    regions <- subset(key_dict, entity_type == "Region")
  
    ### Tag Actor Names With Issues and Drop Them ----
      regions$error <- 0
      regions$length <- str_length(regions$wrong)
      regions$error[regions$length<=2] <- 1
      regions$error[grepl(">", regions$wrong)==TRUE] <- 1
      table(regions$error) 
      
      regions <- regions[regions$error==0,]
      regions <- transmute(regions, wrong = wrong, entity_type = entity_type)
      regions$space <- " "
      regions$wrong_wspaces <- paste(regions$space, regions$wrong, regions$space, sep="")
      regions <- distinct(regions)
      regions_vector <- as.vector(regions$wrong_wspaces)
  
    ## 1.2 CITIES ----
      cities <- subset(key_dict, entity_type == "City")
      
      ### Tag Actor Names With Issues and Drop Them ----
      cities$error <- 0
      cities$length <- str_length(cities$wrong)
      cities$error[cities$length<=2] <- 1
      cities$error[grepl(">", cities$wrong)==TRUE] <- 1
      cities$error[grepl("255o Cuarto", cities$wrong)==TRUE] <- 1 # Backslashes
      cities$error[grepl("237ehir", cities$wrong)==TRUE] <- 1 # Backslashes
      table(cities$error) 
      
      cities <- cities[cities$error==0,]
      cities <- transmute(cities, wrong = wrong, entity_type = entity_type)
      cities$space <- " "
      cities$wrong_wspaces <- paste(cities$space, cities$wrong, cities$space, sep="")
      cities <- distinct(cities)
      cities_vector <- as.vector(cities$wrong_wspaces)
    
# 2. Bring in Combined Data ----
  full_df <- read.csv("../Data/Analysis/01_Combined_Data.csv",
           encoding = "UTF-8")
      
  full_df <- rename(full_df, unique_id = X)
    
# 3. Filter for Observations with Subnational Terms in the Title or Abstract ----
  
  ## 3.1 Tag Observations with a Region Term ----
    full_df$Region.Term <- NA
    regions_list_length <- length(regions_vector)
    loop_tracker <- 0
    
    system.time(
      for (item in regions_vector) {
        loop_tracker <- loop_tracker +1
        
        print(paste("On Item", loop_tracker, "of", regions_list_length))
        
        print(paste("Looking for",item,"in Title"))
        full_df$Region.Term[grepl(item, full_df$Title, ignore.case=TRUE) == TRUE] <- item
        
        print(paste("Looking for",item,"in Abstract"))
        full_df$Region.Term[grepl(item, full_df$Abstract, ignore.case=TRUE) == TRUE] <- item
      }
    )
    table(full_df[["Region.Term"]])
    
  ## 3.2 Tag Observations with a City Term ----
  full_df$City.Term <- NA
  cities_list_length <- length(cities_vector)
  loop_tracker <- 0
  
  full_df$Subnational.Term <- 0
  full_df_flagged <- full_df[0,]
  
  ### Split cities vector into chunks ----
  cities_vector1 <- cities_vector[1:3000]
  cities_vector2 <- cities_vector[3001:6000]
  cities_vector3 <- cities_vector[6001:9000]
  cities_vector4 <- cities_vector[9001:12000]
  cities_vector5 <- cities_vector[12001:15000]
  cities_vector6 <- cities_vector[15001:17227]
  
  ### Subset 1 ----
    start_time <- Sys.time()  
      for (item in cities_vector1) {
        loop_tracker <- loop_tracker +1
        
        print(paste("On Item", loop_tracker, "of", cities_list_length))
        
        print(paste("Looking for",item,"in Title"))
        full_df$City.Term[grepl(item, full_df$Title) == TRUE] <- item
        
        print(paste("Looking for",item,"in Abstract"))
        full_df$City.Term[grepl(item, full_df$Abstract) == TRUE] <- item
        
        # Pull Out Flagged Terms
        full_df$Subnational.Term[!is.na(full_df$Region.Term) | !is.na(full_df$City.Term)] <- 1
        
        flagged <- full_df[full_df$Subnational.Term==1,] # select only flagged articles
        full_df_flagged <- rbind(full_df_flagged, flagged) # bind to flagged article list
        
        full_df <- full_df[full_df$Subnational.Term == 0,] # only keep articles that have not been flagged to improve speed
        
      }
    end_time <- Sys.time()
    end_time - start_time
    
    table(full_df[["City.Term"]])
    # write.csv(full_df_flagged, "../Data/Analysis/02_Flagged_Only.csv",
    #           fileEncoding="UTF-8", row.names=FALSE)    # Save along the way
  
  ### Subset 2 ----
    start_time <- Sys.time()  
    for (item in cities_vector2) {
      loop_tracker <- loop_tracker +1
      
      print(paste("On Item", loop_tracker, "of", cities_list_length))
      
      print(paste("Looking for",item,"in Title"))
      full_df$City.Term[grepl(item, full_df$Title) == TRUE] <- item
      
      print(paste("Looking for",item,"in Abstract"))
      full_df$City.Term[grepl(item, full_df$Abstract) == TRUE] <- item
      
      # Pull Out Flagged Terms
      full_df$Subnational.Term[!is.na(full_df$Region.Term) | !is.na(full_df$City.Term)] <- 1
      
      flagged <- full_df[full_df$Subnational.Term==1,]
      full_df_flagged <- rbind(full_df_flagged, flagged)
      
      full_df <- full_df[full_df$Subnational.Term == 0,]
      
    }
    end_time <- Sys.time()
    end_time - start_time
    
    table(full_df[["City.Term"]])
    # write.csv(full_df_flagged, "../Data/Analysis/02_Flagged_Only.csv",
    #           fileEncoding="UTF-8", row.names=FALSE)    # Save along the way

  
  ### Subset 3 ----
    start_time <- Sys.time()  
    for (item in cities_vector3) {
      loop_tracker <- loop_tracker +1
      
      print(paste("On Item", loop_tracker, "of", cities_list_length))
      
      print(paste("Looking for",item,"in Title"))
      full_df$City.Term[grepl(item, full_df$Title) == TRUE] <- item
      
      print(paste("Looking for",item,"in Abstract"))
      full_df$City.Term[grepl(item, full_df$Abstract) == TRUE] <- item
      
      # Pull Out Flagged Terms
      full_df$Subnational.Term[!is.na(full_df$Region.Term) | !is.na(full_df$City.Term)] <- 1
      
      flagged <- full_df[full_df$Subnational.Term==1,]
      full_df_flagged <- rbind(full_df_flagged, flagged)
      
      full_df <- full_df[full_df$Subnational.Term == 0,]
      
    }
    end_time <- Sys.time()
    end_time - start_time
    
    table(full_df[["City.Term"]])
    # write.csv(full_df_flagged, "../Data/Analysis/02_Flagged_Only.csv",
    #           fileEncoding="UTF-8", row.names=FALSE)    # Save along the way

  ### Subset 4 ----
    start_time <- Sys.time()  
    for (item in cities_vector4) {
      loop_tracker <- loop_tracker +1
      
      print(paste("On Item", loop_tracker, "of", cities_list_length))
      
      print(paste("Looking for",item,"in Title"))
      full_df$City.Term[grepl(item, full_df$Title) == TRUE] <- item
      
      print(paste("Looking for",item,"in Abstract"))
      full_df$City.Term[grepl(item, full_df$Abstract) == TRUE] <- item
      
      # Pull Out Flagged Terms
      full_df$Subnational.Term[!is.na(full_df$Region.Term) | !is.na(full_df$City.Term)] <- 1
      
      flagged <- full_df[full_df$Subnational.Term==1,]
      full_df_flagged <- rbind(full_df_flagged, flagged)
      
      full_df <- full_df[full_df$Subnational.Term == 0,]
      
    }
    end_time <- Sys.time()
    end_time - start_time
    
    table(full_df[["City.Term"]]) 
    # write.csv(full_df_flagged, "../Data/Analysis/02_Flagged_Only.csv",
    #           fileEncoding="UTF-8", row.names=FALSE)    # Save along the way
  
  ### Subset 5 ----
    start_time <- Sys.time()  
    for (item in cities_vector5) {
      loop_tracker <- loop_tracker +1
      
      print(paste("On Item", loop_tracker, "of", cities_list_length))
      
      print(paste("Looking for",item,"in Title"))
      full_df$City.Term[grepl(item, full_df$Title) == TRUE] <- item
      
      print(paste("Looking for",item,"in Abstract"))
      full_df$City.Term[grepl(item, full_df$Abstract) == TRUE] <- item
      
      # Pull Out Flagged Terms
      full_df$Subnational.Term[!is.na(full_df$Region.Term) | !is.na(full_df$City.Term)] <- 1
      
      flagged <- full_df[full_df$Subnational.Term==1,]
      full_df_flagged <- rbind(full_df_flagged, flagged)
      
      full_df <- full_df[full_df$Subnational.Term == 0,]
      
    }
    end_time <- Sys.time()
    end_time - start_time
    
    table(full_df[["City.Term"]])
    # write.csv(full_df_flagged, "../Data/Analysis/02_Flagged_Only.csv",
    #           fileEncoding="UTF-8", row.names=FALSE)    # Save along the way

    ### Subset 6 ----
    start_time <- Sys.time()  
    for (item in cities_vector6) {
      loop_tracker <- loop_tracker +1
      
      print(paste("On Item", loop_tracker, "of", cities_list_length))
      
      print(paste("Looking for",item,"in Title"))
      full_df$City.Term[grepl(item, full_df$Title) == TRUE] <- item
      
      print(paste("Looking for",item,"in Abstract"))
      full_df$City.Term[grepl(item, full_df$Abstract) == TRUE] <- item
      
      # Pull Out Flagged Terms
      full_df$Subnational.Term[!is.na(full_df$Region.Term) | !is.na(full_df$City.Term)] <- 1
      
      flagged <- full_df[full_df$Subnational.Term==1,]
      full_df_flagged <- rbind(full_df_flagged, flagged)
      
      full_df <- full_df[full_df$Subnational.Term == 0,]
      
    }
    end_time <- Sys.time()
    end_time - start_time
    
    table(full_df[["City.Term"]]) 
    # write.csv(full_df_flagged, "../Data/Analysis/02_Flagged_Only.csv",
    #           fileEncoding="UTF-8", row.names=FALSE)    # Save along the way

    