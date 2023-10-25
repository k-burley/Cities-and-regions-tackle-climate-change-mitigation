# ------------------------------------------------------------------------------
# Program Name: 01_Import_and_Initial_Processing.R
# Program Purpose: Combine data extracts from Scopus and Web of Science and perform initial removal of duplicates
# Note: Set working directory to local version of GitHub repository

# Author: Katherine Burley
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
# ------------------------------------------------------------------------------

### 0. Set Up ----

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

### 1. Bring In Scopus Data ----

  #### 1.1 Append Extract Files ----
  scopus_files <- list.files(path="../Data/Original/Data-Extracts-Scopus", pattern = "scopus_", full.names = TRUE)
  head(scopus_files)
  
  scopus_full_extract_df <- do.call(rbind,lapply(scopus_files, function(i){read.csv(i, encoding="UTF-8")}))
  
  scopus_full_extract_df <- scopus_full_extract_df[order(scopus_full_extract_df$Title),]
  
  #### 1.2 Investigate Duplicates (Optional) ----
  # length(unique(scopus_full_extract_df$Title))
  # dupes <- distinct(scopus_full_extract_df[duplicated(scopus_full_extract_df), ]) # df of duplicates (unique)
  # length(unique(dupes$Title))
  # dupes <- dupes[order(dupes$Title),]
  # dupes$dup <- 1
  # scopus_temp <- merge(scopus_full_extract_df, dupes, all=TRUE)
  # scopus_temp$dup <- ifelse(is.na(scopus_temp$dup), 0, scopus_temp$dup)
  # table(scopus_temp[["dup"]])
  # rm(dupes, scopus_temp)
  
  #### 1.3 Drop/Rename Columns ----
  scopus_clean_df <- subset(scopus_full_extract_df, select=-c(Volume, Issue, Art..No., Page.start, Page.end, Page.count, 
                                                              Cited.by, Affiliations, Authors.with.affiliations))
  scopus_clean_df <- rename(scopus_clean_df, Authors = X.U.FEFF.Authors, Author.Ids = Author.s..ID, 
                            Source.Title = Source.title, Source.ID = EID)
  
  #### 1.4 Clean Up Duplicates ----
  scopus_clean_df <- scopus_clean_df %>%
    distinct()
  
  #### 1.5 Prep for Append, Standardize Authors ----
  scopus_toappend <- scopus_clean_df
  
  scopus_toappend <- subset(scopus_toappend, select=c(Authors, Title, Year, Source.Title, DOI, Link, Abstract, Author.Keywords,
                                                      Index.Keywords, Document.Type, Open.Access, Source, Source.ID))
  
  scopus_toappend$Standard.Authors <- scopus_toappend$Authors  
  
  nmax <- max(str_count(scopus_toappend$Standard.Authors, ",")) + 1
  scopus_toappend <- scopus_toappend %>%
    separate(Standard.Authors, into = paste0("author", seq_len(nmax)), sep=", ")
  
  scopus_toappend <- subset(scopus_toappend, select=c(Authors, Title, Year, Source.Title, DOI, Link, Abstract, Author.Keywords,
                                                      Index.Keywords, Document.Type, Open.Access, Source, Source.ID, author1, 
                                                      author2, author3, author4, author5, author6, author7, author8, author9,
                                                      author10, author11, author12, author13, author14, author15, author16, 
                                                      author17, author18, author19, author20, author21, author22, author23, 
                                                      author24, author25, author26, author27, author28, author29, author30,
                                                      author31, author32, author33, author34, author35, author36, author37,
                                                      author38, author39, author40, author41, author42, author43, author44,
                                                      author45, author46, author47, author48, author49, author50))
  
  author_vars <- grep("author", colnames(scopus_toappend), value=TRUE)
  
  for (var in author_vars) {
    scopus_toappend[[var]] <- gsub(" .*", "", scopus_toappend[[var]])
    scopus_toappend[[var]] <- tolower(scopus_toappend[[var]])
  }
  
  author_vars2 <- author_vars[2:50]
  
  scopus_toappend$Standard.Authors <- scopus_toappend$author1
  
  for (var in author_vars2) {
    scopus_toappend$Standard.Authors <- paste(scopus_toappend$Standard.Authors,", ",scopus_toappend[[var]], sep="")
  }
  
  scopus_toappend$Standard.Authors <- gsub(", NA.*", "", scopus_toappend$Standard.Authors)
  
  scopus_toappend <- subset(scopus_toappend, select=c(Authors, Standard.Authors, Title, Year, Source.Title, DOI, Link, Abstract, 
                                                      Author.Keywords, Index.Keywords, Document.Type, Open.Access, Source, Source.ID))
  
### 2. Bring in WoS Data ----
  
  #### 2.1 Initial  Drops ----
    wos_full_extract_df <- read.csv("../Data/Original/Data-Extracts-WOS/webofscience_totalfinal.csv", 
                                    encoding = "UTF-8")
    
    wos_clean_df <- wos_full_extract_df %>% 
      distinct()
  
  #### 2.2 Check some fields and find duplicates ----
    sum(!is.na(wos_clean_df$Author.Keywords))
    sum(!is.na(wos_clean_df$Keywords.Plus))
    
    wos_clean_df <- wos_clean_df %>%
      separate(UT..Unique.WOS.ID., c("Source", "WOS.ID"), sep=":")
    
    length(unique(wos_clean_df$WOS.ID))
    wos_clean_df <- wos_clean_df[order(wos_clean_df$WOS.ID),]
    wos_clean_df$dupes <- duplicated(wos_clean_df$WOS.ID) | duplicated(wos_clean_df$WOS.ID, fromLast=TRUE)
  
  #### 2.3 Second Set of Drops ----
    wos_clean_df <- subset(wos_clean_df, select=c(Publication.Type, Authors, Article.Title, Source.Title, Author.Keywords,
                                                  Keywords.Plus, Abstract, Publication.Year, DOI,
                                                  Open.Access.Designations, Source, WOS.ID)) # No Link
    
    wos_clean_df <- rename(wos_clean_df, Document.Type = Publication.Type, Year = Publication.Year, Source.ID = WOS.ID, 
                           Title = Article.Title, Index.Keywords = Keywords.Plus, Open.Access = Open.Access.Designations)
    
    wos_clean_df <- wos_clean_df %>% 
      distinct() 
  
  #### 2.4 Prep for Append - Standardize Authors ----
    wos_toappend <- wos_clean_df
    wos_toappend$Standard.Authors <- wos_toappend$Authors
    
    nmax <- max(str_count(wos_toappend$Standard.Authors, "; "), na.rm=T)
    
    wos_toappend <- wos_toappend %>%
      separate(Standard.Authors, into = paste0("author", seq_len(nmax)), sep="; ")
    
    wos_toappend <- subset(wos_toappend, select=c(Authors, Title, Year, Source.Title, DOI, Abstract, Author.Keywords,
                                                  Index.Keywords, Document.Type, Open.Access, Source, Source.ID, author1, 
                                                  author2, author3, author4, author5, author6, author7, author8, author9,
                                                  author10, author11, author12, author13, author14, author15, author16, 
                                                  author17, author18, author19, author20, author21, author22, author23, 
                                                  author24, author25, author26, author27, author28, author29, author30,
                                                  author31, author32, author33, author34, author35, author36, author37,
                                                  author38, author39, author40, author41, author42, author43, author44,
                                                  author45, author46, author47, author48, author49, author50))   
    
    for (var in author_vars) {
      wos_toappend[[var]] <- gsub(", .*", "", wos_toappend[[var]])
      wos_toappend[[var]] <- tolower(wos_toappend[[var]])
    }
    
    wos_toappend$Standard.Authors <- wos_toappend$author1
    
    for (var in author_vars2) {
      wos_toappend$Standard.Authors <- paste(wos_toappend$Standard.Authors,", ",wos_toappend[[var]], sep="")
    }
    
    wos_toappend$Standard.Authors <- gsub(", NA.*", "", wos_toappend$Standard.Authors)
    
    wos_toappend <- subset(wos_toappend, select=c(Authors, Standard.Authors, Title, Year, Source.Title, DOI, Abstract, 
                                                  Author.Keywords, Index.Keywords, Document.Type, Open.Access, Source, Source.ID))  
  
### 3. Combine WoS and Scopus ----
  rm(scopus_full_extract_df, wos_full_extract_df)
  
  #### 3.1 Append ----
    full_df <- bind_rows(scopus_toappend, wos_toappend)
    rm(scopus_clean_df, wos_clean_df, author_vars, author_vars2, nmax, scopus_files, var)
  
  #### 3.2 Standardize Titles & Sources ----
    full_df$Standard.Title <- full_df$Title
    full_df$Standard.Title <- gsub("'", "", full_df$Standard.Title)
    full_df$Standard.Title <- gsub("\"", "", full_df$Standard.Title)
    full_df$Standard.Title <- tolower(full_df$Standard.Title)
    full_df$Standard.Title <- gsub(" ", "", full_df$Standard.Title)
    
    full_df$Standard.Source.Title <- full_df$Source.Title
    full_df$Standard.Source.Title <- tolower(full_df$Standard.Source.Title) 
    full_df$Standard.Source.Title <- gsub(" ", "", full_df$Standard.Source.Title)
    
    full_df <- full_df[,c("Standard.Title", "Standard.Authors", "Standard.Source.Title", "Year", "Source", "Title", "Authors", 
                          "Source.Title", "Document.Type", "DOI", "Link", "Abstract", "Author.Keywords", "Index.Keywords", 
                          "Open.Access", "Source.ID")] 
    
    full_df <- full_df[order(full_df$Standard.Title),]

  #### 3.3 Additional Cleaning ---- 
    # Source IDs are causing some duplicates - let's drop them
    full_df <- subset(full_df, select=-c(Source.ID))  
    full_df <- full_df %>%
      distinct()  # 366,031
  
  ####  3.4 Identify All Duplicates (Title, Source, Year) - Removed Author ----
    full_df$dup <- duplicated(full_df[,c("Standard.Title", "Standard.Source.Title", "Year")]) | 
      duplicated(full_df[,c("Standard.Title", "Standard.Source.Title", "Year")], fromLast=TRUE)
  
  ####  3.5 Identify Dups between Scopus & WoS and Keep Scopus Version ----
    dupes <- full_df[full_df$dup==TRUE,]    
  
    dupes <- dupes %>%
      group_by(Standard.Title, Source) %>%
      summarize(Count = sum(dup))
    
    dupes <- spread(dupes, key=Source, value=Count)
    
    dupes$true_dup <- 0
    dupes$true_dup[!is.na(dupes$Scopus) & !is.na(dupes$WOS)] <- 1 # True dups have both Scopus and WoS observations
    
    length(unique(dupes$Standard.Title)) # Just to confirm we can merge with this
    
    full_df <- merge(full_df, dupes, all.x=TRUE)
    
    full_df$drop <- 0
    full_df$drop[full_df$true_dup==1 & full_df$Source=="WOS"] <- 1
    
    full_df <- subset(full_df, full_df$drop==0)
  
  ####  3.6 Identify and Drop Remaining Dups (Keep 1st Observation) ----
    full_df <- full_df[order(full_df$Standard.Title, full_df$Source),] # sort again
    full_df$dup_2 <- duplicated(full_df[,c("Standard.Title", "Standard.Source.Title", "Year")])
    full_df <- subset(full_df, full_df$dup_2==FALSE) 
    
  #### 3.7 Check ----
    n_distinct(full_df[,c("Standard.Title", "Standard.Source.Title", "Year")]) 
  
  #### 3.8 Export ----
    full_df$Source[full_df$true_dup==1] <- "Scopus & WOS"
    table(full_df$Source)
    full_df <- subset(full_df, select=-c(dup, Scopus, WOS, true_dup, drop, dup_2))
    
    # write.csv(full_df, "../Data/Analysis_Data/01_Combined_Data.csv",
    #           fileEncoding = "UTF-8")
