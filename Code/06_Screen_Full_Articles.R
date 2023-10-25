# ------------------------------------------------------------------------------
# Program Name: 06_Screen_Full_Articles.R
# Program Purpose: Test full article screening methods and apply to all retrievable articles to screen for eligibility

# Note: Set working directory to local version of GitHub repository "Code" folder
# Note: We do not include the PDF files for articles, but we do include lists of the article PDFs that were used in each code section in the "Data" Folder:
  # Section 1 (1st Test Sample) - "Cities-and-regions-tackle-climate-change-mitigation\Data\Analysis\06_Screening_Sample1.csv"
  # Section 2 (2nd Test Sample) - "Cities-and-regions-tackle-climate-change-mitigation\Data\Analysis\06_Screening_Sample2_OOS.csv"
  # Section 3 (All Retrievable Articles) - "Cities-and-regions-tackle-climate-change-mitigation\Data\Analysis\06_All_Retrieved_Articles_List.csv"

# Author: Katherine Burley
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
# ------------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)
library(tidytext)
library(igraph)
library(ggraph)
# library(ClimActor)
# install.packages("pdftools") # We need this to extract text from the PDFs!
library("pdftools")
library(stringr)
# install.packages("tokenizers")
library(tokenizers)

#### 1. Test Sample of Articles ----
  # Bring in Articles
  # 
  articles_sample <- list.files("", pattern="\\.pdf$") # Location of PDF files on local computer

  # Bring in article text 
  articles_text1 <- lapply(articles_sample, pdftools::pdf_text)

  # For each article, combine all pages into character vector of length 1
  articles_text <- lapply(articles_text1, function(x) tolower(x))
  articles_text <- lapply(articles_text, function(x) paste(x ,collapse=" "))
  articles_text <- lapply(articles_text, function(x) str_squish(x))
  articles_text <- do.call("rbind", articles_text)
  # articles.df <- data.frame()
  rm(articles_text1)
  
  # Convert to Dataframe
  articles.df <- data.frame(file_name = articles_sample, text = articles_text)
  rm(articles_text, articles_sample)
  
  # Remove Reference Sections from Articles - TESTING OUT
  articles.df <- articles.df %>%
    mutate(locations = str_locate_all(text, "references")) # Find where they say "references

  # articles.df$last_loc <- apply(articles.df["locations"], 2, function(y) apply(y, 1, function(x) paste(max(unlist(x), na.rm=FALSE))))
  articles.df$last_loc <- apply(articles.df["locations"], 1, function(x) paste(max(unlist(x), na.rm=FALSE)))
    # Identify location of last occurrence of "references"
  
  articles.df <- articles.df %>%
    mutate(last_loc = as.numeric(last_loc)) # Must convert first so that we can filter based on last_loc
  
  # Check the ratio of punctuation before and after "references" as a check to make sure we don't remove text from articles w/o references sections
  articles.df <- articles.df %>%
    filter(last_loc!=-Inf) %>% # these articles do not say "references" at all, so we won't modify them
    mutate(ref_begin = last_loc-2010,
           ref_end = last_loc+2001) %>% # different lengths to account for the word "references"
    mutate(before_ref = substring(text, ref_begin, last_loc-10),
           after_ref = substring(text, last_loc+1, ref_end)) %>% # extract 2000 characters before and after "references" 
    mutate(before_words = count_words(before_ref),
           after_words = count_words(after_ref)) %>% # count words
    mutate(before_punct = str_count(before_ref, pattern="[[:punct:]]+"),
           after_punct = str_count(after_ref, pattern="[[:punct:]]+")) %>% # count punctuation
    mutate(before_ratio = before_punct/before_words,
           after_ratio = after_punct/after_words,
           diff = after_ratio-before_ratio) %>% # calculate ratios & the difference
    
    # Rule: Do not remove references section if the difference in punctuation ratio before and after the last occurrence of "references" is <6%
    mutate(text = case_when(between(diff, -0.06, 0.06) ~ text,
                            TRUE ~ substring(text, 1, last_loc))) %>%
    bind_rows(articles.df[articles.df$last_loc==-Inf,]) %>%
    select(c(file_name, text))

  # Identify those that mention "co2"
  articles.df <- articles.df %>%
    filter(str_detect(text, "co2")) %>%
    # subset(select=-c(text)) %>%
    separate(file_name, into=c("rm1", "unique_id", "rm2", "rm3"), sep="_") %>%
    select(-c(rm1, rm2, rm3)) %>%
    mutate(unique_id = as.integer(unique_id))
  
  # Look for numbers & positive words around all references to "CO2"
  test <- articles.df %>%
    mutate(text2 = str_extract_all(text, ".{40}co2.{40}")) %>%
    select(c(unique_id, text2))
  
  test <- test %>%
    mutate(total_refs = lengths(text2)) 
  
  splits <- max(test$total_refs, na.rm=TRUE)+1
  
  test <- test %>%
    separate(text2, into = paste0("ref", seq_len(splits)), sep="\",") 
  
  names <- colnames(test)
  names <- names[grepl("^ref", names)]
  
  positive_words <- c(
    "reduc",  "^kt", "^kg","^mt", "^ton", "^e$", "yr$", "equivalent"
  )
  
  test_lng <- test %>%
    gather(key="ref_no", value="co2_reference", names) %>% 
    filter(co2_reference!="") %>%
    mutate(across(co2_reference, ~gsub("co2", "", .x))) %>% # remove co2 bc it already has a number
    filter(str_detect(co2_reference, regex(paste0(positive_words, collapse = "|"),ignore_case = T))) %>% # look for positive words in the characters around co2
    filter(grepl("\\d+", co2_reference)) # look for any numbers in the characters around co2
  
  # TESTING: Filter to articles that contain more than one hit for numbers + positive words around "co2"
  sample_count_1 <- test_lng %>%
    count(unique_id) %>%
    filter(n>1)
    
  # Check Against Screened Sample Results
  full_sample <- read.csv("../Data/Testing/Manual_Screening_Sample.csv",
                          header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8")

  # Quick Analysis of Words Around CO2
  filtered <- full_sample %>%
    mutate(matched = unique_id %in% sample_count_1$unique_id)
    
  check <- filtered %>%
    count(include, matched)
  
#### 2. Test Performance Out-of-Sample ----
    # Bring in Articles
    articles_sample <- list.files("", pattern="\\.pdf$") # Location of PDF files on local computer

    # Bring in article text 
    articles_text1 <- lapply(articles_sample, pdftools::pdf_text)

    # For each article, combine all pages into character vector of length 1
    articles_text <- lapply(articles_text1, function(x) tolower(x))
    articles_text <- lapply(articles_text, function(x) paste(x ,collapse=" "))
    articles_text <- lapply(articles_text, function(x) str_squish(x))
    articles_text <- do.call("rbind", articles_text)
    rm(articles_text1)
    
    # Convert to Dataframe
    articles.df <- data.frame(file_name = articles_sample, text = articles_text)
    rm(articles_text, articles_sample)
    
    # Remove Reference Sections from Articles - TESTING OUT
    articles.df <- articles.df %>%
      mutate(locations = str_locate_all(text, "references")) # Find where they say "references
    
    # Identify location of last occurrence of "references"
    articles.df$last_loc <- apply(articles.df["locations"], 1, function(x) paste(max(unlist(x), na.rm=FALSE)))
    
    articles.df <- articles.df %>%
      mutate(last_loc = as.numeric(last_loc)) # Must convert first so that we can filter based on last_loc
    
    # Check the ratio of punctuation before and after "references" as a check to make sure we don't remove text from articles w/o references sections
    articles.df <- articles.df %>%
      filter(last_loc!=-Inf) %>% # these articles do not say "references" at all, so we won't modify them
      mutate(ref_begin = last_loc-2010,
             ref_end = last_loc+2001) %>% # different lengths to account for the word "references"
      mutate(before_ref = substring(text, ref_begin, last_loc-10),
             after_ref = substring(text, last_loc+1, ref_end)) %>% # extract 2000 characters before and after "references" 
      mutate(before_words = count_words(before_ref),
             after_words = count_words(after_ref)) %>% # count words
      mutate(before_punct = str_count(before_ref, pattern="[[:punct:]]+"),
             after_punct = str_count(after_ref, pattern="[[:punct:]]+")) %>% # count punctuation
      mutate(before_ratio = before_punct/before_words,
             after_ratio = after_punct/after_words,
             diff = after_ratio-before_ratio) %>% # calculate ratios & the difference
      # Rule: Do not remove references section if the difference in punctuation ratio before and after the last occurrence of "references" is <6%
      mutate(text = case_when(between(diff, -0.06, 0.06) ~ text,
                              TRUE ~ substring(text, 1, last_loc))) %>%
      bind_rows(articles.df[articles.df$last_loc==-Inf,]) %>%
      select(c(file_name, text))
    
    # Identify those that mention "co2"
    articles.df <- articles.df %>%
      filter(str_detect(text, "co2")) %>%
      separate(file_name, into=c("rm1", "unique_id", "rm2", "rm3"), sep="_") %>%
      select(-c(rm1, rm2, rm3)) %>%
      mutate(unique_id = as.integer(unique_id))
    
    # Look for numbers around all references to "CO2"
    test <- articles.df %>%
      mutate(text2 = str_extract_all(text, ".{40}co2.{40}")) %>%
      select(c(unique_id, text2))
    
    test <- test %>%
      mutate(total_refs = lengths(text2)) 
    
    splits <- max(test$total_refs, na.rm=TRUE)+1
    
    test <- test %>%
      separate(text2, into = paste0("ref", seq_len(splits)), sep="\",") 
    
    names <- colnames(test)
    names <- names[grepl("^ref", names)]
    
    positive_words <- c(
      "reduc",  "^kt", "^kg","^mt", "^ton", "^e$", "yr$", "equivalent"
    )
    
    test_lng <- test %>%
      gather(key="ref_no", value="co2_reference", names) %>% # change this every time you change the strategy ref1:ref184
      filter(co2_reference!="") %>%
      mutate(across(co2_reference, ~gsub("co2", "", .x))) %>% # remove co2 bc it already has a number
      filter(str_detect(co2_reference, regex(paste0(positive_words, collapse = "|"),ignore_case = T))) %>% # look for positive words in the characters around co2
      filter(grepl("\\d+", co2_reference)) # look for any numbers in the characters around co2
    
    # TESTING: Filter to articles that contain more than one hit for numbers + positive words around "co2" 
    sample_count_2 <- test_lng %>%
      count(unique_id) %>%
      filter(n>1)    
    
    # Check Against Screened Sample Results
    full_sample_oos <- read_excel("../Data/Testing/Manual_Screening_Sample_OOS_Test.xlsx",
                            col_names=TRUE)
    
    names(full_sample_oos) <- tolower(names(full_sample_oos))
    
    full_sample_oos <- full_sample_oos %>%
      rename(quantitative_impacts = "quantitative impacts?",
             emissions_mitigation_strategy = "emissions mitigation strategy?",
             subnat_context = "subnational context?",
             orig_research = "original research?") %>%
      distinct(unique_id, include, quantitative_impacts, emissions_mitigation_strategy, subnat_context, orig_research)
    
    full_sample_oos <- full_sample_oos %>%
      distinct(unique_id, include)
    
    include <- full_sample_oos %>%
      filter(include=="Yes")
    
    # Quick Analysis of Words Around CO2
    filtered_oos <- full_sample_oos %>%
      mutate(matched = unique_id %in% sample_count_2$unique_id)
    
    check_oos <- filtered_oos %>%
      count(include, matched)
    
#### 3. Filter All Articles ----
  rm(list=ls())
  articles_all <- list.files("C:/Article_Downloads", pattern="\\.pdf$") # Location of PDF files on local computer
  
  setwd("C:/Article_Downloads")

  articles_text1 <- lapply(articles_all, pdftools::pdf_text)
  
  # For each article, combine all pages into character vector of length 1
  articles_text1 <- lapply(articles_text1, function(x) tolower(x))
  articles_text1 <- lapply(articles_text1, function(x) paste(x ,collapse=" "))
  articles_text1 <- lapply(articles_text1, function(x) str_squish(x))
  articles_text <- do.call("rbind", articles_text1)
  rm(articles_text1)

  # Convert to Dataframe
  articles.df <- data.frame(file_name = articles_all, text = articles_text)
  rm(articles_text)
  
  # Remove Reference Sections from Articles - TESTING OUT
  articles.df <- articles.df %>%
    mutate(locations = str_locate_all(text, "references")) # Find where they say "references
  
  # articles.df$last_loc <- apply(articles.df["locations"], 2, function(y) apply(y, 1, function(x) paste(max(unlist(x), na.rm=FALSE))))
  articles.df$last_loc <- apply(articles.df["locations"], 1, function(x) paste(max(unlist(x), na.rm=FALSE)))
  # Identify location of last occurrence of "references"
  
  articles.df <- articles.df %>%
    mutate(last_loc = as.numeric(last_loc)) # Must convert first so that we can filter based on last_loc
  
  # Check the ratio of punctuation before and after "references" as a check to make sure we don't remove text from articles w/o references sections
  articles.df <- articles.df %>%
    filter(last_loc!=-Inf) %>% # these articles do not say "references" at all, so we won't modify them
    mutate(ref_begin = last_loc-2010,
           ref_end = last_loc+2001) %>% # different lengths to account for the word "references"
    mutate(before_ref = substring(text, ref_begin, last_loc-10),
           after_ref = substring(text, last_loc+1, ref_end)) %>% # extract 2000 characters before and after "references" 
    mutate(before_words = count_words(before_ref),
           after_words = count_words(after_ref)) %>% # count words
    mutate(before_punct = str_count(before_ref, pattern="[[:punct:]]+"),
           after_punct = str_count(after_ref, pattern="[[:punct:]]+")) %>% # count punctuation
    mutate(before_ratio = before_punct/before_words,
           after_ratio = after_punct/after_words,
           diff = after_ratio-before_ratio) %>% # calculate ratios & the difference
    
  # Rule: Do not remove references section if the difference in punctuation ratio before and after the last occurrence of "references" is <6%
  mutate(text = case_when(between(diff, -0.06, 0.06) ~ text,
                          TRUE ~ substring(text, 1, last_loc))) %>%
  bind_rows(articles.df[articles.df$last_loc==-Inf,]) %>%
  select(c(file_name, text))

  # Identify those that mention "co2"
  articles.df <- articles.df %>%
    filter(str_detect(text, "co2")) %>%
    separate(file_name, into=c("rm1", "unique_id", "rm2", "rm3"), sep="_") %>%
    select(-c(rm1, rm2, rm3)) %>%
    mutate(unique_id = as.integer(unique_id))
  
  # Filter based on the text around all references to "CO2" (numbers + positive words)
  test <- articles.df %>%
    mutate(text2 = str_extract_all(text, ".{40}co2.{40}")) %>%
    select(c(unique_id, text2))
  
  test <- test %>%
    mutate(total_refs = lengths(text2)) 
  
  splits <- max(test$total_refs, na.rm=TRUE)+1
  
  test <- test %>%
    separate(text2, into = paste0("ref", seq_len(splits)), sep="\",") 
  
  names <- colnames(test)
  names <- names[grepl("^ref", names)]
  
  positive_words <- c(
    "reduc",  "^kt", "^kg","^mt", "^ton", "^e$", "yr$", "equivalent"
  )
  
  test_lng <- test %>%
    gather(key="ref_no", value="co2_reference", names) %>% 
    filter(co2_reference!="") %>%
    mutate(across(co2_reference, ~gsub("co2", "", .x))) %>% # remove co2 bc it already has a number
    filter(str_detect(co2_reference, regex(paste0(positive_words, collapse = "|"),ignore_case = T))) %>% # look for positive words in the characters around co2
    filter(grepl("\\d+", co2_reference)) # look for any numbers in the characters around co2
  
  # Filter to articles that contain more than one hit for numbers + positive words around "co2" 
  articles.final <- test_lng %>%
    count(unique_id) %>%
    filter(n>1)    
  
  # Export 
  write.csv(articles.final, "../Data/Analysis/06_Filtered_Articles.csv",
            fileEncoding = "UTF-8", row.names = F)
  
#### 4. Create files for data extraction ----
  final_article_ids <- read.csv("../Data/Analysis/06_Filtered_Articles.csv",
                         header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8")
  
  previous_list <- read.csv("../Data/Analysis/04_Abstract_Filtering_Results.csv",
                            header=T, as.is=T, stringsAsFactors = FALSE, encoding = "UTF-8")
  
  all_sample <- full_sample %>%
    bind_rows(full_sample_oos) %>%
    select(-impacts) %>%
    filter(include=="Yes") # Keep Articles Included From Sample Testing
  
  final_list <- previous_list %>%
    filter(unique_id %in% final_article_ids$unique_id) %>%
    select(c(unique_id, Year, Title, Authors, City.Term, Region.Term, DOI))
  
  final_list <- full_join(final_list, all_sample, by="unique_id")
  
  final_list <- final_list %>%
    mutate(Sample = case_when(!is.na(include) ~ "Yes",
                                TRUE ~ "No")) %>%
    mutate(Reviewed = case_when(!is.na(include) ~ "Yes",
                                TRUE ~ "No")) %>%
    mutate(Extracted = include) %>%
    mutate(From_Final_List = "Yes") %>%
    rename("Included" = "include") %>%
    select(unique_id, Year, Title, Authors, City.Term, Region.Term, DOI, From_Final_List, Sample, Reviewed, Included, Extracted)
  
  # write.csv(final_list, "../Data/Analysis/06_Data_Extraction_Article_List.csv",
  #           fileEncoding="UTF-8", row.names=F) # Export CSV as a record
  # write_xlsx(final_list, "../Data/Analysis/06_Data_Extraction_Article_List.xlsx") # Export excel as the data extraction file and tracker
  
  # At this point, manual screening was completed on remaining articles.
  
  


