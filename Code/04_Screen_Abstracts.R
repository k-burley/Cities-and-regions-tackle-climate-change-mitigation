# ------------------------------------------------------------------------------
# Program Name: 04_Screen_Abstracts.R
# Program Purpose: Filter articles based on abstracts using rules and bigram analysis

# Note: Set working directory to local version of GitHub repository "Code" folder
# Note: This code used ClimActor version 0.0.1

# Author: Zhi Ye Yeo, Katherine Burley
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
# ------------------------------------------------------------------------------

# 0. Set Up ----

## Clear R Environment ----
rm(list = ls())
R.version

## Libraries ----
library(tidyverse)
library(readxl)
library(tidytext)
library(igraph)
library(ggraph)

# 1. Read in both sets of data 
og <- read_excel("../Data/Analysis/03_First_Round_Data1.xlsx",
                 sheet = "03_First_Round_Data1")
og2 <- read_excel("../Data/Analysis/03_First_Round_Data2.xlsx",
                  sheet = "03_First_Round_Data2")
df <- bind_rows(og, og2)

rm(og, og2)
gc()

# 2. Rules Based Filtering ----

## De-duplicate the datasets by removing all special characters and spaces ----
df <- df %>% filter(!duplicated(gsub("[^a-zA-Z0-9]", "", tolower(df$Title))))

## Eliminate those that are older than 2010 ----
df <- df %>%
  filter(as.numeric(Year) >= 2010)

## check for those with actors names only after the copyright symbol ----
df <- df %>%
  separate(Abstract, into = c("pre_copyright", "post_copyright"), 
           sep = "©", remove = F) %>%
  mutate(entity.term = coalesce(City.Term, Region.Term),
         flag = str_detect(post_copyright, fixed(entity.term)) & !str_detect(pre_copyright, fixed(entity.term))) %>%
  filter(!flag|is.na(flag)) # remove those that have the entity term only post copyright sign

## Filter out articles that contain the stopwords in the abstract and title ----
stopwords <- c(
  "air pollut", "PM2.5", "meta-analysis", "literature review",
  "the proceedings contain(s){0,1}", "isotope", "meteorolog",
  "corrigendum", "erratum", "Correction", "bacteria", "oxid",
  "health"
)

df <- df %>%
  filter(!(str_detect(Abstract, regex(paste0(stopwords, collapse = "|"), ignore_case = T))|
             str_detect(Title, regex(paste0(stopwords, collapse ="|"), ignore_case = T))))

# Filter out articles flagged on generic subnational terrms
general_entity <- c(
  "University", "Park", "LES", "^West$", "^Western$",
  "^East$", "^Eastern$", "^North$", "^Northern$", "^South$",
  "^Southern$"
)

df <- df %>%
  filter(!str_detect(df$entity.term, paste0(general_entity, collapse = "|")))

# Preserve current sample of included articles for validation
sample <- df[df$Include=="Yes" & !is.na(df$Include),]


# 3. Bigram Analysis ---- 

## Generate bigrams ----
bigram <- df %>%
  unnest_tokens(bigram, Abstract, token = "ngrams", n = 2)

bigrams_count <- bigram %>%
  group_by(Title) %>%
  count(bigram, sort = T)

## Remove those with stop words and weird combinations ----
bigrams_sep <- bigrams_count %>%
  separate(bigram, c("bigram_1", "bigram_2"), sep = " ")

bigrams_sep <- bigrams_sep %>%
  filter(!bigram_1 %in% stop_words$word) %>%
  filter(!bigram_2 %in% stop_words$word)

bigrams_sep <- bigrams_sep %>% 
  filter(bigram_1 != "ã") %>%
  filter(bigram_1 != "â") %>%
  filter(bigram_2 != "ã") %>%
  filter(bigram_2 != "â")

## Unite the bigrams and see which bigrams appeared the most in documents ----
bigrams_combined <- bigrams_sep %>%
  unite("bigram", bigram_1, bigram_2, sep = " ") %>%
  group_by(bigram) %>%
  count(bigram, sort = T)

## Remove publishing related terms ----
bigrams_combined <- bigrams_combined %>%
  filter(!str_detect(bigram, "rights reserved|elsevier|springer|verlag|berlin"))

## Use positive words to filter for relevant bigrams to keep ----

topical_words <- c("climate", "greenhouse", "gas", "ghg", "mitigat",
                   "carbon", "CO2", "CO 2", "CH4", "CH 4", "methane",
                   "N20", "N 2 O", "N2 O", "energy", "electricity",
                   "fossil", "fuel")

positive_words <- c(
  "emission(s){0,1}", "avoid", "effective", "abate", 
  "reduc", "quanti", "efficie", "impact"
)

bigrams_keep <- bigrams_combined %>%
  filter(str_detect(bigram, regex(paste0(topical_words, collapse = "|"),
                                  ignore_case = T))) %>%
  filter(str_detect(bigram, regex(paste0(positive_words, collapse = "|"),
                                  ignore_case = T)))

rm(bigram, bigrams_count, bigrams_sep)
gc()


## Now use the bigrams to filter the main abstracts ----
final <- df %>%
  filter(str_detect(Abstract, regex(paste0(bigrams_keep$bigram, collapse = "|"),
                                    ignore_case = T)))

filtered <- final %>%
  filter(final$unique_id %in% sample$unique_id) # Using ID rather than title

sum(sample$Include == "Yes")
sum(filtered$Include == "Yes", na.rm = T)
sum(sample$Year >= 2010 & sample$Include == "Yes")

# 4. Export ----
write.csv(final, "../Data/Analysis/04_Abstract_Filtering_Results.csv",
          fileEncoding = "UTF-8", row.names = F)

