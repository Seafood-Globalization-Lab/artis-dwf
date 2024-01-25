# Script to test EEZ standardization for ingesting new SAU data
library(readr)
library(magrittr)
library(tidyverse)
library(countrycode)
library(DBI)

# load new data - AM from ARTIS/QA/outputs/
# this data only contains species & countries (not EEZ) standardized SAU marine capture
prod_sau <- read_csv("./data/standardized_sau_prod.csv")
# existing SAU data in ARTIS
artis_sau <- read.csv("data/SAU_ARTIS_2010-2020.csv")

# number of 2010-2020 iso3 codes
length(unique(artis_sau$importer_iso3c))

# number of iso3 codes in standardized_sau_prod.csv
length(unique(prod_sau$country_iso3_alpha))

# iso3 codes in artis_sau NOT in prod_sau
setdiff(unique(artis_sau$importer_iso3c), unique(prod_sau$country_iso3_alpha))

# iso3 codes in prod_sau NOT in artis_sau
setdiff(unique(prod_sau$country_iso3_alpha), unique(artis_sau$importer_iso3c))
# "SCG" - SCG (Serbo-Croatian: Srbija i Crna Gora), the ISO 3166-1 alpha-3 code for the former union of Serbia and Montenegro, which peacefully split in 2006 into two countries

# run EEZ standardize script
source("./scripts/standardize_sau.R")

# Add on cleaning chunk step by step to understand exactly what is happening
test1 <- prod_sau %>% 
  # split compound eez into 2 columns by parentheses and remove
  separate(eez, into = c("eez_1", "eez_2"), sep = "\\(", remove = FALSE) %>%
  mutate(eez_2 = gsub("\\)", "", eez_2)) %>%
  # use countrycode package to transform original country name to iso3 code
  mutate(eez_iso3c = countrycode(eez, origin = "country.name", destination = "iso3c")) %>% 
  # Deal with NAs (didn't match with countrycode()) - 
  mutate(eez_iso3c = case_when(
    # non NA values remain the same
    (!is.na(eez_iso3c)) ~ eez_iso3c, 
    # NAs - use first country name in split eez data to convert to iso3 code
    (is.na(eez_iso3c) ~ countrycode(eez_1, origin = "country.name", destination = "iso3c")))) %>%
  mutate(eez_iso3c = case_when(
    # non NA values remain the same
    (!is.na(eez_iso3c)) ~ eez_iso3c, 
    # Remaining NAs use second country name in split which was in parentheses
    (is.na(eez_iso3c) ~ countrycode(eez_2, origin = "country.name", destination = "iso3c")))) %>%
  # Manually correct know instance that is not captured in code above
  mutate(eez_iso3c = case_when(
    (eez == "US Virgin Isl.") ~ "USA",
    TRUE ~ eez_iso3c
  )) %>% 
  # create new column of country names based on the iso3 codes - standardizes nomenclature
  mutate(eez_current_name = countrycode(eez_iso3c, origin = "iso3c", destination = "country.name"))

# Small test

# rows not dealt with in cleaning code
test3 <- test1 %>% 
  filter(is.na(eez_iso3c))

# adds artis_iso3 and artis_country_name columns - runs manual corrections eez corrections to standardize to ARTIS nomenclature
test2 <- standardize_sau_eez(test1, "eez_iso3c", "eez_current_name")

# rows not dealt with after standardizing function - use to update function code
test4 <- test2 %>% 
  filter(is.na(artis_iso3))

# what instances are not matching with artis country names
test5 <- test2 %>% 
  filter(is.na(artis_country_name))

# query local ARTIS database version to get data
con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("DB_NAME"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("DB_USERNAME"),
                 password=Sys.getenv("DB_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

# get distinct iso3 codes for exporters, importers, and producers from ARTIS database
exporters <- dbGetQuery(con, 
                        "SELECT DISTINCT(exporter_iso3c) AS iso3c FROM all_estimates_snet")
importers <- dbGetQuery(con, 
                        "SELECT DISTINCT(importer_iso3c) AS iso3c FROM all_estimates_snet")
producers <- dbGetQuery(con, 
                        "SELECT DISTINCT(source_country_iso3c) AS iso3c FROM all_estimates_snet")

# Close database connection
dbDisconnect(con)
rm(list = c("con"))

# make vector of EEZs currently in ARTIS database
artis_iso3c <- unique(c(exporters$iso3c, importers$iso3c, producers$iso3c))

# what SAU data EEZs (standardized to ARTIS nomencalture) are not currently in the ARTIS database
test6 <- test2 %>% 
  filter(!(artis_iso3 %in% artis_iso3c))

# get relevant eez information about these left over territories 
remaining_teritories <- test6 %>% 
  select(eez_iso3c, artis_iso3, artis_country_name, eez_current_name) %>% 
  distinct()     