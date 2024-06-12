# Load database data

# Load packages
library(tidyverse)
library(countrycode)
library(exploreARTIS)
library(DBI)

# Initial database pulls

# Database connection - needs .Rproj at root directory to run .Renviron with new R session
con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("DB_NAME"),
                 host=Sys.getenv("localhost"),
                 port=Sys.getenv("5432"),
                 user=Sys.getenv("DB_USERNAME"),
                 password=Sys.getenv("DB_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

# SAU Production dataframe
prod_sau <- dbGetQuery(con, "SELECT * FROM sau_production") %>%
  select(-record_id) 

# Species metadata
sciname_metadata <- dbGetQuery(con, "SELECT * FROM sciname") %>%
  select(-record_id)

# Maximum taxa resolution 
code_max_resolved_taxa <- dbGetQuery(con, "SELECT * FROM code_max_resolved_taxa") %>%
  select(-record_id) %>%
  mutate(hs6 = case_when(
    str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
    TRUE ~ hs6
  )) %>%
  mutate(hs_version = case_when(
    str_length(hs_version) == 1 ~ paste("0", hs_version, sep = ""),
    TRUE ~ hs_version
  )) %>%
  mutate(hs_version = paste("HS", hs_version, sep = ""))

# Close database connection
dbDisconnect(con)
rm(list = c("con"))
