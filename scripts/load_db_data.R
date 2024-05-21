# Load database data

# Load packages
library(dplyr)
library(countrycode)
library(exploreARTIS)
library(DBI)


# Connect and Pull Heroku ARTIS database ----------------------------------

# database connection to local Postgres SQL - Heroku server version
con_heroku <- dbConnect(RPostgres::Postgres(),
                        dbname=Sys.getenv("HEROKU_DB_NAME"),
                        host=Sys.getenv("HEROKU_HOST"),
                        #server=Sys.getenv("DB_SERVER"),
                        port=Sys.getenv("HEROKU_DB_PORT"),
                        user=Sys.getenv("HEROKU_DB_USERNAME"),
                        password=Sys.getenv("HEROKU_DB_PW"))

# Check that connection is established by checking which tables are present
dbListTables(con_heroku)

#FIXIT - this is not a correct query check example
consumption <- dbGetQuery(con_heroku, 
                         "SELECT * FROM consumption
                         WHERE year >= 2016
                         AND habitat = 'marine'
                         AND method = 'capture'
                         AND consumption_source = 'foreign'
                         AND consumer_iso3c = 'BLZ'")

# Close database connection
dbDisconnect(con_heroku)
rm(list = c("con_heroku"))

# Connect and Pull Local ARTIS Database -----------------------------------

# Database connection - needs .Rproj at root directory to run .Renviron with new R session
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname=Sys.getenv("DB_NAME"),
#                  host=Sys.getenv("DB_HOST"),
#                  port=Sys.getenv("5432"),
#                  user=Sys.getenv("DB_USERNAME"),
#                  password=Sys.getenv("DB_PASSWORD"))

# SAU Production dataframe
# prod_sau <- dbGetQuery(con, "SELECT * FROM sau_production") %>%
#   select(-record_id) 
# 
# # Species metadata
# sciname_metadata <- dbGetQuery(con, "SELECT * FROM sciname") %>%
#   select(-record_id)
# 
# # Maximum taxa resolution 
# code_max_resolved_taxa <- dbGetQuery(con, "SELECT * FROM code_max_resolved_taxa") %>%
#   select(-record_id) %>%
#   mutate(hs6 = case_when(
#     str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
#     TRUE ~ hs6
#   )) %>%
#   mutate(hs_version = case_when(
#     str_length(hs_version) == 1 ~ paste("0", hs_version, sep = ""),
#     TRUE ~ hs_version
#   )) %>%
#   mutate(hs_version = paste("HS", hs_version, sep = ""))
# 
# # Close database connection
# dbDisconnect(con)
# rm(list = c("con"))
