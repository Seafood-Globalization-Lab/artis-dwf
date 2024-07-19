# Build Distant Water Fleet Profiles for each country of interest

# loop through list of countries, knit the Rmd/Quarto file for each country, and save the output as individual HTML reports.


# Load Packages & Create variables -----------------------------------------
library(dplyr)
library(countrycode)
library(exploreARTIS)
library(rmarkdown)
library(readr)
library(tidyr)
library(sqldf)
library(glue)
library(cleanrmd)
library(tufte)
library(stringr)
library(job)


# Create directories ------------------------------------------------------

outdir <- file.path(".", "output", fsep = "/")
scrdir <- file.path(".", "scripts", fsep = "/")
anndir <- file.path(".", "annotations", fsep = "/")

# Load Data -------------------------------------------------
#existing SAU data in ARTIS
# artis_sau <- read_csv(
#   file.path("data", "SAU_ARTIS_2010-2020.csv", 
#             fsep = "/"))

# load new SAU data - AM from ARTIS repo ./QA/outputs/
# Only contains species & countries standardized SAU marine capture (not EEZ) 
if(exists("prod_sau") == FALSE){
  prod_sau <- read_csv(
    file.path(".", "data", "standardized_sau_prod.csv",
              fsep = "/"))
}

# Read in local SAU consumption file (eventually replace with Heroku db)
if(exists("consumption") == FALSE){
  consumption <- read_csv(
    file.path(".", "data", "complete_consumption.csv")) 
}

# read in scientific name metadata (contains common names)
# sciname_metadata <- read_csv(
#     file.path(".", "data", "sciname_metadata.csv"))
sciname_metadata <- read_csv(
  file.path(".", "data", "sciname_metadata_heroku.csv"))


# Clean sciname_metadata --------------------------------------------------
# correct NAs found in sciname_metadata that are used in this analysis
sciname_common <- sciname_metadata %>%  
  select(sciname, common_name) %>% 
  add_row(sciname = "merluccius gayi", 
          common_name = "pacific hake") %>% # found in fb_common_to_sci
  add_row(sciname = "pentheroscion mbizi", 
          common_name = "blackmouth croaker") %>% # found in fb_common_to_sci
  add_row(sciname = "paralichthyidae", 
          common_name = "large-tooth flounder") %>% #fishbase website family
  add_row(sciname = "cephea",
          common_name = "cephea genus jelly") %>% 
  # replace common names with "=" with shortened version
  mutate(common_name = if_else(str_detect(common_name, "="), 
                               str_extract(common_name, "(?<=\\=)[^\\)]+"), 
                               common_name))

message("data import is complete")

# Run Scripts -------------------------------------------------------------

# load functions
source(file.path(scrdir, "functions.R", fsep = "/"))
source(file.path(scrdir, "fun_plot_sankey_annotate.R"))
# get record of flags of convenience  
source(file.path(scrdir, "scrape_foc.R", fsep = "/"))

# FIXIT: pull consumption data from Heroku server database
# source(file.path(".", "scripts", "load_db_data.R"))

# 1) standardize production SAU data
# 2) proportion of landings by producer captured in recorded source EEZs
# 3) disaggregate consumption by EEZ of catch - 
# Join production SAU and ARTIS SAU data 
if(exists("consumption_eez") == FALSE){
  source(file.path(".", "scripts", "clean_data.R"))
}


message("running clean_data.R is complete")

# Countries of interest ---------------------------------------------------

# Vector of Oceana countries:
#countries <- c("Belize", "Brazil", "Canada", "Chile", "Mexico", "Philippines", "Peru", "UK", "USA", "Spain", "Malaysia", "Ghana", "Senegal")
#countries <- c("Belize")
#countries <- c("Malaysia", "Belize", "Brazil", "Canada")
countries <- "Malaysia"

# Standardize country names
countries_std <- countrycode(countries,
                              origin = "country.name",
                              destination = "iso3c")
# for testing only
#countries_i <- countries_std

# Filter last 5 years of data ----------------------------
# uncomment to filter data by year & in consumption_eez_2
#max_year <- max(consumption_eez$year)
#last_x_yrs <- seq(max_year - 4, max_year, by = 1)

consumption_eez_2 <- consumption_eez %>% 
#  filter(year %in% last_x_yrs) %>% 
  # rename AM's cleaning data script column names that are confusing to align with standard ARTIS column names (clean_data.R)
  rename(producer_iso3c = source_country_iso3c,
         eez_iso3c = catch_artis_iso3,
         eez_name = catch_artis_country_name)

# Troubleshoot common name NAs ----------------------------
# create empty csv for recording common name NAs recorded in the Rmd
#file.create(file.path(outdir, "common_name_na.csv"))

# Build DWF profiles ---------------------------------------------

# Loop through focal countries - Build reports for each
for (i in 1:length(countries_std)) {
  countries_i <- countries_std[i]
  
#job::job({
  
  # is country flag of convenience
  foc_logic <- countries_i %in% itf_foc_std$country_iso3c

  message(glue::glue("started filtering {countries_i} data"))
  
  # 1) focal country DWF activities
  country_i_dwf <- consumption_eez_2 %>% 
    filter(producer_iso3c == countries_i) %>% 
    left_join(sciname_common, by = "sciname")

  # 2) focal country consumption of DWF catch
  country_i_consump <- consumption_eez_2 %>% 
    filter(consumer_iso3c == countries_i) %>% 
    left_join(sciname_common, by = "sciname")
    
  # 3) Fishing activity in focal country EEZ
  country_i_eez_fishing <- consumption_eez_2 %>% 
    filter(eez_iso3c == countries_i) %>% 
    left_join(sciname_common, by = "sciname")

  message(glue::glue("started rendering {countries_i} profile"))
    
  rmarkdown::render(
    input = "country_profile_template.Rmd",
    params = list(countries_i = countries_i,
                  country_i_dwf = country_i_dwf,
                  country_i_consump = country_i_consump,
                  country_i_eez_fishing = country_i_eez_fishing,
                  foc_logic = foc_logic), 
    output_dir = outdir,
    output_file = paste("dwf", countries_i, "profile.pdf", sep = "_")
  )
#}, title = paste0(countries_i, " ", Sys.time()))
  
}



