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


# Create directories ------------------------------------------------------

outdir <- file.path(".", "output", fsep = "/")
scrdir <- file.path(".", "scripts", fsep = "/")

# Load Data -------------------------------------------------
#existing SAU data in ARTIS
artis_sau <- read_csv(
  file.path("data", "SAU_ARTIS_2010-2020.csv", 
            fsep = "/"))

# load new SAU data - AM from ARTIS repo ./QA/outputs/
# Only contains species & countries standardized SAU marine capture (not EEZ) 
prod_sau <- read_csv(
  file.path(".", "data", "standardized_sau_prod.csv", 
            fsep = "/"))

# Read in local SAU consumption file (eventually replace with Heroku db)
consumption <- read_csv(
  file.path(".", "data", "complete_consumption.csv"))

# Run Scripts -------------------------------------------------------------

# load functions
source(file.path(".", "scripts", "functions.R", 
                 fsep = "/"))

# pull consumption data from Heroku server database
#source(file.path(".", "scripts", "load_db_data.R"))

# 1) standardize production SAU data
# 2) proportion of landings by producer captured in recorded source EEZs
# 3) disaggregate consumption by EEZ of catch - 
# Join production SAU and ARTIS SAU data 
source(file.path(".", "scripts", "clean_data.R"))

# Countries of interest ---------------------------------------------------

# Vector of Oceana countries:
# countries <- c("Belize", "Brazil", "Canada", "Chile", "Mexico", "Philippines", "Peru", "UK", "USA", "Spain", "Malaysia", "Ghana", "Senegal")
countries <- c("Belize")

# Standardize country names
countries_std <- countrycode(countries,
                              origin = "country.name",
                              destination = "iso3c")
# for testing only
countries_i <- countries_std

# Build DWF profiles ---------------------------------------------

# Filter last 5 years of data
max_year <- max(consumption_eez$year)
last_x_yrs <- seq(max_year - 4, max_year, by = 1)

consumption_eez_xyrs <- consumption_eez %>% 
  filter(year %in% last_x_yrs)

# Loop through focal countries - Build reports for each
for (i in 1:length(countries_std)) {
  countries_i <- countries_std[i]
  
  # 1) focal country DWF activities
  country_i_dwf <- consumption_eez_xyrs %>% 
    filter(source_country_iso3c == countries_i,
           dwf == "foreign")
  # domestic reference
  country_i_dom <- consumption_eez_xyrs %>% 
    filter(source_country_iso3c == countries_i,
           dwf != "foreign")
  
  # 2) focal country consumption of DWF catch
  country_i_dwf_consump <- consumption_eez_xyrs %>% 
    filter(consumer_iso3c == countries_i,
           dwf == "foreign")
  # domestic reference
  country_i_dom_consump <- consumption_eez_xyrs %>% 
    filter(consumer_iso3c == countries_i,
           dwf != "foreign")
    
  # 3) Fishing activity in focal country EEZ
  country_i_eez_fishing <- consumption_eez_xyrs %>% 
    filter(catch_artis_iso3 == countries_i,
           dwf == "foreign")
  # domestic reference
  country_i_dom_fishing <- consumption_eez_xyrs %>% 
    filter(catch_artis_iso3 == countries_i,
           dwf != "foreign")
  ########## vvv Replaced by code above
  # filter by single country of interest
  artis_eez_i <- artis_eez %>%
    filter(source_country_iso3c == countries_i)
  
  artis_sau_i <- artis_sau %>% 
    filter(habitat == "marine", 
           method == "capture", 
           year %in% year_int, 
           source_country_iso3c == countries_i)
  
  # filter consumption table by focal country 
  consumption_i <- consumption %>% 
    filter(year %in% unique(artis_eez$year),
           source_country_iso3c == countries_i)
  ########## ^^^
  rmarkdown::render(
    input = "country_profile_template.Rmd",
    params = list(countries_i = countries_i,
                  artis_eez_i = artis_eez_i,
                  artis_sau_i = artis_sau_i),
    #output_dir = outdir,
    output_file = paste("dwf", countries_i, "profile.html", sep = "_")
  )
}



































