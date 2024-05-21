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
# 3) disaggregate ARTIS by EEZ of catch - Join production SAU and ARTIS SAU data 
source(file.path(".", "scripts", "clean_data.R"))

# Countries of interest ---------------------------------------------------

# Vector of Oceana countries:
# countries <- c("Belize", "Brazil", "Canada", "Chile", "Mexico", "Philippines", "Peru", "UK", "USA", "Spain", "Malaysia", "Ghana", "Senegal")
countries <- c("Belize")
#year_int <- c(2016, 2017, 2018, 2019, 2020)

# Standardize country names
countries_std <- countrycode(countries,
                              origin = "country.name",
                              destination = "iso3c")
# for testing only
countries_i <- countries_std

# Build DWF profiles ---------------------------------------------

for (i in 1:length(countries_std)) {
  countries_i <- countries_std[i]
  
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
  
  rmarkdown::render(
    input = "country_profile_template.Rmd",
    params = list(countries_i = countries_i,
                  artis_eez_i = artis_eez_i,
                  artis_sau_i = artis_sau_i),
    #output_dir = outdir,
    output_file = paste("dwf", countries_i, "profile.html", sep = "_")
  )
}



































