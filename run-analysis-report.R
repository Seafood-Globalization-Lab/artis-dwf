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
prod_sau <- read_csv(
  file.path(".", "data", "standardized_sau_prod.csv",
            fsep = "/"))

# Read in local SAU consumption file (eventually replace with Heroku db)
consumption <- read_csv(
  file.path(".", "data", "complete_consumption.csv"))

sciname_metadata <- read_csv(
  file.path(".", "data", "sciname_metadata.csv")
)

message("data import is complete")

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

message("running clean_data.R is complete")

# Countries of interest ---------------------------------------------------

# Vector of Oceana countries:
# countries <- c("Belize", "Brazil", "Canada", "Chile", "Mexico", "Philippines", "Peru", "UK", "USA", "Spain", "Malaysia", "Ghana", "Senegal")
#countries <- c("Belize")
countries <- c("Philippines", "Ghana")

# Standardize country names
countries_std <- countrycode(countries,
                              origin = "country.name",
                              destination = "iso3c")
# for testing only
countries_i <- countries_std

# limit sciname metadata -------------
sciname_metadata <- sciname_metadata %>% 
  select(sciname, common_name)

# Filter last 5 years of data ----------------------------
max_year <- max(consumption_eez$year)
last_x_yrs <- seq(max_year - 4, max_year, by = 1)

consumption_eez_2 <- consumption_eez %>% 
  filter(year %in% last_x_yrs) %>% 
  rename(producer_iso3c = source_country_iso3c,
         eez_iso3c = catch_artis_iso3,
         eez_name = catch_artis_country_name)

# Build DWF profiles ---------------------------------------------

# Loop through focal countries - Build reports for each
for (i in 1:length(countries_std)) {
  countries_i <- countries_std[i]

  # 1) focal country DWF activities
    country_i_dwf <- consumption_eez_2 %>% 
      filter(producer_iso3c == countries_i)

  # 2) focal country consumption of DWF catch
    country_i_dwf_consump <- consumption_eez_2 %>% 
      filter(consumer_iso3c == countries_i)
    
  # 3) Fishing activity in focal country EEZ
    country_i_eez_fishing <- consumption_eez_2 %>% 
      filter(eez_iso3c == countries_i)

  rmarkdown::render(
    input = "country_profile_template.Rmd",
    params = list(countries_i = countries_i,
                  country_i_dwf = country_i_dwf,
                  country_i_dwf_consump = country_i_dwf_consump,
                  country_i_eez_fishing = country_i_eez_fishing,
                  sciname_metadata = sciname_metadata), 
    output_dir = outdir,
    output_file = paste("dwf", countries_i, "profile.html", sep = "_")
  )
  message(glue("Compteted rendering {countries_i}'s profile"))
}



































