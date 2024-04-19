# Build Distant Water Fleet Profiles for each country of interest

# loop through list of countries, knit the Rmd/Quarto file for each country, and save the output as individual HTML reports.

# Load Packages
library(quarto) # Rmd successor

# List of Oceana countries of interest: 
countries <- c("Belize", "Brazil", "Canada", "Chile", "Mexico", "Philippines", "Peru", "UK", "USA", "Spain", "Malaysia", "Ghana", "Senegal")

# Standardize country names 
countries_std <-countrycode(countries,
                              origin = "country.name",
                              destination = "country.name")

# Loop through each country to generate individual reports
for (a_country in countries_std) {
  
  # Knit the Rmd/Quarto file
  quarto::quarto_render(
    input = file.path(".", "country_profile_template.Qmd"),  
    output_file = paste0("report_", a_country, ".html"),
    params = list(a_country = a_country) # not sure if this is correst - AM
  )
  
  cat(paste("Report for", a_country, "generated.\n"))
}
