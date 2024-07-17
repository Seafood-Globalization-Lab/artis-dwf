# scrape Flag of convience (FOC) list from ITF Seafarers (International Transport Workers Federation) website

# load libraries 
library(rvest) # webscraping

# scrape
itf_foc <- read_html("https://www.itfseafarers.org/en/focs/current-registries-listed-as-focs") %>% 
  html_node(xpath = "/html/body/div[1]/div/div[2]/main/div[2]/div/div/div[3]/article/div/div[1]/ul") %>%
  html_elements("li") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename("country" = value) # name variable column

itf_foc_std <- itf_foc %>% 
  mutate(country_iso3c = countrycode(country,
                               origin = "country.name",
                               destination = "iso3c"))

