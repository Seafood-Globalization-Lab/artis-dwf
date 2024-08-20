# ARTIS DWF Update

# Notes -------------------------------------------------
# - Standardize EEZ territories (done)
# - Find out what happened to catch on the high seas
# - Identify source of many-to-many matching issue 
# - Ensure only marine capture is retained (done - with sau standardized data)
# - Improve sankey figures to represent consumption flows
# - Create country profiles for Oceana + select countries of interest
# - Interest in W Africa pelagics and humboldt squid

# Load Packages -------------------------------------------------
library(tidyverse)
library(countrycode)
library(exploreARTIS)

# Load Data & Scripts -------------------------------------------------
#existing SAU data in ARTIS
artis_sau <- read.csv("data/SAU_ARTIS_2010-2020.csv")

# load new SAU data - AM from ARTIS repo ./QA/outputs/
# Only contains species & countries standardized SAU marine capture (not EEZ) 
prod_sau <- read_csv("./data/standardized_sau_prod.csv")

source(file.path("./scripts/standardize_sau.R"))

# Clean Data -------------------------------------------------
prod_sau <- prod_sau %>%
  
  # clarify column names
  rename(prod_iso3 = country_iso3_alpha,
        catch_eez = eez) %>% 
  
  # Break apart catch_eez column - identify ISO3 codes with one of the 3 eez columns
  separate(catch_eez, 
           into = c("catch_eez_1", "catch_eez_2"), 
           sep = "\\(", remove = FALSE) %>%
  mutate(catch_eez_2 = gsub("\\)", "", catch_eez_2)) %>%
  
  # create new cleaned eez column - eez_iso3 - from countrycode library names
  mutate(catch_eez_iso3c = countrycode(catch_eez, 
                                 origin = "country.name", 
                                 destination = "iso3c")) %>% 
  mutate(catch_eez_iso3c = case_when(
    (!is.na(catch_eez_iso3c)) ~ catch_eez_iso3c, 
    (is.na(catch_eez_iso3c) ~ countrycode(catch_eez_1, 
                                    origin = "country.name", 
                                    destination = "iso3c")))) %>%
  mutate(catch_eez_iso3c = case_when(
    (!is.na(catch_eez_iso3c)) ~ catch_eez_iso3c, 
    (is.na(catch_eez_iso3c) ~ countrycode(catch_eez_2, 
                                    origin = "country.name", 
                                    destination = "iso3c")))) %>%
  
  # correct single instance 
  mutate(catch_eez_iso3c = case_when(
    (catch_eez == "US Virgin Isl.") ~ "USA",
    TRUE ~ catch_eez_iso3c
  )) %>%
  
  # use cleaned ISO3 codes to generate standard country name column
  mutate(catch_eez_name = countrycode(catch_eez_iso3c, 
                                        origin = "iso3c", 
                                        destination = "country.name")) %>% 
  
  # adds artis_iso3 and catch_artis_country_name columns
  standardize_sau_eez("catch_eez_iso3c", "catch_eez_name") %>% 
  rename(catch_artis_iso3 = artis_iso3,
         catch_artis_country_name = artis_country_name) %>% 
  
  # remove columns only used for standardization process
  select(-catch_eez_1, -catch_eez_2, -catch_eez) %>% 
  
  # add habitat and method to sau data
  mutate(prod_method = "capture", 
         habitat = "marine") %>%
  
  # aggregate catch quantity records
  group_by(year, prod_iso3, sciname, prod_method, 
           habitat, catch_artis_iso3, catch_artis_country_name) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>% 
  ungroup() %>% 
  
  # Tag domestic versus foreign fishing
  mutate(dwf = case_when(
    (catch_artis_iso3 == prod_iso3) ~ "domestic",
    TRUE ~ "foreign"
  ))

# Summary Stats & Figures ----------------------------------------------

# Landings x Year x (domestic vs foreign)
prod_sau %>%
  group_by(year, dwf) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ggplot(aes(x = year, y = live_weight_t/1000000, fill = dwf)) +
  geom_area() +
  labs(x = "", 
       y = "Landings (mil t, live weight)",
       title = "Overall Domestic vs Foreign Landings") +
  theme_bw()

# Average Landings x Country
prod_sau %>%
  filter(dwf == "foreign") %>%
  group_by(catch_artis_country_name) %>%
  summarise(total_dwf = sum(live_weight_t)) %>%
  filter(total_dwf > 1000000) %>%
  ungroup() %>%
  ggplot(aes(y = fct_reorder(catch_artis_country_name, total_dwf), 
             x = total_dwf/(1000000*length(unique(prod_sau$year))))) +
  geom_bar(stat = "identity") +
  labs(y = "", 
       x = "Ave. Landings (mil t, live weight)",
       title = "") +
  theme_bw()

# Average Landings x Country
prod_sau %>%
  filter(dwf == "foreign") %>%
  group_by(prod_iso3) %>%
  summarise(total_dwf = sum(live_weight_t)) %>%
  filter(total_dwf > 1000000) %>%
  ungroup() %>%
  ggplot(aes(y = fct_reorder(prod_iso3, total_dwf), 
             x = total_dwf/(1000000*length(unique(prod_sau$year))))) +
  geom_bar(stat = "identity") +
  labs(y = "", 
       x = "Ave. Landings (mil t, live weight)",
       title = "") +
  theme_bw()

# Top DWF fishing countries x Foreign landings x year
prod_sau %>%
  filter(dwf == "foreign") %>%
  group_by(catch_artis_country_name) %>%
  mutate(total_dwf = sum(live_weight_t)) %>%
  filter(total_dwf > 15000000) %>%
  group_by(year, catch_artis_country_name) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ggplot(aes(x = year, y = live_weight_t/1000000, fill = catch_artis_country_name)) +
  geom_area() +
  labs(x = "", 
       y = "Landings (mil t, live weight)", 
       fill = "Fishing entity",
       title = "Top countries landings from distant water fishing") +
  theme_bw()

# Top species caught by DFW
prod_sau %>%
  filter(dwf == "foreign") %>%
  group_by(SciName) %>%
  mutate(total_dwf = sum(live_weight_t)) %>%
  filter(total_dwf > 5000000) %>%
  group_by(year, SciName) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ggplot(aes(x = year, y = live_weight_t/1000000, fill = SciName)) +
  geom_area() +
  labs(x = "", 
       y = "Landings (mil t, live weight)", 
       fill = "",
       title = "Top species landings by distant water fishing") +
  theme_bw()

# number of observations
prod_sau %>%
  nrow()

# number of low production observations
prod_sau %>%
  filter(live_weight_t < 0.1) %>%
  nrow()

# Join Prod SAU & ARTIS SAU data ---------------------------------------------

# Proportion of landings by country flag captured in recorded source eezs
prod_sau_props <- prod_sau %>%
  # aggregate landings amount - 
  # disregard habitat, production method, sector, end use
  group_by(year, prod_iso3, sciname, 
           catch_artis_country_name, catch_artis_iso3, dwf) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>% 
  # calculate prop catch over each source eez - 
  # does not contract df over 2nd group_by()
  group_by(year, prod_iso3, sciname) %>% # needs to be exactly what data is joining by after
  mutate(prop_by_catch_eez = live_weight_t/sum(live_weight_t)) %>%
  select(-live_weight_t)

# Disaggregate ARTIS by EEZ of catch - join datasets
artis_eez <- artis_sau %>% 
  # prod_sau is inherently only marine capture - match artis_sau data
  filter(habitat == "marine", 
         method == "capture") %>%
  # pull prod_sau_props data for year year, source country, and species
  left_join(prod_sau_props, 
            by = c("year", 
                   "source_country_iso3c" = "prod_iso3", 
                   "sciname")) %>%
  # recalculate live_weight_t catch - each trade and product record gets split apart by the number of catch eez from prod_sau_props - essentially assigning a probability a product was caught in a specific eez. 
  mutate(live_weight_t = live_weight_t*prop_by_catch_eez)
# many-to-many warning is what we expect here - one row of artis_sau correlates with multiple prod_sau eez

# Set Year ----------------------------------------------------

# set year for following analysis
year_int <- 2019

# China Analysis ------------------------------------------------------

# Filter for China
artis_eez_chn <- artis_eez %>% 
  # prod_sau is inherently only marine capture - match artis_sau data
  filter(source_country_iso3c == "CHN",
         year == year_int)

# landings mass check - filter
artis_sau_check <- artis_sau %>% 
  filter(habitat == "marine", 
         method == "capture", 
         year == year_int, 
         source_country_iso3c == "CHN")

# e^-6 or e^-9 considered 0 - haven't gained or lost any mass
sum(artis_eez_chn$live_weight_t) - sum(artis_sau_check$live_weight_t)

nrow(artis_eez_chn) 

# print top 25
artis_eez_chn %>% 
  filter(dwf == "foreign") %>%
  group_by(sciname) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  arrange(desc(live_weight_t)) %>%
  print(n = 25)

# plot artis_sau$source_country_iso3c (without catch eez information)
artis_eez_chn %>%
 # select(-source_country_iso3c) %>%
 # rename("source_country_iso3c" = "catch_artis_iso3") %>%
  filter(sciname == "illex argentinus") %>%
  plot_sankey()

# plot with catch eez information
artis_eez_chn %>%
  select(-source_country_iso3c) %>%
  rename("source_country_iso3c" = "catch_artis_iso3") %>%
  filter(sciname == "illex argentinus") %>%
  plot_sankey()
  
# Spain Analysis -------------------------------------------------------

# Filter for Spain
artis_eez_esp <- artis_eez %>% 
  filter(source_country_iso3c == "ESP",
         year == year_int)

nrow(artis_eez_esp) 

# print top 25 species live weights
artis_eez_esp %>% 
  filter(dwf == "foreign") %>%
  group_by(sciname) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  arrange(desc(live_weight_t)) %>%
  print(n = 25)

# plot sankey
artis_eez_esp %>%
  select(-source_country_iso3c) %>%
  rename("source_country_iso3c" = "catch_artis_iso3") %>%
  filter(sciname == "prionace glauca") %>%
  plot_sankey()

# Fishmeal analysis ----------------------------------------------------

# Top species exported in fishmeal codes for top 10 fishmeal exporting countries 
# in addition to any Oceana countries of interest not in top 10 
# (Belize, Brazil, Canada, Chile, Mexico, the Philippines, Peru, the UK, USA, Spain, Malaysia, Ghana, Senegal)

artis_fmfo_sum <- artis_eez %>% 
  mutate(food_or_fmfo = case_when(
    hs6 == "230120" ~ "fishmeal",
    hs6 != "230120" ~ "nonfishmeal"
  )) %>%
  group_by(year, food_or_fmfo) %>%
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE)) 

artis_fmfo <- artis_eez %>% 
  mutate(food_or_fmfo = case_when(
    hs6 == "230120" ~ "fishmeal",
    hs6 != "230120" ~ "nonfishmeal"
  )) %>% 
  filter(food_or_fmfo == "fishmeal") %>% 
  select(-source_country_iso3c) %>%
  rename("source_country_iso3c" = "catch_artis_iso3")

# Top fishmeal exporting countries
top_10_fm_exporters <- artis_fmfo %>% 
  group_by(source_country_iso3c) %>% 
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  slice_max(order_by = live_weight_t, 
            n = 10) %>%
  pull(source_country_iso3c)

# AM 2024-02-02
# do not know where sciname_metadata.csv came from
# see on Github repo --> https://github.com/jagephart/ms-seafood-globalization/blob/09267a9a530969faa4a89b35622b5104e15a4693/prelim_results.Rmd#L81

# Can proceed with scinames already in dataset

artis_fmfo %>%
  filter(source_country_iso3c %in% top_10_fm_exporters) %>%
  # left_join(sciname_metadata %>%
  #             select(sciname, common_name), 
  #           by = "sciname") %>%
  plot_ts(artis_var = "sciname", 
          # source_country_iso3c is same as catch_artis_country_name
          facet_variable = "catch_artis_country_name", 
          facet_values = 10, 
          prop_flow_cutoff = 0.1)

additional_fm_exporters <- c("BEL", "BRA", "CAN", "CHL", "MEX", "PHL",
                             "PER", "GBR", "USA", "ESP", "MYS", "GHA", "SEN") 
# only keep exporters not already represented in top 10
additional_fm_exporters <- additional_fm_exporters[!(additional_fm_exporters %in% top_10_fm_exporters)]

artis_fmfo %>%
  filter(source_country_iso3c %in% additional_fm_exporters) %>%
  # left_join(sciname_metadata %>%
  #             select(sciname, common_name), by = "sciname") %>%
  plot_ts(artis_var = "sciname", 
          facet_variable = "catch_artis_country_name", 
          facet_values = 10, 
          prop_flow_cutoff = 0.15)

# Top importers
top_10_fm_importers <- artis_fmfo %>% 
  group_by(importer_iso3c) %>% 
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  slice_max(order_by = live_weight_t, n = 10) %>%
  pull(importer_iso3c)

artis_fmfo %>%
  filter(importer_iso3c %in% top_10_fm_importers) %>%
  # left_join(sciname_metadata %>%
  #             select(sciname, common_name), by = "sciname") %>%
  plot_ts(artis_var = "sciname", 
          facet_variable = "importer_iso3c", 
          facet_values = 10, 
          prop_flow_cutoff = 0.1)

additional_fm_importers <- c("BEL", "BRA", "CAN", "CHL", "MEX", "PHL",
                             "PER", "GBR", "USA", "ESP", "MYS", "GHA", "SEN") 
# only keep importers not already represented in top 10
additional_fm_importers <- additional_fm_importers[!(additional_fm_importers %in% top_10_fm_importers)]

artis_fmfo %>%
  filter(importer_iso3c %in% additional_fm_importers) %>%
  # left_join(sciname_metadata %>%
  #             select(sciname, common_name), by = "sciname") %>%
  plot_ts(artis_var = "sciname", 
          facet_variable = "importer_iso3c", 
          facet_values = 12, 
          prop_flow_cutoff = 0.1)

# Top importers by source country
artis_fmfo %>%
  filter(importer_iso3c %in% top_10_fm_importers) %>%
  # same as source_country_iso3c
  plot_ts(artis_var = "catch_artis_country_name", 
          facet_variable = "importer_iso3c", 
          facet_values = 10, 
          prop_flow_cutoff = 0.1, 
          plot.type = "stacked") +
  theme(legend.title = element_text("Source Country"))

# Additional imports of interest by source country
artis_fmfo %>%
  filter(importer_iso3c %in% additional_fm_importers) %>%
  plot_ts(artis_var = "catch_artis_country_name", 
          facet_variable = "importer_iso3c", 
          facet_values = 12, 
          prop_flow_cutoff = 0.1, 
          plot.type = "stacked") +
  theme(legend.title = element_text("Source Country"))


# Peru Analysis -------------------------------------------------------
artis_eez_per <- artis_eez %>%
  # Those fishing pota in Peru's water 
  filter(catch_artis_iso3 == "PER", sciname == "dosidicus gigas")

artis_eez_per %>% 
  plot_sankey()

# Madagascar Analysis -------------------------------------------------------
artis_eez_mdg <- artis_eez %>%
  # Those fishing pota in Peru's water 
  filter(catch_artis_iso3 == "MDG")

artis_eez_mdg %>% 
  filter(year %in% 2015:2019, 
         sciname == "katsuwonus pelamis") %>%
  plot_sankey()
