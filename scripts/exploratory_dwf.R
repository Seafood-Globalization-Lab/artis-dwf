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
  
  # aggregate catch quantity records
  # group_by(group_by(across(-quantity))) %>% 
  # summarize(live_weight_t = sum(quantity)) %>% 
  group_by(year, prod_iso3, SciName, prod_method, 
           habitat, catch_artis_iso3, catch_artis_country_name) %>% 
  summarise(live_weight_t = sum(quantity)) %>% 
  ungroup() %>% 
  
  # Tag domestic versus foreign fishing
  mutate(dwf = case_when(
    (catch_artis_iso3 == prod_iso3) ~ "domestic",
    TRUE ~ "foreign"
  ))

# Summary Stats & Figures -------------------------------------------------

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

# Join SAU & ARTIS data ---------------------------------------------------------

# Proportion of catch by ...
prod_sau_props <- prod_sau %>%
  # aggregate catch amount - disregard habitat, production method, sector, end use
  group_by(year, prod_iso3, SciName, 
           catch_artis_country_name, catch_artis_iso3, dwf) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>% 
  # calculate prop catch over each eez - does not contract df
  # needs to be exactly what we are joining by
  group_by(year, prod_iso3, SciName) %>%
  mutate(prop_by_catch_eez = live_weight_t/sum(live_weight_t)) %>%
  select(-live_weight_t)

# Disaggregate ARTIS by EEZ for China 2019
artis_eez <- artis_sau %>% 
  filter(habitat == "marine", 
         method == "capture", 
         year == 2019, 
         source_country_iso3c == "CHN") %>%
  left_join(prod_sau_props %>% 
              filter(year == 2019, 
                     prod_iso3 == "CHN"), 
            by = c("year", "source_country_iso3c" = "prod_iso3", "sciname" = "SciName")) %>%
  mutate(live_weight_t = live_weight_t*prop_by_catch_eez)
# many-to-many is what we expect here - one row of artis_sau correlates with multiple prod_sau eez

# landings mass check - filter
artis_sau_check <- artis_sau %>% 
  filter(habitat == "marine", 
         method == "capture", 
         year == 2019, 
         source_country_iso3c == "CHN")

# e^-6 or e^-9 considered 0 - haven't gained or lost any mass
sum(artis_eez$live_weight_t) - sum(artis_sau_check$live_weight_t)

nrow(artis_eez) 

artis_eez %>% 
  filter(dwf == "foreign") %>%
  group_by(sciname) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  arrange(desc(live_weight_t)) %>%
  print(n = 25)

# Sanky Flows -----------------------------------------------------------------

# 
artis_eez %>%
 # select(-source_country_iso3c) %>%
 # rename("source_country_iso3c" = "catch_artis_iso3") %>%
  filter(sciname == "illex argentinus") %>%
  plot_sankey()

artis_eez %>%
  select(-source_country_iso3c) %>%
  rename("source_country_iso3c" = "catch_artis_iso3") %>%
  filter(sciname == "illex argentinus") %>%
  plot_sankey()
  

# Disaggregate ARTIS by EEZ for Spain 2019
artis_eez <- artis_sau %>% 
  filter(habitat == "marine", method == "capture", 
         year == 2019, source_country_iso3c == "ESP") %>%
  left_join(prod_sau_props %>% 
              filter(year == 2019, 
                     prod_iso3 == "ESP"), 
            by = c("year", "source_country_iso3c" = "prod_iso3", "sciname")) %>%
  mutate(live_weight_t = live_weight_t*prop_by_eez)

nrow(artis_eez) 

artis_eez %>% 
  filter(dwf == "foreign") %>%
  group_by(sciname) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  arrange(desc(live_weight_t)) %>%
  print(n = 25)
  
artis_eez %>%
  select(-source_country_iso3c) %>%
  rename("source_country_iso3c" = "eez_iso3c") %>%
  filter(sciname == "prionace glauca") %>%
  plot_sankey()


# Fishmeal analysis
# Top species exported in fishmeal codes for top 10 fishmeal exporting countries 
# in addition to any Oceana countries of interest not in top 10 
# (Belize, Brazil, Canada, Chile, Mexico, the Philippines, Peru, the UK, USA, Spain, Malaysia, Ghana, Senegal)

# Top fishmeal exporting countries
top_10_fm_exporters <- artis_fmfo %>% 
  group_by(source_country_iso3c) %>% 
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  slice_max(order_by = live_weight_t, n = 10) %>%
  pull(source_country_iso3c)

artis_fmfo %>%
  filter(source_country_iso3c %in% top_10_fm_exporters) %>%
  left_join(sciname_metadata %>%
              select(sciname, common_name), by = "sciname") %>%
  plot_ts(artis_var = "common_name", facet_variable = "source_country_iso3c", facet_values = 10, 
          prop_flow_cutoff = 0.1)

additional_fm_exporters <- c("BEL", "BRA", "CAN", "CHL", "MEX", "PHL",
                             "PER", "GBR", "USA", "ESP", "MYS", "GHA", "SEN") 

additional_fm_exporters <- additional_fm_exporters[!(additional_fm_exporters %in% top_10_fm_exporters)]

artis_fmfo %>%
  filter(source_country_iso3c %in% additional_fm_exporters) %>%
  left_join(sciname_metadata %>%
              select(sciname, common_name), by = "sciname") %>%
  plot_ts(artis_var = "common_name", facet_variable = "source_country_iso3c", facet_values = 10, 
          prop_flow_cutoff = 0.15)

# Top importers
top_10_fm_importers <- artis_fmfo %>% 
  group_by(importer_iso3c) %>% 
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  slice_max(order_by = live_weight_t, n = 10) %>%
  pull(importer_iso3c)

artis_fmfo %>%
  filter(importer_iso3c %in% top_10_fm_importers) %>%
  left_join(sciname_metadata %>%
              select(sciname, common_name), by = "sciname") %>%
  plot_ts(artis_var = "common_name", facet_variable = "importer_iso3c", facet_values = 10, 
          prop_flow_cutoff = 0.1)

additional_fm_importers <- c("BEL", "BRA", "CAN", "CHL", "MEX", "PHL",
                             "PER", "GBR", "USA", "ESP", "MYS", "GHA", "SEN") 

additional_fm_importers <- additional_fm_importers[!(additional_fm_importers %in% top_10_fm_importers)]

artis_fmfo %>%
  filter(importer_iso3c %in% additional_fm_importers) %>%
  left_join(sciname_metadata %>%
              select(sciname, common_name), by = "sciname") %>%
  plot_ts(artis_var = "common_name", facet_variable = "importer_iso3c", facet_values = 12, 
          prop_flow_cutoff = 0.1)

# Top importers by source country
artis_fmfo %>%
  filter(importer_iso3c %in% top_10_fm_importers) %>%
  plot_ts(artis_var = "source_country_iso3c", facet_variable = "importer_iso3c", facet_values = 10, 
          prop_flow_cutoff = 0.1, plot.type = "stacked")

artis_fmfo %>%
  filter(importer_iso3c %in% additional_fm_importers) %>%
  plot_ts(artis_var = "source_country_iso3c", facet_variable = "importer_iso3c", facet_values = 12, 
          prop_flow_cutoff = 0.1, plot.type = "stacked")

