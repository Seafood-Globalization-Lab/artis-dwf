# ARTIS DWF Update

# NOTES:
# - Standardize EEZ territories
# - Find out what happened to catch on the high seas
# - Identify source of many-to-many matching issue
# - Ensure only marine capture is retained
# - Improve sankey figures to represent consumption flows
# - Create country profiles for Oceana + select countries of interest
# - Interest in W Africa pelagics and humboldt squid

# Load packages
library(tidyverse)
library(countrycode)
library(exploreARTIS)

# Load data
# source("load_data.R")
artis_sau <- read.csv("data/SAU_ARTIS_2010-2020.csv")

# Clean data
prod_sau <- prod_sau %>%
  # Break apart EEZ column to identify ISO codes
  separate(eez, into = c("eez_1", "eez_2"), sep = "\\(", remove = FALSE) %>%
  mutate(eez_2 = gsub("\\)", "", eez_2)) %>%
  mutate(eez_iso3c = countrycode(eez, origin = "country.name", destination = "iso3c")) %>% 
  mutate(eez_iso3c = case_when(
    (!is.na(eez_iso3c)) ~ eez_iso3c, 
    (is.na(eez_iso3c) ~ countrycode(eez_1, origin = "country.name", destination = "iso3c")))) %>%
  mutate(eez_iso3c = case_when(
    (!is.na(eez_iso3c)) ~ eez_iso3c, 
    (is.na(eez_iso3c) ~ countrycode(eez_2, origin = "country.name", destination = "iso3c")))) %>%
  mutate(eez_iso3c = case_when(
    (eez == "US Virgin Isl.") ~ "USA",
    TRUE ~ eez_iso3c
  )) %>%
  select(-eez_1, -eez_2) %>%
  # FIXIT: Need to add in country standardization here
  # Tag domestic versus foreign fishing
  mutate(dwf = case_when(
    (eez_iso3c == country_iso3_alpha) ~ "domestic",
    TRUE ~ "foreign"
  ))
  
# Summary stats/figs on DWF
prod_sau %>%
  group_by(year, dwf) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ggplot(aes(x = year, y = live_weight_t/1000000, fill = dwf)) +
  geom_area() +
  labs(x = "", y = "Landings (mil t, live weight)") +
  theme_bw()

prod_sau %>%
  filter(dwf == "foreign") %>%
  group_by(country_name_en) %>%
  summarise(total_dwf = sum(live_weight_t)) %>%
  filter(total_dwf > 1000000) %>%
  ungroup() %>%
  ggplot(aes(y = fct_reorder(country_name_en, total_dwf), 
             x = total_dwf/(1000000*length(unique(prod_sau$year))))) +
  geom_bar(stat = "identity") +
  labs(y = "", x = "Ave. Landings (mil t, live weight)") +
  theme_bw()

prod_sau %>%
  filter(dwf == "foreign") %>%
  group_by(country_name_en) %>%
  mutate(total_dwf = sum(live_weight_t)) %>%
  filter(total_dwf > 15000000) %>%
  group_by(year, country_name_en) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ggplot(aes(x = year, y = live_weight_t/1000000, fill = country_name_en)) +
  geom_area() +
  labs(x = "", y = "Landings (mil t, live weight)", fill = "Fishing entity") +
  theme_bw()

# Top species caught by DFW
prod_sau %>%
  filter(dwf == "foreign") %>%
  group_by(sciname) %>%
  mutate(total_dwf = sum(live_weight_t)) %>%
  filter(total_dwf > 5000000) %>%
  group_by(year, sciname) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ggplot(aes(x = year, y = live_weight_t/1000000, fill = sciname)) +
  geom_area() +
  labs(x = "", y = "Landings (mil t, live weight)", fill = "") +
  theme_bw()

prod_sau %>%
  nrow()

prod_sau %>%
  filter(live_weight_t < 0.1) %>%
  nrow()

prod_sau_props <- prod_sau %>%
  group_by(year, country_iso3_alpha, sciname, eez, eez_iso3c, dwf) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  group_by(year, country_iso3_alpha, sciname) %>%
  mutate(prop_by_eez = live_weight_t/sum(live_weight_t)) %>%
  select(-live_weight_t)
  
# Disaggregate ARTIS by EEZ for China 2019
artis_eez <- artis_sau %>% 
  filter(habitat == "marine", method == "capture", 
         year == 2019, source_country_iso3c == "CHN") %>%
  left_join(prod_sau_props %>% 
              filter(year == 2019, 
                     country_iso3_alpha == "CHN"), 
            by = c("year", "source_country_iso3c" = "country_iso3_alpha", "sciname")) %>%
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
  filter(sciname == "illex argentinus") %>%
  plot_sankey()
  

# Disaggregate ARTIS by EEZ for Spain 2019
artis_eez <- artis_sau %>% 
  filter(habitat == "marine", method == "capture", 
         year == 2019, source_country_iso3c == "ESP") %>%
  left_join(prod_sau_props %>% 
              filter(year == 2019, 
                     country_iso3_alpha == "ESP"), 
            by = c("year", "source_country_iso3c" = "country_iso3_alpha", "sciname")) %>%
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

