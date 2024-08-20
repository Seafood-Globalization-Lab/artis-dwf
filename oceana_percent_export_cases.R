# Oceana percent export



oceana_cases <- read.csv("data/oceana_trawling_cases.csv") %>%
  mutate(eez_iso3c = countrycode(eez, origin = "country.name", destination = "iso3c"),
         fishing_entity_iso3c = countrycode(fishing_entity, origin = "country.name", destination = "iso3c")) %>%
  mutate(scientific_name = str_to_lower(scientific_name)) %>%
  select(eez_iso3c, fishing_entity_iso3c, scientific_name) %>%
  distinct()


oceana_cases_percent_exported <- oceana_cases %>%
  # artis_eez created in exploratory_dwf script
  left_join(artis_eez %>%
              filter(dom_source == "domestic",
                     habitat == "marine", 
                     method == "capture") %>%
              group_by(year, source_country_iso3c, catch_artis_iso3, sciname) %>% 
              summarise(export_live_weight_t = sum(live_weight_t, na.rm = TRUE)), 
            by = c("fishing_entity_iso3c" = "source_country_iso3c", "eez_iso3c" = "catch_artis_iso3",
                   "scientific_name" = "sciname")) %>%
  left_join(prod_sau %>% 
              filter(habitat == "marine", 
                     prod_method == "capture") %>%
              group_by(year, prod_iso3, sciname) %>%
              summarise(prod_live_weight_t = sum(live_weight_t, na.rm = TRUE)),
            by = c("fishing_entity_iso3c" = "prod_iso3", "scientific_name" = "sciname", "year")) %>%
  mutate(percent_exported = 100*export_live_weight_t/prod_live_weight_t)

write.csv(oceana_cases_percent_exported, "data/oceana_cases_percent_exported.csv", row.names = FALSE)
