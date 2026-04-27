#### Wetland Vegetation Dashboard NOTES ####


## option to change display latin name of glossy buckthorn from "Rhamnus frangula" to "Frangula alnus"

# species lists
species_data <- read.csv("data/vis_FOA_NETN_spplist_2011_2025_20260324.csv") %>%
  filter(!str_detect(latin.name, regex("unknown", ignore_case = TRUE))) %>% 
  mutate(
    latin.name = if_else(latin.name == "Rhamnus frangula", "Frangula alnus", latin.name))