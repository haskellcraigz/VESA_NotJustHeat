#######################################
## Load and clean shapefiles for mapping and analysis
## Date Created: Nov 25th 2024
## Last Modified: Nov 25th 2024
#######################################

# load shapefiles [Update path if necessary to match local environment]---------
nuts.shp <- read_sf("Data/NUTS_RG_10M_2021_4326.shp")
country_eu.shp <- read_sf("Data/CNTR_RG_10M_2020_4326.shp")


# clean and subset --------
# NUTS 3
nuts3.shp = nuts.shp %>% 
  filter(LEVL_CODE == 3) %>% 
  filter(CNTR_CODE != "TR") %>% 
  select(NUTS_ID, geometry)

nuts3.shp$area <- st_area(nuts3.shp)


# NUTS 2
nuts2.shp = nuts.shp %>% 
  filter(LEVL_CODE == 2)


nuts2.shp$area <- st_area(nuts2.shp)

# Europe boundaries
country_eu_sam.shp = country_eu.shp %>% 
  filter(EU_STAT == "T" | EFTA_STAT == "T" | CC_STAT == "T") %>% 
  filter(NAME_ENGL != "Ukraine") %>% 
  filter(NAME_ENGL != "Moldova") %>%
  filter(NAME_ENGL != "Turkey") %>% 
  filter(CNTR_ID != "TR") %>% 
  arrange(NAME_ENGL)

uk.shp = country_eu.shp %>% 
  filter(CNTR_ID == "UK")

country_eu_sam.shp <- rbind(country_eu_sam.shp, uk.shp)