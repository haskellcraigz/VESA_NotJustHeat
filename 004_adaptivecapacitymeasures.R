######################################
## LOADING HDI AND OTHER MEASURES OF A
## Date Created: Nov. 25th 2024
## Last Modified: Nov. 25th 2024
#####################################
# NOTES: code for correlation coefficients between HDI and other measures,
# All data is at the NUTS2 level (for correlations)
# ALSO the within/between country variance in HDI. Here we extract HDI at both
# NUTS2 and NUTS3 levels

# HDI [Update path if necessary to match local environment] -------------------
# raster data from: https://www.nature.com/articles/sdata20184#Sec14

print("loading HDI...")

hdi_raster <- rast("Data/HDI_1990_2015_v2.nc")

# years associated with each raster layer. See: https://datadryad.org/stash/dataset/doi:10.5061/dryad.dk1j0
years <- c(1990:2015)



# add the dates 
names(hdi_raster) <- years

# matching crs
nuts2.shp <- st_transform(nuts2.shp, st_crs(hdi_raster))

## spatial mean of HDI at NUTS 2 level ----------------
hdi_extracted_nuts2 <- exact_extract(hdi_raster, nuts2.shp, 'mean', 
                               append_cols = 'NUTS_ID',  progress = TRUE)

# rename
names(hdi_extracted_nuts2) <- str_replace(names(hdi_extracted_nuts2), "mean\\.", "HDI_") 

# pivot longer so one row corresponds to one year and one NUTS region
hdi_long_nuts2 <- hdi_extracted_nuts2 %>%
  pivot_longer(cols = c(HDI_1990:HDI_2015), names_to = c("year"), values_to = "HDI") %>% 
  mutate(year = str_replace_all(year, "HDI_", "")) %>% 
  mutate(year = as.numeric(year))


## spatial mean of HDI at NUTS 3 level ----------------

# matching crs
nuts3.shp <- st_transform(nuts3.shp, st_crs(hdi_raster))

# spatial mean
hdi_extracted_nuts3 <- exact_extract(hdi_raster, nuts3.shp, 'mean', 
                           append_cols = 'NUTS_ID',  progress = TRUE)

# rename
names(hdi_extracted_nuts3) <- str_replace(names(hdi_extracted_nuts3), "mean\\.", "HDI_") 


# pivot longer 
hdi_long_nuts3 <- hdi_extracted_nuts3%>%
  pivot_longer(cols = c(HDI_1990:HDI_2015), names_to = c("year"), values_to = "HDI") %>%
  mutate(year = str_replace_all(year, "HDI_", "")) %>% 
  mutate(year = as.numeric(year))

# Keep only the values for 2014 to use in VESA calculation
hdi_long_nuts3_2014 <- hdi_long_nuts3 %>%
  filter(year == 2014)


## remove datafiles used for processing -------------
rm("hdi_extracted_nuts3", "hdi_extracted_nuts2", "hdi_raster")

# load GDP ----------------------
print("loading GDP...")
gdp <- list(GDP_pc = get_eurostat("nama_10r_2gdp",
                                  time_format = "num",
                                  filters = list(unit="EUR_HAB_EU27_2020")))
# unlist and clean
gdp <- gdp[[1]] %>%
  dplyr::select(geo, year = time, GDP = values)


# load life-expectancy ----------------------------
print("loading life expectancy...")
life_expect <- get_eurostat("demo_r_mlifexp") #life expectancy 


life_expect <- life_expect %>% 
  filter(sex == "T", #not stratified by sex
         age == "Y65") %>% #filter to life expect at age 65
  dplyr::select(geo, TIME_PERIOD, values) %>% 
  mutate(year = lubridate::year(TIME_PERIOD), life_expectancy = values)



# load HAC index --------------------------
print("loading HAC index...")

HAC_2012_2016 <- read_excel("Data/HAC_2012_2016.xlsx")
HAC_2017_2021 <- read_excel("Data/HAC_2017_2021.xlsx")

## clean ----------------
# pull out means from each 5 year period dataset and join together
HAC_year <- left_join(dplyr::select(HAC_2012_2016, geo, starts_with("HAC")),
                      dplyr::select(HAC_2017_2021, geo, starts_with("HAC")))

HAC_long <- HAC_year %>%
  pivot_longer(cols = HAC_2012:HAC_2021, names_to = "year", values_to ="HAC") %>% 
  mutate(year = str_replace_all(year, "HAC_", "")) %>% 
  mutate(year = as.numeric(year))


## remove datafiles used for processing -------------
rm(HAC_2012_2016, HAC_2017_2021)


