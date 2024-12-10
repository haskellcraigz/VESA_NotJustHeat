##############################################################################################
# HAC Index Analysis 2012-2016
# Author: Domenico Bovienzo
# started script: 09.08.2024
# last updated: 27.11.2024
# Title: Operationalization of Health Adaptive Capacity (HAC) Index
##############################################################################################
rm()
rm(list=ls())


#1 Preliminary steps before running the code ----

### Install packages
#install.packages("regions")
#install.packages("eurostat")
#install.packages("tidyverse")
#install.packages('reshape2')
#install.packages("sfheaders")
#install.packages("sf")
#install.packages("dplyr")
#install.packages("readODS")
#install.packages("rvest")
#install.packages("utils")
#install.packages("zoo")
#install.packages("openxlsx")
#install.packages("readxl")
#install.packages("COINr")
#install.packages("giscoR")
#install.packages("countrycode")
#install.packages("ggplot2")
#install.packages("shiny")
#install.packages("remotes")
#install.packages("htmltools")#remotes::install_version("htmltools", version = "0.5.8")
#install.packages("flextable")
#install.packages("gridExtra")
# install.packages("mice")
#install.packages("stargazer")
#install.packages("stringr")
# install.packages("knitr")
#install.packages("kableExtra")
#install.packages("patchwork")


#Load libraries -------
library(regions)
library(eurostat)
library(tidyverse)
library(reshape2)
library(sfheaders)
library(sf)
library(dplyr)
library(readODS)
library(rvest)
library(utils)
library(zoo)
library(openxlsx)
library(readxl)
library(COINr)
library(giscoR)
library(countrycode)
library(ggplot2)
library(shiny)
library(htmltools)
library(mice)
library(flextable)
library(gridExtra)
library(stargazer)
library(stringr)
library(knitr)
library(kableExtra)
library(patchwork) 


# Clear the cache (optional step, useful if you encounter persistent issues)
#gisco_clear_cache()


###2  Data preparation ----

#2.1  Downloading and preparing non-Eurostat data ----

# Hospital at NUTS2 -----
##########################################################################
#This section of the code prepares data on healthcare services in Europe, sourced from the GISCO-Eurostat database 
#(accessible here:https://ec.europa.eu/eurostat/web/gisco/geodata/basic-services#Healthcare) and
#and supplemented with German national public data at the NUTS1 level 
#(accessible here: https://www.destatis.de/EN/Themes/Society-Environment/Health/Hospitals/Tables/gd-hospitals-laender.html)
##########################################################################

# Replace 'path_to_your_geopackage.gpkg' with the actual path to your GeoPackage file
#geopackage_path <- "N:\\Incubator2024_climate\\PROJECT\\R\\DATA-RAW\\ACI\\EU_hc.gpkg"
geopackage_path <- "C:/Users/Domenico/Desktop/MPIDR_Program/MPIDR_project/EU_hc.gpkg"

# List the layers in the GeoPackage
layers <- st_layers(geopackage_path)
print(layers)

# Read the 'EU' layer
eu_data <- st_read(geopackage_path, layer = "EU")
print(eu_data)

# Get NUTS2 codes from Eurostat for a specific year (e.g., 2021)
nuts2_codes <- get_eurostat_geospatial(output_class = "sf", nuts_level = "2", year = 2021)


#Check the CRS (Coordinate Reference System) and transform if necessary
if (st_crs(eu_data) != st_crs(nuts2_codes)) {
  eu_data <- st_transform(eu_data, st_crs(nuts2_codes))
}

#Check and fix geometries if needed
data <- st_make_valid(eu_data)
nuts2 <- st_make_valid(nuts2_codes)

# Rename column dataset that have a common column to join 
data <- data %>%
  rename(CNTR_CODE = cntr_id) 

#Spatial join to associate data points with NUTS2 regions
data_nuts2 <- st_join(data, nuts2, join = st_within, left = FALSE) 

#Rename columns
data_nuts2 <- data_nuts2 %>%
  rename(id=id.x,
         id.1=id.y,
         CNTR_CODE = CNTR_CODE.x,
         CNTR_CODE.1 = CNTR_CODE.y)

# Find rows in `data` that are not in `data_nuts2`
unmatched <- data[!data$id %in% data_nuts2$id, ] #Found 587 observations out of 12449.

# Find nearest NUTS2 region for unmatched points
unmatched$nearest_nuts2 <- st_nearest_feature(unmatched, nuts2)

# Merge nearest NUTS2 data into the unmatched points
nearest_nuts2_data <- nuts2[unmatched$nearest_nuts2, ]
unmatched <- cbind(unmatched, st_drop_geometry(nearest_nuts2_data))

# Combine the datasets
data_nuts2_combined <- rbind(data_nuts2, unmatched[,-26])

# Check for mismatches in the CNTR_CODE column
mismatches <- data_nuts2_combined %>% #Found 77 mismatched observations out of 12449
  filter(CNTR_CODE != CNTR_CODE.1)

# Extract the country code from the mismatches 'id' column in order to fix the mismatch
mismatches <- mismatches[, 1:25] %>%  
  mutate(CNTR_CODE = substr(id, 1, 2)) # Adjust based on your 'id' format !!

# Separate mismatched data by country code
mismatches_by_country <- mismatches %>%
  split(.$CNTR_CODE)

# Create a unique identifier in order to associate the data to the nearest point in the country
nuts2 <- nuts2 %>%
  mutate(rowid = row_number()) # Create a unique identifier

find_nearest <- function(mismatch_group, nuts2) {
  # Extract the country code from the mismatches 'id' column
  mismatch_group <- mismatch_group %>%
    mutate(CNTR_CODE = substr(id, 1, 2))
  
  # Filter NUTS2 regions by country code
  nuts2_filtered <- nuts2 %>%
    filter(CNTR_CODE %in% unique(mismatch_group$CNTR_CODE))
  
  # Add rowid to the filtered NUTS2 data
  nuts2_filtered <- nuts2_filtered %>%
    mutate(rowid = row_number())
  
  # Find nearest NUTS2 region for each mismatched point
  nearest <- st_nearest_feature(mismatch_group, nuts2_filtered)
  
  # Join nearest regions back to the mismatched data using rowid
  mismatch_group %>%
    mutate(nearest_rowid = nearest) %>%
    left_join(nuts2_filtered %>% st_drop_geometry(), by = c("nearest_rowid" = "rowid"))
}

# Apply nearest neighbor function to each country code group
results <- lapply(mismatches_by_country, find_nearest, nuts2 = nuts2) %>%
  bind_rows()
#print(results)

# Rename and remove useless columns from the 'results' dataset
hospi_miss <- results[, -c(26)]

hospi_miss <- hospi_miss %>%
  rename(id=id.x,
         id.1=id.y,
         CNTR_CODE = CNTR_CODE.x,
         CNTR_CODE.1 = CNTR_CODE.y)

# Remove the mismatched rows from data_nuts2
data_nuts2_corrected <- data_nuts2_combined %>%
  filter(CNTR_CODE == CNTR_CODE.1)

# Append the corrected rows from hospi_miss
data_nuts2_corrected <- bind_rows(data_nuts2_corrected, hospi_miss)

# Drop the unique_id column if not needed
data_nuts2 <- data_nuts2_corrected 

# Aggregate the number of hospitals by NUTS2_ID(each observation in the original data counts as 1)
aggregated_data <- data_nuts2 %>%
  group_by(geo) %>%
  summarise(observations = n())

#Adding 'time' column 
aggregated_data <- aggregated_data %>%
  mutate(time = as.numeric(2017))  

# Rename existing columns 'old_column_name' to 'new_column_name'
aggregated_data1 <- aggregated_data %>%
  rename(values = observations) 

# Rename the dataframe and drop geometries
Hospitals_count <- aggregated_data1 %>%
  st_drop_geometry() 

#Add information at about hospitals for Germany (https://www.destatis.de/EN/Themes/Society-Environment/Health/Hospitals/Tables/gd-hospitals-laender.html)
# Create a mapping of NUTS2 regions to larger regions
nuts2_to_region <- list(
  "DE11" = "Baden-Württemberg", "DE12" = "Baden-Württemberg", "DE13" = "Baden-Württemberg", "DE14" = "Baden-Württemberg",
  "DE21" = "Bayern", "DE22" = "Bayern", "DE23" = "Bayern", "DE24" = "Bayern", "DE25" = "Bayern", "DE26" = "Bayern", "DE27" = "Bayern",
  "DE30" = "Berlin",
  "DE40" = "Brandenburg",
  "DE50" = "Bremen",
  "DE60" = "Hamburg",
  "DE71" = "Hessen", "DE72" = "Hessen", "DE73" = "Hessen", 
  "DE80" = "Mecklenburg-Vorpommern",
  "DE91" = "Niedersachsen", "DE92" = "Niedersachsen", "DE93" = "Niedersachsen", "DE94" = "Niedersachsen",
  "DEA1" = "Nordrhein-Westfalen", "DEA2" = "Nordrhein-Westfalen", "DEA3" = "Nordrhein-Westfalen", "DEA4" = "Nordrhein-Westfalen",
  "DEA5" = "Nordrhein-Westfalen", 
  "DEB1" = "Rheinland-Pfalz", "DEB2" = "Rheinland-Pfalz", "DEB3" = "Rheinland-Pfalz",
  "DEC0" = "Saarland",
  "DED2" = "Sachsen", "DED4" = "Sachsen", "DED5" = "Sachsen",
  "DEE0" = "Sachsen-Anhalt",
  "DEF0" = "Schleswig-Holstein",
  "DEG0" = "Thüringen"
)

# Convert the mapping list to a data frame
nuts2_to_region_df <- data.frame(
  geo = names(nuts2_to_region),
  region = unlist(nuts2_to_region)
)


# Create a vector with the values for each NUTS1 region
values <- c(
  "Baden-Württemberg" = 249,
  "Bayern" = 353,
  "Berlin" = 88,
  "Brandenburg" = 63,
  "Bremen" = 14,
  "Hamburg" = 61,
  "Hessen" = 149,
  "Mecklenburg-Vorpommern" = 38,
  "Niedersachsen" = 173,
  "Nordrhein-Westfalen" = 333,
  "Rheinland-Pfalz" = 85,
  "Saarland" = 22,
  "Sachsen" = 78,
  "Sachsen-Anhalt" = 45,
  "Schleswig-Holstein" = 93,
  "Thüringen" = 49
)

# Add a column with the total number of hospitals to the nuts2_to_region_df
nuts2_to_region_df <- nuts2_to_region_df %>%
  mutate(total_hospitals = values[region])

# Load population data and adjust time column format
population <- get_eurostat("tgs00096") %>%
  rename(time = TIME_PERIOD) %>%
  mutate(time = as.Date(time, format = "%Y-%m-%d"))

# Filter the population data for the specific date "2022-01-01"
filtered_population <- population %>%
  filter(time == as.Date("2020-01-01"))

# Add total number of hospitals to NUTS2 dataset and join with filtered population data
merged_data <- nuts2_to_region_df %>%
  mutate(total_hospitals = values[region]) %>%
  left_join(filtered_population, by = "geo")

# Calculate the sum of 'values' by 'region' and compute proportions in one step
merged_data <- merged_data %>%
  group_by(region) %>%
  mutate(
    sum_values = sum(values, na.rm = TRUE),
    proportion = values / sum_values
  ) %>%
  ungroup()

# Calculate the downscaled value of hospitals at NUTS2 level and round the result
merged_data <- merged_data %>%
  mutate(
    val_NUTS2 = total_hospitals * proportion,
    val_NUTS2_approx = round(val_NUTS2)
  )

# Subset the required columns and add the 'time' and 'iCode' columns
NUTS2_DE <- merged_data %>%
  select(geo, val_NUTS2_approx) %>%
  mutate(
    time = 2017
  ) %>%
  rename(values = val_NUTS2_approx)

# Combine with the existing Hospitals_count dataset
Hospitals_count <- bind_rows(Hospitals_count, NUTS2_DE) %>%
  mutate(iCode = "Hospital_count")


# Load and preprocess the population data
population <- get_eurostat("tgs00096") %>%
  rename(time = TIME_PERIOD) %>%
  mutate(time = as.numeric(format(time, "%Y")))

# Join the population data with Hospitals_count by NUTS2 region and time
merged_data <- Hospitals_count %>%
  left_join(population, by = c("geo", "time"))

# Calculate hospitals per 100,000 people and prepare the final dataset
Hospital_count_PHT <- merged_data %>%
  mutate(
    Hospitals_100K_people = round((values.x / values.y) * 100000),
    values = Hospitals_100K_people,
    iCode = "Hospitals_100K_people"
  ) %>%
  select(geo, time, values, iCode)

# Function to replicate data for a given year
replicate_data <- function(year) {
  Hospital_count_PHT %>%
    mutate(time = year)
}


# Create dataset for years 2011 to 2022
years <- 2011:2022
Hospitals_100k_inh <- lapply(years, replicate_data) %>% bind_rows()

rm(list= ls()[! (ls() %in% c('Hospitals_100k_inh', 'nuts2', 'nuts2_codes'))])


# Average travel time to the nearest hospital in minutes at NUTS2  ----
##########################################################################
#The code is designed to compute the average travel time to the nearest hospital across NUTS2 
#regions. This involves merging data from two sources: the Healthcare Accessibility GeoPackage at the 
# at NUTS3 (available here: https://data.europa.eu/data/datasets/de17ed31-7474-4ff5-8f8c-bdd50c690a0a?locale=en)
#and 2016 car travel time data provided by ESPON (accessible here: https://database.espon.eu/indicator/859/)
##########################################################################

# Replace 'path_to_your_geopackage.gpkg' with the actual path to your GeoPackage file
#DHosp_geopackage_path <- "N:\\Incubator2024_climate\\PROJECT\\R\\DATA-RAW\\ACI\\HEALTHCARE_ACCESSIBILITY.gpkg"

DHosp_geopackage_path <- "C:/Users/Domenico/Desktop/MPIDR_Program/MPIDR_project/HEALTHCARE_ACCESSIBILITY.gpkg"

# List the layers in the GeoPackage
DHosp_layers <- st_layers(DHosp_geopackage_path)
print(DHosp_layers)

# Read the 'EU' layer
DHosp_eu_data <- st_read(DHosp_geopackage_path, layer = "NUTS3_Healthcare_Travel_Time")
print(DHosp_eu_data)

# Aggregate to NUTS2 level
DHosp_eu_data_nuts2 <- DHosp_eu_data %>%
  mutate(NUTS2_ID = substr(NUTS_ID, 1, 4)) %>%  # Assuming NUTS2 ID is equivalent to the first 4 characters of NUTS3 ID as based on based on Regulation (EC) No 1059/2003 (https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=CELEX%3A32003R1059)
  group_by(NUTS2_ID) %>%
  summarise(Average_travel_time = mean(Average_travel_time, na.rm = TRUE))


DHosp_eu_data_nuts2  <- DHosp_eu_data_nuts2  %>%
  mutate(time = as.numeric(2017),  # Adding 'Time' column filled with 2020
         iCode = "Distance_Hospitals")  # Adding 'iCode' column filled with "hospitals"

# Rename existing columns 'old_column_name' to 'new_column_name'
DHosp_eu_data_nuts2 <- DHosp_eu_data_nuts2 %>%
  rename(geo = NUTS2_ID,
         values = Average_travel_time)  %>%
  st_drop_geometry() 

# Add car travel times by ESPON 2016 ()
ESPON <- read_excel("C:/Users/Domenico/Desktop/MPIDR_Program/MPIDR_project/ESPON_car_travel_times/ind_859_shr-hosp_data.xlsx")

#Add NUTS2 and aggregate by mean
ESPON_nuts2 <- ESPON %>%
  mutate(geo = substr(tunit_code, 1, 4)) %>%
  group_by(geo) %>%
  filter(version == 2013) %>%
  summarise(values= mean(y_2016, na.rm = TRUE)) 

# Add column time
ESPON_nuts2 <- ESPON_nuts2 %>%
  mutate(time = 2017)

# Perform a left join to combine the datasets
combined_data <- DHosp_eu_data_nuts2 %>%
  left_join(ESPON_nuts2, by = "geo", suffix = c(".dhosp", ".espon"))

# Fill missing values in the DHosp_eu_data_nuts2 values column
combined_data <- combined_data %>%
  mutate(values = coalesce(values.dhosp, values.espon)) %>%
  select(geo, values)

Car_travel_times <- combined_data %>%
  mutate(time = as.numeric(2017), iCode = "Car_Travel_times")

# Function to replicate data for a given year
replicate_data <- function(year) {
  data <- Car_travel_times 
  data$time <- year
  return(data)
}

# Create dataset for years 2011 to 2022
years <- 2011:2022
Car_travel_times <- lapply(years, replicate_data) %>% bind_rows()

str(Car_travel_times)

# Assuming your dataframe is named Car_travel_times and the column is named "values"
Car_travel_times$values[is.nan(Car_travel_times$values)] <- NA

rm(list= ls()[! (ls() %in% c('Hospitals_100k_inh','nuts2_codes', 'nuts2', 'Car_travel_times'))])


#European_Quality_of_Government_Index ----
##########################################################################
#The European Quality of Government Index (EQI) assesses citizens' views on the quality, fairness, and 
#corruption of public services, key factors for ensuring effective healthcare delivery and accessibility.  
#Data is available for the years 2010, 2013, 2017, 2021, and 2024, encompassing all 27 EU member
#states at the NUTS2 regional level. This information can be accessed through the University of 
# Gothenburg website(accessible here:https://www.gu.se/en/quality-government/qog-data/data-downloads/european-quality-of-government-index).
##########################################################################

EQI_institu_tmp <- read.csv(url("https://www.qogdata.pol.gu.se/data/qog_eqi_long_24.csv"))  
names(EQI_institu_tmp)[names(EQI_institu_tmp) == "EQI"] <- "values"
names(EQI_institu_tmp)[names(EQI_institu_tmp) == "region_code"] <- "geo"
names(EQI_institu_tmp)[names(EQI_institu_tmp) == "year"] <- "time"
#rm(list= ls()[! (ls() %in% c('GDP_final','GDPpps_final','GDPpc_final','GDPpps_pc_final','GDPdens_final','GVA_final','GVApc_final','GVAagr_final', 'GVAind_final','GVAmnf_final', 'GVAcnstr_final','GVAgr_final', 'HHI_final', 'HHIpc_final','HHIpps_final','HHIpps_pc_final','EMPL_final','EMPLeap_final','EMPLf_final','EMPLagr_final','EMPLind_final','EMPLcnstr_final','EMPLr_final','EMPLrf_final','EMPL_kt_final','EMPLktf_final', 'UEMPL_final','UEMPLf_final','UEMPLr_final','UEMPLrf_final','LUEMPLr_final','LUEMPLrf_final','AROPE_final', 'MatDep_final','COMTr_final','COMTrf_final','DPNDr_final','OADr_final','YADr_final','POPdens_final','IMR_final','LE_final','LEf_final','LEm_final','LUS_final','LUSh_final','LC_final','LCa_final','LCc_final','LCw_final','LCg_final','LCwa_final','LCwet_final','LU_final','LUagr_final','LUfa_final','LUmq_final','LUen_final','LUwwt_final','LUcnstr_final','LUinf_final','IRRa_final','IRRagr_final','IRRuaa_final','EDUPl_final','EDUPh_final','EDUPt_final','EDUl_final','EDUlf_final','EDUh_final','EDUhf_final','GERD_final','RD_final','RDr_final','RDr_final','RDrf_final','PAT_final','PATpmi_final','HIA_final','IIA_final', 'HP_final','HPpc_final','HOSb_final','HOSbi_final','MRN_final','VEH_final','VEHr_final', 'SE_final', 'HRST_final','CHDD_final', 'HTEC_final', 'HTPAT_final','GFCF_final','EQI_final'))])
EQI_institu <- EQI_institu_tmp 
#  mutate(iCode = "European_Quality_of_Government_Index")  # Adding 'iCode' column filled with "hospitals"

# Find missing geo codes
missing_geo <- anti_join(nuts2, EQI_institu, by = "geo")


# Define the imputation function
impute_value <- function(geo_code, time_value, data) {
  country_code <- substr(geo_code, 1, 2)
  nuts1_code <- substr(geo_code, 1, 3)
  nuts0_code <- substr(geo_code, 1, 2)
  
  # 1. Impute the mean of NUTS2 level data for the same country and time
  nuts2_mean <- EQI_institu %>%
    filter(substr(geo, 1, 2) == country_code, nchar(geo) == 4, time == time_value) %>%
    summarise(mean_value = mean(values, na.rm = TRUE)) %>%
    pull(mean_value)
  
  if (!is.na(nuts2_mean) && length(nuts2_mean) > 0) {
    return(nuts2_mean)
  }
  
  # 2. Impute the mean of NUTS1 level data for the same country and time
  nuts1_mean <- EQI_institu %>%
    filter(substr(geo, 1, 3) == nuts1_code, nchar(geo) == 3, time == time_value) %>%
    summarise(mean_value = mean(values, na.rm = TRUE)) %>%
    pull(mean_value)
  
  if (!is.na(nuts1_mean) && length(nuts1_mean) > 0) {
    return(nuts1_mean)
  }
  
  # 3. Impute the mean of NUTS0 level data for the same country and time
  nuts0_mean <- EQI_institu %>%
    filter(substr(geo, 1, 2) == nuts0_code, nchar(geo) == 2, time == time_value) %>%
    summarise(mean_value = mean(values, na.rm = TRUE)) %>%
    pull(mean_value)
  
  if (!is.na(nuts0_mean) && length(nuts0_mean) > 0) {
    return(nuts0_mean)
  }
  
  # If no imputation value is found, return NA
  return(NA)
}

# Add the missing time values to missing_geo
time_values <- c("2010", "2013", "2017", "2021", "2024")

# Expand missing_geo to include all required time values
missing_geo_with_time <- expand.grid(geo = unique(missing_geo$geo), time = time_values)

# Apply imputation to the missing_geo_with_time dataset
missing_geo_with_time_imputed <- missing_geo_with_time %>%
  rowwise() %>%
  mutate(imputed_value = impute_value(geo, time, EQI_institu)) %>%
  ungroup()

# Check the result
print(missing_geo_with_time_imputed)

# Ensure the `time` column in `missing_geo_with_time_imputed` is integer
missing_geo_with_time_imputed <- missing_geo_with_time_imputed %>%
  mutate(time = as.integer(as.character(time)))

# Bind rows to update EQI_institu
EQI_institu_updated <- EQI_institu %>%
  bind_rows(
    missing_geo_with_time_imputed %>%
      select(geo, time, imputed_value) %>%
      rename(values = imputed_value)
  )

EQI_institu_updated <- EQI_institu_updated %>%
  select(geo, time, values)


# Expand the dataset to include all years from 2010 to 2024
expand_years <- function(geo_code, min_year = 2010, max_year = 2024) {
  data.frame(geo = geo_code, time = seq(min_year, max_year, by = 1))
}

# Get unique geo codes
unique_geos <- unique(EQI_institu_updated$geo)

# Create a data frame with all combinations of geo and years
all_geo_years <- do.call(rbind, lapply(unique_geos, expand_years))

# Merge with the original dataset to include the expanded years
expanded_data <- left_join(all_geo_years, EQI_institu_updated, by = c("geo", "time"))

# Define the function to interpolate values
interpolate_if_possible <- function(x, time) {
  if (sum(!is.na(x)) >= 2) {
    return(approx(time, x, time, rule = 2)$y)
  } else {
    return(x)
  }
}

# Apply the interpolation to the expanded dataset
EQI_Index <- expanded_data %>%
  group_by(geo) %>%
  arrange(time) %>%
  mutate(across(where(is.numeric), ~ interpolate_if_possible(., time))) %>%
  ungroup() %>%
  mutate(iCode = "EQI_Index")

rm(list= ls()[! (ls() %in% c('Hospitals_100k_inh', 'Car_travel_times', 'EQI_Index','nuts2_codes','nuts2'))])


#2.2 Download EUROSTAT data through API (missing: PISA, Low waged workers, qualification mismatch) ----

# get data (missing: PISA, Low waged workers, qualification mismatch)----
l_data1 <- list(
  Empl_tech_know = get_eurostat("htec_emp_reg2", time_format = "num", filters = list(sex = "T", unit = "PC_EMP", nace_r2="Q")),
  Primary_Income_hh = get_eurostat("tgs00036", time_format = "num"),
  Pop_edu = get_eurostat("edat_lfse_04", time_format = "num", filters = list(isced11 = "ED5-8", age="Y25-64", sex="T")), # ED34 is "Upper secondary ed, general (check)
  GDP_pc = get_eurostat("nama_10r_2gdp", time_format = "num", filters = list(unit="EUR_HAB_EU27_2020")),
  Empl_rate = get_eurostat("lfst_r_lfe2emprt", time_format = "num", filters = list(age="Y15-64", sex="T"))
  )

# save
saveRDS(l_data1, "raw_data.RDS")

# Rename the dataset only for replication purposes
l_data <- l_data1


# Define a function to rename columns to ensure consistency
rename_time_column <- function(data) {
  # Rename the "TIME_PERIOD" column to "time" if it exists
  if ("TIME_PERIOD" %in% colnames(data)) {
    colnames(data)[colnames(data) == "TIME_PERIOD"] <- "time"
  }
  
  # Ensure all time-related columns are named "time"
  if ("time" %in% colnames(data)) {
    colnames(data)[colnames(data) == "time"] <- "time"
  }
  
  return(data)
}

# Apply the function to each element in the list l_data
l_data <- lapply(l_data, rename_time_column)

# Now, you can process your data with a consistent column name "time"
l_data_filt <- lapply(names(l_data), function(iCode) {
  X <- l_data[[iCode]]
  X <- X[c("geo", "time", "values")]
  X$iCode <- iCode
  return(X)
})


# convert to table
df_data <- Reduce(rbind, l_data_filt)


#Merge Eurostat and other non-Eurostat Data together
combined_data0 <- bind_rows(df_data, Hospitals_100k_inh) %>%
  select(geo,time, values, iCode)
df_data <-combined_data0


combined_data1 <- bind_rows(df_data, Car_travel_times) %>%
  select(geo,time, values, iCode)
df_data <-combined_data1


combined_data2 <- bind_rows(df_data, EQI_Index) %>%
  select(geo,time, values, iCode)
df_data <-combined_data2


# Remove Extra European Union countries 
df_data <- df_data %>%
  filter(!grepl("^(uk|tr|me|mk|li|is|ch|al|rs)", geo, ignore.case = TRUE))

# rename columns
names(df_data) <- c("uCode", "Time", "Value", "iCode")

# Assuming 'nuts2_codes' is the sf object obtained from Eurostat containing NUTS2 boundaries
# Get NUTS2 codes from Eurostat for a specific year (e.g., 2021)
#nuts2_codes <- get_eurostat_geospatial(output_class = "sf", nuts_level = "2", year = 2021)

# Subset the dataset to keep only columns "a" and "b"
nuts2_codes_eu <- nuts2_codes %>% 
  select(NUTS_NAME, geo) %>% 
  st_drop_geometry()

# Assign names to columns
names(nuts2_codes_eu) <- c("uName", "uCode")

# Filter df_data based on NUTS2 codes from nuts2_codes_europe
df_data_filtered <- df_data[df_data$uCode %in% nuts2_codes_eu$uCode, ] 

df_filtered <- df_data_filtered %>%
  filter(Time >= 2011)

# pivot to wide
iData1 <- tidyr::pivot_wider(df_filtered, names_from = "iCode", values_from = "Value")


# also add country names
iData1 <- merge(nuts2_codes_eu, iData1)
str(iData1)

# Order the dataset by uCode and Time
data <- iData1 %>%
  arrange(uCode, Time)


#Plot for visualizing data
# Define variables and their corresponding data
variables <- c(
  "Empl_tech_know",
  "Primary_Income_hh",
  "Pop_edu",
  "GDP_pc",
  "Empl_rate",
  "Hospitals_100k_inh",
  "Car_Travel_times",
  "EQI_Index"
)

sources <- c(
  "Eurostat",
  "Eurostat",
  "Eurostat",
  "Eurostat",
  "Eurostat",
  "GISCO/Eurostat",
  "EEA/EPSON",
  "University of Gothenburg"
)

units <- str_wrap(c(
  "Percentage of total employment                          ",
  "Million purchasing power standards (PPS, EU27 from 2020)",
  "Percentage                                               ",
  "Euro per inhabitant in percentage of the EU27 (from 2020) average)",
  "Percentage                                               ",
  "Per 100k people                                           ",
  "Minutes                                                   ",
  "NA                                                        "
), width = 35)

directionality <- c("+", "+", "+", "+", "+", "+", "-", "+")
year <- c(
  "2012-2021",
  "2012-2021",
  "2012-2021",
  "2012-2021",
  "2012-2021",
  "2020",
  "2013;2020",
  "2010;2013;2017;2021"
)

description <- str_wrap(c(
  "Employment in technology and knowledge-intensive sectors by NUTS 2 region and sex - Human health and social work activities",
  "Primary income of private households by NUTS 2 region",
  "Population by educational attainment level, sex and NUTS 2 region - Tertiary education - Age: 25-64 years old",
  "Gross domestic product (GDP) at current market prices by NUTS 2 region",
  "Employment rates by sex, age and NUTS 2 region",
  "Number of healthcare services by NUTS2 ",
  "Average travel time to the nearest hospital in minutes at NUTS2",
  "The European Quality of Government Index (EQI) assesses citizens' views on the quality, fairness, and corruption of public services"
), width = 35)

# Create a formatted data frame
summary_combined <- data.frame(
  Variable = variables,
  Description = description,
  Sources = sources,
  Unit = units,
  Polarity = directionality,
  Year = year,
  stringsAsFactors = FALSE
)


# Create a flextable from the data frame
flextable_table <- flextable(summary_combined)

# Adjust column widths for better formatting
flextable_table <- autofit(flextable_table)

# Set font size for the entire table
flextable_table <- fontsize(flextable_table, size = 14) 

# Align text in columns
flextable_table <- align(flextable_table, align = "center", part = "all")
flextable_table <- valign(flextable_table, valign = "top", part = "all")

# Set table properties for wrapping and layout
flextable_table <- set_table_properties(
  flextable_table, 
  layout = "autofit", 
  width = 1.0
)


# Save as an image
#save_as_image(flextable_table, path = "F_Variable_Description.png")


# 3. Data Imputation  ----
#Backward and forward imputation

# Function to calculate the percentage of missing values
missing_percentage <- function(x) {
  sum(is.na(x)) / length(x) * 100
}

# Apply the function to each column
missing_values <- sapply(data , missing_percentage)

# Print the result
print(missing_values)



# Perform forward and backward imputation within each group of uCode
data_imputed <- data %>%
  group_by(uCode) %>%
  mutate(across(where(is.numeric), ~ {
    if (all(is.na(.))) {
      .
    } else {
      na.locf(na.locf(., na.rm = FALSE), fromLast = TRUE)
    }
  }))


# Use mice for predictive mean matching imputation
# First, convert the data frame to one compatible with mice
data_mice <- as.data.frame(data_imputed)

# Perform predictive mean matching imputation
imputed_data <- mice(data_mice, method = 'pmm', m = 1, maxit = 5, seed = 123)

# Extract the complete dataset
data_final <- complete(imputed_data, 1)

# Checking missing values estimations and significance -----
# Function to calculate the percentage of missing values
missing_percentage <- function(x) {
  sum(is.na(x)) / length(x) * 100
}

# Apply the function to each column
missing_values <- sapply(data , missing_percentage)

# Print the result
print(missing_values)

# Apply the function to each column
missing_valuesimp <- sapply(data_final , missing_percentage)

# Print the result
print(missing_valuesimp)

# Function to create density plots
create_density_plot <- function(data_before, data_after, variable) {
  ggplot() +
    geom_density(data = data_before, aes_string(x = variable, color = '"Original"'), na.rm = TRUE) +
    geom_density(data = data_after, aes_string(x = variable, color = '"Imputed"'), alpha = 0.7) +
    labs(title = paste('Distribution of', variable),
         x = variable, y = 'Density') +
    theme_minimal() +
    scale_color_manual(values = c('Original' = 'blue', 'Imputed' = 'red'))
}

# Get the names of all numeric variables
numeric_vars <- data[,-3] %>%
  select(where(is.numeric)) %>%
  names()

# Create a list of density plots for all numeric variables
density_plots <- lapply(numeric_vars, function(var) {
  create_density_plot(data[,-3], data_final[,-3], var)
})

# Arrange plots in a panel (adjust ncol to fit your needs)
do.call(grid.arrange, c(density_plots, ncol = 2))

# Summary statistics for the original dataset
summary_original <- data %>%
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                           median = ~median(.x, na.rm = TRUE), 
                                           sd = ~sd(.x, na.rm = TRUE))))

summary_original <- summary_original[, -(1:3)]

# Summary statistics for the imputed dataset
summary_imputed <- data_final %>%
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                           median = ~median(.x, na.rm = TRUE), 
                                           sd = ~sd(.x, na.rm = TRUE))))
#Remove the first three lines
summary_imputed <- summary_imputed[, -(1:3)]


#Statistical test to check imputation -----

# Correlation matrix comparison
cor_original <- cor(data %>% select(where(is.numeric)), use = "complete.obs")
cor_imputed <- cor(data_final %>% select(where(is.numeric)))

print("Correlation matrix before imputation:")
print(cor_original)
print("Correlation matrix after imputation:")
print(cor_imputed)

# Visualize the correlation matrices
corrplot::corrplot(cor_original, method = "color", title = "Original Data Correlation Matrix", mar=c(0,0,1,0))
corrplot::corrplot(cor_imputed, method = "color", title = "Imputed Data Correlation Matrix", mar=c(0,0,1,0))

# Scatter plot matrix for visual comparison (optional)
pairs(data %>% select(where(is.numeric)), main = "Original Data", col = "blue")
pairs(data_final %>% select(where(is.numeric)), main = "Imputed Data", col = "red")

##. Printing the Missingness Table with Stargazer
# Combine missingness data into a single table
missing_table <- data.frame(
  Variable = names(missing_values),
  Missing_Percentage_Before = round(missing_values, 2),
  Missing_Percentage_After = round(missing_valuesimp, 2)
)

# Print the missingness table with stargazer
stargazer(missing_table, type = "text", summary = FALSE, title = "Missingness Table")

# Create a flextable
missing_table <- as.data.frame(missing_table)
ft <- flextable(missing_table) %>%
  set_caption(caption = "Missing data Percentage Before and After Imputation")

# Save as an image
#save_as_image(ft, path = "Missing_data_Percentage.png")


# Run statistical tests
ks_tests <- lapply(numeric_vars, function(var) {
  ks.test(data[[var]], data_final[[var]], alternative = "two.sided")
})

t_tests <- lapply(numeric_vars, function(var) {
  t.test(data[[var]], data_final[[var]], alternative = "two.sided")
})

wilcox_tests <- lapply(numeric_vars, function(var) {
  wilcox.test(data[[var]], data_final[[var]], alternative = "two.sided")
})


# Create a data frame of test results with p-values
test_results <- data.frame(
  Variable = rep(names(data[,-3] %>% select(where(is.numeric))), each = 3),
  Statistic = rep(c("Mean", "Median", "SD"), times = ncol(data[,-3] %>% select(where(is.numeric)))),
  Original = unlist(summary_original),
  Imputed = unlist(summary_imputed),
  KS_p_value = sapply(ks_tests, function(test) test$p.value),
  T_Test_p_value = sapply(t_tests, function(test) test$p.value),
  Wilcoxon_p_value = sapply(wilcox_tests, function(test) test$p.value)
)

# Round p-values for better readability
test_results <- test_results %>%
  mutate(across(ends_with("p_value"), round, 4))

# Print the results
stargazer(test_results, type = "text", summary = FALSE, title = "Statistical Test Results (Pre/Post Imputation)", rownames = FALSE)


# Create a flextable
test_results <- as.data.frame(test_results)
ft <- flextable(test_results) %>%
  set_caption(caption = "Statistical Test Results (Pre/Post Imputation)")


# Save as an image
# save_as_image(ft, path = "Statistics_Pre_Post_Imputation.png")


# Identify numeric columns, excluding 'uCode', 'uName', and 'Time'
numeric_cols <- names(data_final)[sapply(data_final, is.numeric) & !(names(data_imputed) %in% c("uCode", "uName", "Time"))]

# Pivot to long format
df_long <- data_final %>%
  pivot_longer(
    cols = all_of(numeric_cols),  # Pivot only numeric columns
    names_to = "iCode",           # New column for former column names
    values_to = "Value"           # New column for values
  )


df_long <- df_long[,-2] 


#4 Development o HAC INDEX by using COINr package and pursue format----
df_filtered <- df_long  

# pivot to wide
iData <- tidyr::pivot_wider(df_filtered, names_from = "iCode", values_from = "Value")

#iMeta
#The iMeta data frame specifies everything about each column in iData, 
#including whether it is an indicator, a group, or something else; its name, its units, and where it appears in the structure of the index. 
iMeta <- data.frame(
  iCode = unique(df_filtered$iCode),
  iName = unique(df_filtered$iCode),
  Direction = 1,
  Level = 1,
  Weight = 1,
  Type = "Indicator"
)


#Next we will edit the directions: some of our indicators have a negative directionality 
#(associated with lower values of skills).

# indicators with negative directionality # Correct this part
neg_directions <- c("Car_Travel_times")

# update iMeta
iMeta$Direction[iMeta$iCode %in% neg_directions] <- -1

# assign level 2 groupings
iMeta$Parent <- c("HC","WB","WB","WB","WB","HC","HC","HC")

#Now we must define level 2 itself:
iMeta_L2 <- data.frame(
  iCode = c("HC","WB"), #"K&T","Equ",
  iName = c("Health_care", 
            "Well_being"), #"Knowledge_and_Technology","Equity",
  Direction = 1,
  Level = 2,
  Weight = 1,
  Type = "Aggregate",
  Parent = c("HAC")
)

# add to iMeta
iMeta <- rbind(iMeta, iMeta_L2)

# (level 3 and the index level 4):
iMeta_L34 <- data.frame(
  iCode = c("HAC"),
  iName = c("Health_Adaptive_Capacity_Index"),
  Direction = 1,
  Level = c(3),
  Weight = 1,
  Type = "Aggregate",
  Parent = c(NA)
)

# add to iMeta
iMeta <- rbind(iMeta, iMeta_L34)

DT::datatable(iMeta, rownames = F)


#Pursue construction----
purse <- new_coin(iData = data_final, #data_final
                  iMeta = iMeta,
                  split_to = "all",
                  quietly = TRUE)


# Data treatment to all coins in purse (default specs)
purse <- Treat(purse, dset = "Raw",  winmax = NULL, f2 = "log_CT",na.rm = FALSE, 
               skew_thresh = 2,kurt_thresh = 3.5, f_pass = "check_SkewKurt")



###6 Normalisation (Min-Max)----
HAC <- Normalise(purse, dset = "Treated", global_specs = list(f_n = "n_minmax",
                                                              f_n_para = list(l_u = c(1,100))), global = FALSE)

#7 Arithemetic mean aggregation----
HAC <- Aggregate(HAC, dset = "Normalised", f_ag = "a_amean")

# Export dataset
#export_to_excel(HAC, fname = "HACcoin_export.xlsx", include_log = FALSE)

rm(list= ls()[! (ls() %in% c('Hospitals_100k_inh', 'Car_travel_times', 'EQI_Index',
                             'nuts2_codes','nuts2',"df_long", "HAC"))])



##Extra: Building a single year instance (2014) of the HAC INDEX by using COINr package ----

###HAC_2014 Index
df_filtered_2014 <- df_long  %>%
  filter(Time == 2014)

# Pivot to wide
iData <- tidyr::pivot_wider(df_filtered_2014, names_from = "iCode", values_from = "Value")

#iMeta
#The iMeta data frame specifies everything about each column in iData, 
#including whether it is an indicator, a group, or something else; its name, its units, and where it appears in the structure of the index. 
iMeta <- data.frame(
  iCode = unique(df_filtered_2014$iCode),
  iName = unique(df_filtered_2014$iCode),
  Direction = 1,
  Level = 1,
  Weight = 1,
  Type = "Indicator"
)

#Next we will edit the directions: some of our indicators have a negative directionality 
#(associated with lower values of skills).

# Indicators with negative directionality 
neg_directions <- c("Car_Travel_times")

# Update iMeta
iMeta$Direction[iMeta$iCode %in% neg_directions] <- -1

# Assign level 2 groupings
iMeta$Parent <- c("HC","WB","WB","WB","WB","HC","HC","HC")

#Now we must define level 2 itself:
iMeta_L2 <- data.frame(
  iCode = c("HC","WB"), #"K&T","Equ",
  iName = c("Health_care", 
            "Well_being"), #"Knowledge_and_Technology","Equity",
  Direction = 1,
  Level = 2,
  Weight = 1,
  Type = "Aggregate",
  Parent = c("HAC")
)

# add to iMeta
iMeta <- rbind(iMeta, iMeta_L2)

# (level 3 and the index level 4):
iMeta_L34 <- data.frame(
  iCode = c("HAC"),
  iName = c("Health_Adaptive_Capacity_Index"),
  Direction = 1,
  Level = c(3),
  Weight = 1,
  Type = "Aggregate",
  Parent = c(NA)
)

# add to iMeta
iMeta <- rbind(iMeta, iMeta_L34)

DT::datatable(iMeta, rownames = F)



#8.1 Initial Analysis: joining data presenting all the features characterizing the HAC index
HAC_2014 <- new_coin(iData, iMeta)

#Building the index
HAC_2014 <- qTreat(HAC_2014 , dset = "Raw",  winmax = NULL, f2 = "log_CT",na.rm = FALSE, 
                   skew_thresh = 2,kurt_thresh = 3.5, f_pass = "check_SkewKurt")

HAC_2014$Analysis$Treated$Dets_Table |>
  signif_df() |>
  DT::datatable(rownames = F)


### Normalisation (Min-Max)
HAC_2014 <- Normalise(HAC_2014, dset = "Treated", global_specs = list(f_n = "n_minmax",
                                                                      f_n_para = list(l_u = c(1,100))))

dset_normalised <- get_dset(HAC_2014, dset = "Normalised")

HAC_2014 <- Aggregate(HAC_2014, dset = "Normalised",
                      f_ag = "a_amean")

dset_aggregated <- get_dset(HAC_2014, dset = "Aggregated")

#Plotting correlations
plot_corr(HAC_2014, dset = "Normalised", grouplev = NULL, box_level = 3, use_directions = TRUE, cortype = "pearson", withparent = TRUE)
plot_corr(HAC_2014, dset = "Normalised", grouplev = 2, box_level = 3, use_directions = TRUE, cortype = "spearman")
plot_corr(HAC_2014, dset = "Normalised", grouplev = 2, box_level = 3, use_directions = TRUE, cortype = "kendall")

#Multivariate analysis: Cronbach’s alpha - this is a measure of internal consistency or “reliability” of the data, based on the strength of correlations between indicators.
get_cronbach(HAC_2014, dset = "Normalised", iCodes = "HC", Level = 1)
get_cronbach(HAC_2014, dset = "Normalised", iCodes = "WB", Level = 1)

#Robustness Tests
### Normalisation (z-score)

HAC_2014_b <- Normalise(HAC_2014, dset = "Treated",
                        global_specs = list(f_n = "n_zscore",
                                            f_n_para = list(c(10,2))))


#z-score method on the 0-1 interval
plot_dist(HAC_2014_b, dset = "Normalised", iCodes = "HAC", Level = 1, type = "Dot")
HAC_2014_b <- Aggregate(HAC_2014_b, dset = "Normalised", f_ag = "a_amean")


#Aggregation of the Index using the good old arithmetic mean (MIn-Max Normalization)
HAC_2014_b <- Aggregate(HAC_2014_b, dset = "Normalised", f_ag = "a_amean")

#dset_a

# compare index, sort by absolute rank difference
comparison <- compare_coins(HAC_2014, HAC_2014_b, dset = "Aggregated", iCode = "HAC",
              sort_by = "Abs.diff", decreasing = TRUE)

print(comparison)


#z-score method on the 0-1 interval
#plot_dist(HAC_2014_b, dset = "Normalised", iCodes = "HAC", Level = 1, type = "Dot")
#HAC <- Aggregate(HAC, dset = "Normalised", f_ag = "a_amean")

# get results table
df_HAC_2014 <- get_results(HAC_2014, dset = "Aggregated", tab_type = "Aggs")


###Sensitivity Analysis##

# component of SA_specs for winmax distribution
l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                 Distribution = NULL,
                 Type = "discrete")

# normalisation method

# first, we define the two alternatives: minmax or zscore (along with respective parameters)
norm_alts <- list(
  list(f_n = "n_minmax", f_n_para = list(c(1,100))),
  list(f_n = "n_zscore", f_n_para = list(c(10,2)))
)

# now put this in a list
l_norm <- list(Address = "$Log$Normalise$global_specs",
               Distribution = norm_alts,
               Type = "discrete")

# get nominal weights
w_nom <- HAC_2014$Meta$Weights$Original

# build data frame specifying the levels to apply the noise at
noise_specs = data.frame(Level = c(2,3),
                         NoiseFactor = c(0.25, 0.25))

# get 100 replications
noisy_wts <- get_noisy_weights(w = w_nom, noise_specs = noise_specs, Nrep = 100)

# examine one of the noisy weight sets
tail(noisy_wts[[1]])

# component of SA_specs for weights
l_weights <- list(Address = "$Log$Aggregate$w",
                  Distribution = noisy_wts,
                  Type = "discrete")

## aggregation
l_agg <- list(Address = "$Log$Aggregate$f_ag",
              Distribution = c("a_amean", "a_gmean"),
              Type = "discrete")

# create overall specification list
SA_specs <- list(
  #Winmax = l_winmax,
  Normalisation = l_norm,
  Weights = l_weights,
  Aggregation = l_agg
)

# Not run here: will take a few seconds to finish if you run this
#Uncertainty Analysis
SA_res <- get_sensitivity(HAC_2014, SA_specs = SA_specs, N = 100, SA_type = "UA",
                          dset = "Aggregated", iCode = "HAC")

plot_uncertainty(SA_res)

# Step 2: Extract the relevant data frame
# Assume the relevant data is in SAresults$results (adjust based on inspection)
SA_df <- SA_res$RankStats


class(SA_df$Nominal)

# Ensure Nominal is numeric for regression
SA_df_sorted <- SA_df %>%
  arrange(desc(as.numeric(Nominal)))

SA_df_sorted <- SA_df[order(-SA_df$Nominal), ]


# Add a regression line using Nominal as x-axis
final_plot_with_regression <- ggplot(SA_df_sorted, aes(x = Nominal, y = Median)) +
  geom_point(size = 3, color = "lightgreen") +  # Mean as a light green dot
  geom_errorbar(aes(ymin = Q5, ymax = Q95), width = 0.2, color = "gray50") +  # Error bars
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  # Regression line
  labs(
    title = "",
    x = "NUTS2 Region (Ordered by Nominal Value)",
    y = "Rank"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "white"),
    axis.text.x = element_blank(),  # Remove x-axis values
    axis.title.x = element_text(size = 12, face = "bold")  # Keep x-axis label
  )

# Display the updated plot
print(final_plot_with_regression)



#Sensitivity Analysis
SA_res <- get_sensitivity(HAC_2014, SA_specs = SA_specs, N = 100, SA_type = "SA",
                          dset = "Aggregated", iCode = "HAC")

plot_sensitivity(SA_res)

plot_sensitivity(SA_res, ptype = "box")

# run function removing elements in level 2
l_res <- remove_elements(HAC_2014, Level = 2, dset = "Aggregated", iCode = "HAC")

# get summary of rank changes
l_res$MeanAbsDiff

df_HAC_2014 <- get_results(HAC_2014, dset = "Aggregated", tab_type = "Aggs")

rm(list= ls()[! (ls() %in% c('df_filtered_2014', "HAC_2014", "df_long", "df_HAC_2014"))])

