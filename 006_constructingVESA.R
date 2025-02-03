######################################
## CONSTRUCTION AND COMPARISON OF V=ES AND V=ESA
## Date Created: Dec. 10th 2024
## Last Modified: Dec. 10th 2024
#####################################

# FIX:: Specify which NUTSID to include in analysis --------------
exclude_nuts3 <- c("EE009", "EE00A", #Estonia, missing population age data for all years
                   #UK missing data for all years
                   "UKK24", "UKK25", "UKN0A", "UKN0B", "UKN0C", "UKN0D", "UKN0E",
                   "UKN0F", "UKN0G",
                   #Norway, I believe these IDs are not currently in use, missing 
                   # most variables for most years
                   "NO0B1", "NO0B2",
                   #missing at leats one year of temperature data
                   "UKM66", #UK Shetland Islands
                   "FRY10", "FRY20", "FRY30", "FRY40", "FRY50") #French departements d'outre mer

# Join E, S, A at NUTS3 level into single data table -----------
## Join datasets together
data <- left_join(utciyearly_nuts3, pop_nuts3_clean, 
                       by = c("NUTS_ID" = "geo", "year"))
data <- left_join(data, 
                       dplyr::select(hdi_long_nuts3_2014, NUTS_ID, HDI), #HDI for 2014
                  by = c("NUTS_ID")) 

## filter to time period 2014-2021
data <- data %>%
  filter(year >=2014 & year <= 2021) 

# create VESA --------------------------

## yearly components of VESA
VESA_yearly <- data %>%
  filter(!NUTS_ID %in% exclude_nuts3) %>% 
  #keep just E, S, A components for VESA 
  dplyr::select(NUTS_ID, year,
         utci_below_0, utci_above_26, #number of days defined as 'extreme' temp
         prop_GE65, prop_GE75, #proportion of population >65 and >75
         HDI) %>% #HDI for each nuts3 region taken from the 2014 value
  # A = 1 - HDI so scaling is same direction for all components
  mutate(A = 1 - HDI, 
         A_scaled = A/mean(A, na.rm = T)) 



# combined V over entire period 2014-2021 for study region 
VESA_all <- VESA_yearly %>%
  filter(NUTS_ID %in% hdi_long_nuts3_2014$NUTS_ID) %>%
  group_by(NUTS_ID) %>%
  #Sum/average over the entire period
  mutate(temp_extreme_cold = sum(utci_below_0),
         temp_extreme_hot = sum(utci_above_26),
        #note: UK missing S data for 2020, 
        # IT Sicily missing 2018-2020 (ITG2D, ITG2E, ITG2H, ITG2G)
        # RS Serbia missing data 2014-2016 
         pop65rate_mean = mean(prop_GE65, na.rm = T),
         pop75rate_mean = mean(prop_GE75, na.rm = T)) %>%
  #select only summarized measures for entire study period
  dplyr::select(NUTS_ID, temp_extreme_cold, temp_extreme_hot, 
         pop65rate_mean, pop75rate_mean,
         HDI, A, A_scaled) %>%
  unique() %>% #one row per nuts ID
  mutate(temp_extreme_total = temp_extreme_cold + temp_extreme_hot) %>% #create count of combined mean extreme days
  mutate(E_S_over65 = temp_extreme_total * pop65rate_mean,
         E_S_over75 = temp_extreme_total * pop75rate_mean,
         E_S_A_over65 = temp_extreme_total * pop65rate_mean * A_scaled,
         E_S_A_over75 = temp_extreme_total * pop75rate_mean * A_scaled)

# Calculate % change with A --------------------------------------
VESA_all <- VESA_all %>%
  mutate(A_change = (E_S_A_over65 - E_S_over65)/E_S_over65,
         A_change_percent = (E_S_A_over65 - E_S_over65)/E_S_over65 * 100)


# Rank regions by V --------------------------

## create rankings -------------------------------------

## exposure (hot, cold and extreme temperature)
VESA_all$E_heat_rank = rank(VESA_all$temp_extreme_hot, ties.method = "average")
VESA_all$E_cold_rank = rank(VESA_all$temp_extreme_cold, ties.method = "average")
VESA_all$E_total_rank = rank(VESA_all$temp_extreme_total, ties.method = "average")
VESA_all$S_over65_rank = rank(VESA_all$pop65rate_mean)
VESA_all$A_rank = rank(VESA_all$HDI)
VESA_all$E_S_over65_rank = rank(VESA_all$E_S_over65)
VESA_all$E_S_A_over65_rank = rank(VESA_all$E_S_A_over65)




# Join dataset to shapefile for subsequent plots -----------------
VESA_all.shp <- left_join(nuts3.shp, VESA_all)







