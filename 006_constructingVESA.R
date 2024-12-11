######################################
## CONSTRUCTION AND COMPARISON OF V=ES AND V=ESA
## Date Created: Dec. 10th 2024
## Last Modified: Dec. 10th 2024
#####################################


# Join E, S, A at NUTS3 level into single data table -----------
## Join datasets together
data <- left_join(utciyearly_nuts3, pop_nuts3_clean, 
                       by = c("NUTS_ID" = "geo", "year"))
data <- left_join(data, 
                       select(hdi_long_nuts3_2014, NUTS_ID, HDI), #HDI for 2014
                  by = c("NUTS_ID")) 

## filter to time period 2014-2021
data <- data %>%
  filter(year >=2014 & year <= 2021) 

# create VESA --------------------------

## yearly components of VESA
VESA_yearly <- data %>%
  #keep just E, S, A components for VESA 
  select(NUTS_ID, year,
         utci_below_0, utci_above_26, #number of days defined as 'extreme' temp
         prop_GE65, prop_GE75, #proportion of population >65 and >75
         HDI) %>% #HDI for each nuts3 region taken from the 2014 value
  # A = 1 - HDI so scaling is same direction for all components
  mutate(A = 1 - HDI, A_scaled = A/mean(A, na.rm = T)) 



# combined V over entire period 2014-2021
data_extreme <- VESA_yearly %>%
  group_by(NUTS_ID) %>%
  #Sum/average over the entire period
  mutate(temp_extreme_cold = sum(utci_below_0),
         temp_extreme_hot = sum(utci_above_26),
         pop65rate_mean = mean(prop_GE65),
         pop75rate_mean = mean(prop_GE75)) %>%
  #select only summarized measures for entire study period
  select(NUTS_ID, temp_extreme_cold, temp_extreme_hot, 
         pop65rate_mean, pop75rate_mean,
         HDI, A, A_scaled) %>%
  unique() %>% #one row per nuts ID
  mutate(temp_extreme_mean = temp_extreme_cold + temp_extreme_hot) %>% #create count of combined mean extreme days
  mutate(E_S_over65 = temp_extreme_mean * pop65rate_mean,
         E_S_over75 = temp_extreme_mean * pop75rate_mean,
         E_S_A_over65 = temp_extreme_mean * pop65rate_mean * A_scaled,
         E_S_A_over75 = temp_extreme_mean * pop75rate_mean * A_scaled)
