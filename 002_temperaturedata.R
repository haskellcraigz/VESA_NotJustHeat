#######################################
## Load and clean temperature data
## Date Created: Nov 25th 2024
## Last Modified: Apr 7th 2026
#######################################
## Using UTCI data from TEE: temperature extremes europe dataset

# load data [Update path if necessary to match local environment] --------
print("loading daily UTCI data...")

# load yearly temperature data at NUTS 3 -----------
#utciyearly_nuts3 <- read_csv("Data/utciyearly_nuts3.csv")
tempyearly_nuts3 <- read_csv("Data/teeyearly_nuts3.csv")

# filter to years between 2014 and 2021 ------------
utci_temperature_data <- tempyearly_nuts3 %>%
  select(nuts_id, year, utci_above_26, utci_below_0) %>%
  filter(year >= 2014 & year <= 2023)


# Sensitivity Analysis: other measures of temperature extremes --------------------

tempyearly_nuts3 <- read_csv("Data/teeyearly_nuts3.csv")

## filter to years between 2014 and 2021 ------------
tempyearly_nuts3 <- tempyearly_nuts3 %>%
  filter(year >= 2014 & year <= 2023)





