#######################################
## Load and clean temperature data
## Date Created: Nov 25th 2024
## Last Modified: Nov 25th 2024
#######################################
## Using UTCI data from TEE: temperature extremes europe dataset


# load data [Update path if necessary to match local environment] --------
print("loading daily UTCI data...")

# load yearly temperature data at NUTS 3 -----------
utciyearly_nuts3 <- read_csv("Data/utciyearly_nuts3.csv")

# filter to years between 2014 and 2021 ------------
utci_temperature_data <- utciyearly_nuts3 %>%
  filter(year >= 2014 & year <= 2021)
