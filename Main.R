#######################################
## Main script
## Date Created: Nov 21st 2024
## Last Modified: Dec 10th 2024
#######################################


# packages -------------------------
source("000_packages.R")

# load NUTS2 and NUTS3 shapefiles ---------
source("001_europeshapefiles.R")

# load temperature data --------------
# Using daily UTCI data from TEE temperature extremes europe dataset
source("002_temperaturedata.R")

# load and clean population data ----------
# Note this step may take a few minutes
source("003_populationdata.R")

# load and clean adaptive capacity data ------------
source("004_adaptivecapacitymeasures.R")

# Comparison of measures for A ----------
source("005_comparingA.R")

# Computing V=ES and V=ESA -----------
# based on nuts3_ESA_rank_comparison.R file

# Scripts for creating temperature figures -----------

# Scripts for creating xxx figures -------


