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
# This script constructs the adaptive capacity multiplicative factor from HDI 
# and calculates vulnerability for each nuts3 region 
source("006_constructingVESA.R")

# Scripts for creating main paper figures -----------
# Script includes code for generating figure 1, 2 and 3
source("007_mainfigures.R")

# Scripts for creating  SI figures -------
# Script includes code for supplemental maps and plots on E, S and V, 
# Code for the figures describing the development of the HAC index are contained
# in HAC.R
source("008_supplementalfigures.R")


