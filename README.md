
Repo supporting Not Just Heat: place-based vulnerability to temperature extremes overestimated in Europe without including measures of adaptive capacity

Results from the paper are in `Figures/Main` and `Figures/SI`. Raw data is publicly available online.

## How to replicate results

### File structure
- Home directory contains the following scripts that load, clean, analyse data and generate figures
```
[1] "000_packages.R"                 "001_europeshapefiles.R"         "002_temperaturedata.R"         
[4] "003_populationdata.R"           "004_adaptivecapacitymeasures.R" "005_comparingA.R"              
[7] "006_constructingVESA.R"         "007_mainfigures.R"              "008_supplementalfigures.R"
```
- `Main.R` umbrella script that sources scripts listed above for analysis and figures
- `HAC/` directory with scripts and data for generating HAC index
- `Figures/` directory for saving figure outputs of `Main`
- `Data/` directory for storing raw data 


### Data sources
- Temperature data file "utciyearly_nuts3.csv" can be downloaded from Ronnkvist et al. (2025) TEE:temperature extremes europe available at [X]
- HDI raster data from Kummu et al. (2018) available at [Dryad Data](https://datadryad.org/stash/dataset/doi:10.5061/dryad.dk1j0)
- NUTS shapefiles for the year 2021 (EPSG:4326) from Eurostat available at [Eurostat_NUTS](https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units/territorial-units-statistics)
- Country shapefiles for the year 2020 (EPSG:4326) from Eurostat available at [Eurostat_Country](https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries)
- Country, NUTS2, and NUTS3 cross-reference table from Eurostat available at [NUTS classification](https://ec.europa.eu/eurostat/web/nuts)

The following files must be in the `Data/` folder for the code to run:
```
[1] "CNTR_RG_10M_2020_4326.cpg" "CNTR_RG_10M_2020_4326.dbf" "CNTR_RG_10M_2020_4326.prj" "CNTR_RG_10M_2020_4326.shp"
[5] "CNTR_RG_10M_2020_4326.shx" "dailyutci_nuts3.csv"       "HAC_2012_2016.xlsx"        "HAC_2017_2021.xlsx"       
[9] "HDI_1990_2015_v2.nc"       "NUTS_RG_10M_2021_4326.cpg" "NUTS_RG_10M_2021_4326.dbf" "NUTS_RG_10M_2021_4326.prj"
[13] "NUTS_RG_10M_2021_4326.shp" "NUTS_RG_10M_2021_4326.shx" "NUTS2021-NUTS2024.xlsx"    "utciyearly_nuts3.csv"
```


### Replicating results
1. Download raw data files listed under "Data sources" and add into `Data` folder. Note: pre-generated excel (.xlsx) files with the Health Adaptive Capacity (HAC) Index is already saved in this folder, code to replicate the HAC is available in the `HAC` folder. 
2. Install packages listed in `000_packages.R`
3. Generate HAC (OPTIONAL)
   - Navigate to `HAC` folder and run script to generate HAC index  
   - Copy the data files containing the HAC index into the `Data` folder
4. In Home folder, run `Main.R`



## Computational environment
```
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20
Running under: macOS Sonoma 14.5
```

R packages and versions are specified below.
```
attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] lme4_1.1-35.5        Matrix_1.7-0         readxl_1.4.3         eurostat_4.0.0       lubridate_1.9.3      forcats_1.0.0       
 [7] stringr_1.5.1        dplyr_1.1.4          purrr_1.0.2          readr_2.1.5          tidyr_1.3.1          tibble_3.2.1        
[13] ggplot2_3.5.1        tidyverse_2.0.0      sf_1.0-19            exactextractr_0.10.0 terra_1.7-78        

loaded via a namespace (and not attached):
 [1] gtable_0.3.5       raster_3.6-30      httr2_1.0.6        lattice_0.22-6     tzdb_0.4.0         vctrs_0.6.5       
 [7] tools_4.4.1        ISOweek_0.6-2      generics_0.1.3     curl_5.2.2         proxy_0.4-27       fansi_1.0.6       
[13] RefManageR_1.4.0   pkgconfig_2.0.3    KernSmooth_2.23-24 data.table_1.16.0  assertthat_0.2.1   lifecycle_1.0.4   
[19] compiler_4.4.1     munsell_0.5.1      codetools_0.2-20   class_7.3-22       nloptr_2.1.1       pillar_1.9.0      
[25] MASS_7.3-60.2      regions_0.1.8      classInt_0.4-10    boot_1.3-30        nlme_3.1-164       countrycode_1.6.0 
[31] tidyselect_1.2.1   digest_0.6.37      stringi_1.8.4      splines_4.4.1      rprojroot_2.0.4    bibtex_0.5.1      
[37] grid_4.4.1         here_1.0.1         colorspace_2.1-1   cli_3.6.3          magrittr_2.0.3     utf8_1.2.4        
[43] e1071_1.7-14       withr_3.0.1        scales_1.3.0       backports_1.5.0    rappdirs_0.3.3     sp_2.1-4          
[49] timechange_0.3.0   httr_1.4.7         cellranger_1.1.0   hms_1.1.3          rlang_1.1.4        Rcpp_1.0.13       
[55] glue_1.7.0         DBI_1.2.3          xml2_1.3.6         minqa_1.2.8        rstudioapi_0.16.0  jsonlite_1.8.8    
[61] R6_2.5.1           plyr_1.8.9         units_0.8-5       
```


