#######################################
## Load and clean population data
## Date Created: Nov 25th 2024
## Last Modified: Nov 25th 2024
#######################################

# load data -------
print('loading population data ...')
# Population data
pop_nuts2 <- get_eurostat("demo_r_d2jan")
pop_nuts3 <- get_eurostat("demo_r_pjangrp3")


# Data cleaning -----------------------

## NUTS2 ----------------------------------

## population data - match nuts2 categories to nuts3 categories

Y_LT5 <- c("Y_LT1", "Y1", "Y2", "Y3", "Y4")
Y5_9 <- c("Y5", "Y6", "Y7", "Y8", "Y9")
Y10_14 <- c("Y10", "Y11", "Y12", "Y13", "Y14")
Y15_19 <- c("Y15", "Y16", "Y17", "Y18", "Y19")
Y20_24 <- c("Y20", "Y21", "Y22", "Y23", "Y24")
Y25_29 <- c("Y25", "Y26", "Y27", "Y28", "Y29")
Y30_34 <- c("Y30", "Y31", "Y32", "Y33", "Y34")
Y35_39 <- c("Y35", "Y36", "Y37", "Y38", "Y39")
Y40_44 <- c("Y40", "Y41", "Y42", "Y43", "Y44")
Y45_49 <- c("Y45", "Y46", "Y47", "Y48", "Y49")
Y50_54 <- c("Y50", "Y51", "Y52", "Y53", "Y54")
Y55_59 <- c("Y55", "Y56", "Y57", "Y58", "Y59")
Y60_64 <- c("Y60", "Y61", "Y62", "Y63", "Y64")
Y65_69 <- c("Y65", "Y66", "Y67", "Y68", "Y69")
Y70_74 <- c("Y70", "Y71", "Y72", "Y73", "Y74")
Y75_79 <- c("Y75", "Y76", "Y77", "Y78", "Y79")
Y80_84 <- c("Y80", "Y81", "Y82", "Y83", "Y84")
Y85_89 <- c("Y85", "Y86", "Y87", "Y88", "Y89")
Y_GE85 <- c("Y85", "Y86", "Y87", "Y88", "Y89",
            "Y90", "Y91", "Y92", "Y93", "Y94",
            "Y95", "Y96", "Y97", "Y98", "Y99")
Y_GE90 <- c("Y90", "Y91", "Y92", "Y93", "Y94",
            "Y95", "Y96", "Y97", "Y98", "Y99")


## create additional thresholds 
Y_GE80 <- c("Y80", "Y81", "Y82", "Y83", "Y84",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y90", "Y91", "Y92", "Y93", "Y94",
            "Y95", "Y96", "Y97", "Y98", "Y99")

Y_GE75 <- c("Y75", "Y76", "Y77", "Y78", "Y79",
            "Y80", "Y81", "Y82", "Y83", "Y84",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y90", "Y91", "Y92", "Y93", "Y94",
            "Y95", "Y96", "Y97", "Y98", "Y99")

Y_GE70 <- c("Y70", "Y71", "Y72", "Y73", "Y74",
            "Y75", "Y76", "Y77", "Y78", "Y79",
            "Y80", "Y81", "Y82", "Y83", "Y84",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y90", "Y91", "Y92", "Y93", "Y94",
            "Y95", "Y96", "Y97", "Y98", "Y99")

Y_GE65 <- c("Y65", "Y66", "Y67", "Y68", "Y69",
            "Y70", "Y71", "Y72", "Y73", "Y74",
            "Y75", "Y76", "Y77", "Y78", "Y79",
            "Y80", "Y81", "Y82", "Y83", "Y84",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y90", "Y91", "Y92", "Y93", "Y94",
            "Y95", "Y96", "Y97", "Y98", "Y99")

Y_GE60 <- c("Y60", "Y61", "Y62", "Y63", "Y64",
            "Y65", "Y66", "Y67", "Y68", "Y69",
            "Y70", "Y71", "Y72", "Y73", "Y74",
            "Y75", "Y76", "Y77", "Y78", "Y79",
            "Y80", "Y81", "Y82", "Y83", "Y84",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y90", "Y91", "Y92", "Y93", "Y94",
            "Y95", "Y96", "Y97", "Y98", "Y99")

Y_GE55 <- c("Y55", "Y56", "Y57", "Y58", "Y59",
            "Y60", "Y61", "Y62", "Y63", "Y64",
            "Y65", "Y66", "Y67", "Y68", "Y69",
            "Y70", "Y71", "Y72", "Y73", "Y74",
            "Y75", "Y76", "Y77", "Y78", "Y79",
            "Y80", "Y81", "Y82", "Y83", "Y84",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y85", "Y86", "Y87", "Y88", "Y89",
            "Y90", "Y91", "Y92", "Y93", "Y94",
            "Y95", "Y96", "Y97", "Y98", "Y99")

## generate list of nuts2 and nuts3 codes
nuts2 = nuts.shp %>% 
  st_drop_geometry() %>% 
  filter(LEVL_CODE == 2) %>% 
  select(NUTS_ID) %>% 
  rename(geo = NUTS_ID)

nuts3 = nuts.shp %>% 
  st_drop_geometry() %>% 
  filter(LEVL_CODE == 3) %>% 
  select(NUTS_ID) %>% 
  rename(geo = NUTS_ID)

### Calculate proportion ------------------------------

# keeping nuts2 regions only
pop_nuts2_clean <- left_join(nuts2, pop_nuts2, by=c("geo"))

pop_nuts2_clean <- pop_nuts2_clean %>%
  filter(unit == 'NR', sex == "T") %>% #number and not stratified by sex
  # filter(str_detect(geo, ".*[0-9][0-9].*")) %>% #keep NUTS2 regions only (2 digits)
  group_by(geo, TIME_PERIOD, sex) %>%
  pivot_wider(id_cols = c(geo, TIME_PERIOD, sex), names_from = age, values_from = values) %>%
  # match 5 year groups that are in NUTS3 (for older adults)
  rowwise() %>%
  mutate(year = lubridate::year(TIME_PERIOD),
         
         Y50_54 = sum(c_across(any_of(Y50_54)), na.rm = T),
         Y55_59 = sum(c_across(any_of(Y55_59)), na.rm = T),
         Y60_64 = sum(c_across(any_of(Y60_64)), na.rm = T),
         Y65_69 = sum(c_across(any_of(Y65_69)), na.rm = T),
         Y70_74 = sum(c_across(any_of(Y70_74)), na.rm = T),
         Y75_79 = sum(c_across(any_of(Y75_79)), na.rm = T),
         Y80_84 = sum(c_across(any_of(Y80_84)), na.rm = T),
         Y85_89 = sum(c_across(any_of(Y85_89)), na.rm = T),
         # greater than or equal to ....
         Y_GE55 = sum(c_across(any_of(Y_GE55)), na.rm = T),
         Y_GE60 = sum(c_across(any_of(Y_GE60)), na.rm = T),
         Y_GE65 = sum(c_across(any_of(Y_GE65)), na.rm = T),
         Y_GE70 = sum(c_across(any_of(Y_GE70)), na.rm = T),
         Y_GE75 = sum(c_across(any_of(Y_GE75)), na.rm = T),
         Y_GE80 = sum(c_across(any_of(Y_GE80)), na.rm = T),
         Y_GE85 = sum(c_across(any_of(Y_GE85)), na.rm = T),
         Y_GE90 = sum(c_across(any_of(Y_GE90)), na.rm = T),
         # proportion of pop GE
         prop_GE55 = Y_GE55/TOTAL,
         prop_GE60 = Y_GE60/TOTAL,
         prop_GE65 = Y_GE65/TOTAL,
         prop_GE70 = Y_GE70/TOTAL,
         prop_GE75 = Y_GE75/TOTAL,
         prop_GE80 = Y_GE80/TOTAL,
         prop_GE85 = Y_GE85/TOTAL,
         prop_GE90 = Y_GE90/TOTAL)


## NUTS3 ---------------------

## create additional thresholds 
Y_GE80 <-c("Y80-84", "Y85-89", "Y_GE90")

Y_GE75 <- c("Y75-79", "Y80-84", "Y85-89", "Y_GE90")

Y_GE70 <- c("Y70-74",
            "Y75-79", "Y80-84", "Y85-89", "Y_GE90")

Y_GE65 <- c("Y65-69", "Y70-74",
            "Y75-79", "Y80-84", "Y85-89", "Y_GE90")

Y_GE60 <-c("Y60-64", "Y65-69", "Y70-74",
           "Y75-79", "Y80-84", "Y85-89", "Y_GE90")

Y_GE55 <- c("Y55-59", "Y60-64", "Y65-69", "Y70-74",
            "Y75-79", "Y80-84", "Y85-89", "Y_GE90")





### Calculate proportion ------------------------------

# keeping only nuts 3 regions
pop_nuts3_clean <- left_join(nuts3, pop_nuts3, by=c("geo"))

pop_nuts3_clean <- pop_nuts3 %>%
  filter(unit == 'NR', sex == "T") %>% #number and not stratified by sex
  # filter(str_detect(geo, ".*[0-9][0-9][0-9].*")) %>% #keep NUTS3 regions only (3 digits)
  group_by(geo, TIME_PERIOD, sex) %>%
  pivot_wider(id_cols = c(geo, TIME_PERIOD, sex), names_from = age, values_from = values) %>%
  rowwise() %>%
  mutate(
    year = lubridate::year(TIME_PERIOD),
    # greater than or equal to ....
    Y_GE55 = sum(c_across(any_of(Y_GE55)), na.rm = T),
    Y_GE60 = sum(c_across(any_of(Y_GE60)), na.rm = T),
    Y_GE65 = sum(c_across(any_of(Y_GE65)), na.rm = T),
    Y_GE70 = sum(c_across(any_of(Y_GE70)), na.rm = T),
    Y_GE75 = sum(c_across(any_of(Y_GE75)), na.rm = T),
    Y_GE80 = sum(c_across(any_of(Y_GE80)), na.rm = T),
    # proportion of pop GE
    prop_GE55 = Y_GE55/TOTAL,
    prop_GE60 = Y_GE60/TOTAL,
    prop_GE65 = Y_GE65/TOTAL,
    prop_GE70 = Y_GE70/TOTAL,
    prop_GE75 = Y_GE75/TOTAL,
    prop_GE80 = Y_GE80/TOTAL,
    prop_GE85 = Y_GE85/TOTAL,
    prop_GE90 = Y_GE90/TOTAL)

# Keep only cleaned dataframes in environment -------

rm("Y_GE55", "Y_GE60",               
   "Y_GE65", "Y_GE70","Y_GE75","Y_GE80","Y_GE85","Y_GE90",               
   "Y_LT5","Y10_14","Y15_19","Y20_24","Y25_29","Y30_34",               
   "Y35_39","Y40_44","Y45_49","Y5_9","Y50_54","Y55_59",               
   "Y60_64","Y65_69","Y70_74","Y75_79","Y80_84","Y85_89",
   "pop_nuts3", "pop_nuts2")

