######################################
## ANALYSIS & COMPARISON OF HDI AND OTHER MEASURES OF A
## Date Created: Nov. 25th 2024
## Last Modified: Jan. 31 2025
#####################################

# join all datasets  ---------
#start with life-expectancy since this covers the full range of years 1990 - 2022
A_df <- left_join(life_expect_65, gdp)
A_df <- left_join(A_df, life_expect_birth)
A_df <- left_join(A_df, HAC_long)
A_df <- left_join(A_df, hdi_long_nuts2, by = c("geo" = "NUTS_ID", "year"))

# keep only NUTS2 IDs for which we were able to calculate an HAC
nuts2_HAC_ids <- unique(HAC_long$geo)
A_df <- A_df %>%
  filter(geo %in% nuts2_HAC_ids)


# calculate correlation coefficients -------------
correlations <- A_df %>%
  group_by(year) %>%
  mutate(hac_hdi_pearson_cor = cor(HAC, HDI, method = "pearson",
                                   use =  "pairwise.complete.obs"),
         hac_hdi_spearman_cor = cor(HAC, HDI, method = "spearman",
                                    use =  "pairwise.complete.obs"),
         hac_gdp_pearson_cor = cor(HAC, GDP, method = "pearson",
                                   use =  "pairwise.complete.obs"),
         hac_gdp_spearman_cor = cor(HAC, GDP, method = "spearman",
                                    use =  "pairwise.complete.obs"),
         hdi_gdp_spearman_cor = cor(HDI, GDP, method = "spearman",
                                    use =  "pairwise.complete.obs"),
         hdi_gdp_pearson_cor = cor(HDI, GDP, method = "pearson",
                                   use =  "pairwise.complete.obs"),
         hac_life_pearson_cor = cor(HAC, life_expect_birth, method = "pearson",
                                 use =  "pairwise.complete.obs"),
         hac_life_spearman_cor = cor(HAC, life_expect_birth, method = "spearman",
                                  use =  "pairwise.complete.obs"),
         life_hdi_pearson_cor = cor(life_expect_birth, HDI, method = "pearson",
                                 use =  "pairwise.complete.obs"),
         life_hdi_spearman_cor = cor(life_expect_birth, HDI, method = "spearman",
                                  use =  "pairwise.complete.obs"),
         life_gdp_spearman_cor = cor(life_expect_birth, GDP, method = "spearman",
                                  use =  "pairwise.complete.obs"),
         life_gdp_pearson_cor = cor(life_expect_birth, GDP, method = "pearson",
                                 use =  "pairwise.complete.obs")) %>%
  dplyr::select(year, ends_with("cor")) %>%
  unique()

# summarize correlations table
mean_correlations <- correlations %>% 
  ungroup %>%
  dplyr::select(contains("spearman")) %>%
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))

min_correlations <- correlations %>% 
  ungroup %>%
  dplyr::select(contains("spearman")) %>%
  summarize(across(everything(), ~ min(.x, na.rm = TRUE)))

max_correlations <- correlations %>% 
  ungroup %>%
  dplyr::select(contains("spearman")) %>%
  summarize(across(everything(), ~ max(.x, na.rm = TRUE)))




# WITHIN/BETWEEN region variation in HDI --------------------------------

## load cross-tabulation table for country to nuts2 to nuts3 ------------
crosstab <- readxl::read_xlsx(path = "Data/NUTS2021-NUTS2024.xlsx", 
                              sheet = 3) #For 2021



## Add column for country and NUTS2 ID ----------------

hdi_nested <- hdi_long_nuts3 %>%
  mutate(country_id = substr(NUTS_ID, 1, 2))

## add column for NUTS2 level (identified by all digits except last digit of NUTS3 ID)
hdi_nested <- hdi_nested %>%
  mutate(nuts2_id = substr(NUTS_ID, 1, nchar(NUTS_ID) - 1))


## use cross tab file to check if all nuts2_id s are correctly NUTS2 level codes
check <- left_join(dplyr::select(hdi_nested, NUTS_ID, nuts2_id),
                   dplyr::select(crosstab, `Code 2021`, `NUTS level`),
                   by = c("nuts2_id" = "Code 2021"))

#sum(check$`NUTS level` == 2, na.rm = T) #number of nuts2 IDS correctly identified as NUTS2 regions
#sum(check$`NUTS level` != 2, na.rm = T) #number of nuts2 IDS incorrectly identified
#sum(is.na(check$`NUTS level`)) #number of nuts2 region ids not appearing in cross tab (ie region ID not correctly produced)
#View(filter(check, is.na(`NUTS level`)))

#View(filter(crosstab, `Code 2021` %in% filter(check, is.na(`NUTS level`))$NUTS_ID))

# these regions not found in the cross tab are located in switzerland (CH), UK, AL, Turkiye (TR) etc
# so I feel confident that the data is missing from the crosstab for 2021 rather than representing
# incorrect indexing of NUTS3 into NUTS2


## calculate between and within variance with null MLM -------------


# just nuts3 and countries
m0 <- lmer(HDI ~ 1 + 
             (1 | country_id),
           data = hdi_nested)



# extract variance from random effects
v <- as.data.frame(VarCorr(m0))

tot_v <- sum(v[,4]) #total variance
prop_v_res <- v[2, 4]/tot_v #proportion attributed to within country
# prop_v_res = 0.3666084
prop_v_country <- v[1, 4]/tot_v #proportion attributed to between country
# prop_v_country = 0.6333916



# nuts2, nuts3 and countries
m1 <- lmer(HDI ~ 1 + (1 | nuts2_id) +
             (1 | country_id),
           data = hdi_nested)



# extract variance from random effects
v1 <- as.data.frame(VarCorr(m1))

tot_v1 <- sum(v1[,4]) #total variance
prop_v1_res <- v1[3, 4]/tot_v1 #proportion attributed to within country
#prop_v1_res = 0.2932262
prop_v1_country <- v1[2, 4]/tot_v1 #proportion attributed to between country
#prop_v1_country = 0.5606355
prop_v1_nuts2 <- v1[1, 4]/tot_v1 #proportion attributed to between nuts2
#prop_v1_nuts2 = 0.1461384


