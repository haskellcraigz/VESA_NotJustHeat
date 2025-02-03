######################################
## SUPPLEMENTAL FIGURES
## Date Created: Dec. 10th 2024
## Last Modified: Feb 3rd 2025
#####################################

# Distribution of heat and cold extreme -----------

## heat ---------
fig_si1 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_heat_rank), color=NA) +
  scale_fill_gradient2(low = "#EA9999", high = "#650000",
                       mid = "#cc0000", 
                       midpoint = 757,
                       breaks = c(101, 757, 1414),
                       labels = c("low \n (0)", "mid \n (3)", "high \n (91)"),
                       name="Relative Number of Average Yearly Extreme Heat Days 2014-2020",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))

ggsave("Figures/si_heat.png", plot = fig_si1,
       width = 10, height = 6.7, units = "cm")




## cold --------

fig_si2 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_cold_rank), color=NA) +
  scale_fill_gradient2(low = "#9CD0F9", high = "#1D5179",
                       mid = "#3aa2f4", 
                       midpoint = 757,
                       breaks = c(1, 707, 1414),
                       labels = c("low \n (0)", "mid \n (113)", "high \n (251)"),
                       name="Relative Number of Average Yearly Extreme Cold Days 2014-2020",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))

ggsave("Figures/si_cold.png", plot = fig_si2,
       width = 10, height = 6.7, units = "cm")









# Distribution of A -------------------------

## scaled HDI ----------

fig_si3 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=A_scaled), color=NA) +
  scale_fill_gradient2(low =  "#ffe0e9", high = "#6f2138",  #"#b9375e"
                       mid = "#ff7aa2", 
                       midpoint = 0.8,
                       breaks = c(0.25, 1.25, 2.25, 3.25),
                       labels = c("0", "1 - EU Mean", "2", "3"),
                       name="Adaptive Capacity Multiplicative Factor, A",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))


ggsave("Figures/si_a.png", plot = fig_si3,
       width = 10, height = 6.7, units = "cm")

# Comparison of S with S defined as age 65+ or 75+  ----------
VESA_all$country <- str_sub(VESA_all$NUTS_ID, 1, 2)


fig_si4a <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=pop75rate_mean), color=NA) +
  scale_fill_gradient2(low = "#ADBB96", high = "#37481b",
                       mid = "#5d782e",  
                       midpoint = 0.1,
                       breaks = c(0.028, 0.1, 0.2),
                       labels = c("<3%", "10%", "20%"),
                       name="Proportion population age 75+",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.83),
        legend.title = element_text(face = 2,
                                    vjust = 1.5)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))


ggsave("Figures/si_4a.png", plot = fig_si4a,
       width = 10, height = 6.7, units = "cm")

fig_si4b <- ggplot(data = VESA_all) +
  geom_boxplot(aes(x = country, y = pop75rate_mean, group = country)) +
  theme_bw() +
  labs(title = "Within vs Between Country Variation in Proportion Older Adults",
       y = "Proportion of the population age 75+", x = "Country")


fig_si4c <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=pop65rate_mean), color=NA) +
  scale_fill_gradient2(low = "#5d782e", mid = "#37481b",
                       high = "#48421b",  
                       midpoint = 0.25,
                       breaks = c(0.15, 0.25, 0.35),
                       #labels = c("10%", "20%", "30%"),
                       name="Proportion population age 65+",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.83),
        legend.title = element_text(face = 2,
                                    vjust = 1.5)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))


ggsave("Figures/si_4c.png", plot = fig_si4c,
       width = 10, height = 6.7, units = "cm")

fig_si4d <- ggplot(data = VESA_all) +
  geom_boxplot(aes(x = country, y = pop65rate_mean, group = country)) +
  theme_bw() +
  labs(title = "Within vs Between Country Variation in Proportion Older Adults",
       y = "Proportion of the population age 65+", x = "Country")


# Spatial distribution of other measures of A (at NUTS2) -------------------------

A_avg <- A_df %>%
  group_by(geo) %>%
  mutate(mean_gdp = mean(GDP, na.rm = TRUE),
         mean_hdi = mean(HDI, na.rm = TRUE),
         mean_hac = mean(HAC, na.rm = TRUE),
         mean_life = mean(life_expect_birth, na.rm = TRUE)) %>%
  ungroup() %>%
  select(geo, starts_with("mean")) %>%
  unique()


A_avg.shp <- left_join(nuts2.shp, A_avg, by = c("NUTS_ID" = "geo"))

## HDI --------------

fig_si5a <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=A_avg.shp, aes(fill=mean_hdi), color=NA) +
  scale_fill_gradient2(low =  "#ffe0e9", high = "#6f2138",  
                       mid = "#ff7aa2", 
                       midpoint = 0.7,
                       name="Mean HDI (1990-2015)",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))


ggsave("Figures/si_5a_hdi.png", plot = fig_si5a,
       width = 10, height = 6.7, units = "cm")


## GDP -------
fig_si5b <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=A_avg.shp, aes(fill=mean_gdp), color=NA) +
  scale_fill_gradient2(low =  "#e0fff6", high = "#216f58", 
                       mid = "#7affd7", 
                       midpoint = 70,
                       name="Mean GDP (2000-2022)",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))


ggsave("Figures/si_5b_gdp.png", plot = fig_si5b,
       width = 10, height = 6.7, units = "cm")


## HAC -------------------

fig_si5c <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=A_avg.shp, aes(fill=mean_hac), color=NA) +
  scale_fill_gradient2(low =  "#c9d1e5", high = "#21386f",  
                       mid = "#7aa2ff", 
                       midpoint = 60,
                       breaks = c(20, 40, 60),
                       name="Mean HDI (2012-2021)",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))


ggsave("Figures/si_5c_hac.png", plot = fig_si5c,
       width = 10, height = 6.7, units = "cm")

## Life expectancy at birth -----------------

fig_si5d <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=A_avg.shp, aes(fill=mean_life), color=NA) +
  scale_fill_gradient2(low =  "#ffe7e0", high = "#cf8232",  
                       mid = "#ff957a", 
                       midpoint = 77.5,
                       name="Mean Life Expectancy at Birth (1990-2022)",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))


ggsave("Figures/si_5d_life.png", plot = fig_si5d,
       width = 10, height = 6.7, units = "cm")

# Comparisons of A with life expectancy at age 65 --------------
# calculate correlation coefficients -------------
correlations_L65 <- A_df %>%
  group_by(year) %>%
  mutate(hac_life_pearson_cor = cor(HAC, life_expect_65, method = "pearson",
                                    use =  "pairwise.complete.obs"),
         hac_life_spearman_cor = cor(HAC, life_expect_65, method = "spearman",
                                     use =  "pairwise.complete.obs"),
         life_hdi_pearson_cor = cor(life_expect_65, HDI, method = "pearson",
                                    use =  "pairwise.complete.obs"),
         life_hdi_spearman_cor = cor(life_expect_65, HDI, method = "spearman",
                                     use =  "pairwise.complete.obs"),
         life_gdp_spearman_cor = cor(life_expect_65, GDP, method = "spearman",
                                     use =  "pairwise.complete.obs"),
         life_gdp_pearson_cor = cor(life_expect_65, GDP, method = "pearson",
                                    use =  "pairwise.complete.obs")) %>%
  dplyr::select(year, ends_with("cor")) %>%
  unique()

# summarize correlations table
mean_correlations <- correlations_L65 %>% 
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


