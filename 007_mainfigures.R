######################################
## PLOTTING MAPS OF E, S, A, and VESA
## Date Created: Dec. 10th 2024
## Last Modified: Dec. 10th 2024
#####################################

# export path [Update path if necessary to match local environment]-------
export_path <- "Figures/"


# Figure 1 ------------------

## E (extreme) --------------------- 
#summary(data_shp$temp_extreme_mean)

ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_total_rank), color=NA) +
  scale_fill_gradient2(low = "#C599C3", high = "#370035",
                       mid = "#7C1A79", 
                       midpoint = 757,
                       breaks = c(2, 757, 1514),
                       labels = c("low \n (0.71)", "mid \n (108)", "high \n (344)"),
                       name="Relative Number of Days",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  # labs(title = "Average Yearly Extreme Temp Days (2014-2021)",
  #         subtitle = "Extreme temperature defined as >26C or <0C") +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(30, 70))



## S ---------------------------------- 

#summary(data_shp$pop65rate_mean)

ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=S_over65_rank), color=NA) +
  scale_fill_gradient2(low = "#ADBB96", high = "#253012",
                       mid = "#5d782e", 
                       midpoint = 757,
                       breaks = c(2, 757, 1514),
                       labels = c("low \n (2,571)", "mid \n (19,692)", "high \n (32,485)"),
                       name="Aging Population",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#000000") +
  theme_void() +
  # labs(title = "Average Population over 65 per 100,000 (2014-2021)") +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(30, 70))


## V = E * S -----------------------
#summary(data_shp$E_S_over65)

ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_S_over65_rank), color=NA) +
  scale_fill_gradient2(low = "#01665e", high = "#8c510a",
                       mid = "#f6e8c3", 
                       midpoint = 757,
                       breaks = c(2, 757, 1514),
                       labels = c("low \n (11,185)", "mid \n (2,000,597)", "high \n (5,354,158)"),
                       name="Vulnerability",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  # labs(title = "Vulnerability = E * S",
  #      subtitle = "Extreme temperature, Population > 65") +
  theme(legend.position = c(0.25, 0.83),
        legend.title = element_text(face = 2,
                                    vjust = 1.5)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(30, 70))



# Figure 2 -------------------

## Mapping cold days -----------
ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_cold_rank), color=NA) +
  scale_fill_gradient2(low = "#9CD0F9", high = "#1D5179",
                       mid = "#3aa2f4", 
                       midpoint = 757,
                       breaks = c(1.5, 757, 1514),
                       labels = c("low \n (0.14)", "mid \n (102)", "high \n (344)"),
                       name="Relative Number of Days",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  # labs(title = "Average Yearly Days < 0C (2014-2021)") +
  theme(legend.position = c(0.3, 0.89),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(30, 70))

# Figure 3 ---------------------

## V = ESA -------------

## E * S * A ------------------
ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_S_A_over65_rank), color=NA) +
  scale_fill_gradient2(low = "#01665e", high = "#8c510a",
                       mid = "#f6e8c3", 
                       midpoint = 757,
                       breaks = c(2, 757, 1514),
                       labels = c("low", "mid", "high"),
                       name="Vulnerability",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  labs(title = "Vulnerability = E * S * A",
       subtitle = "Extreme temperature, Population > 65") +
  coord_sf(xlim = c(-7.9, 35), ylim = c(30, 70))

## % Change with A -----------------
## FIX::Since previous rendition of code, now 21 regions have a % change greater than 200%
## remove 2 outlier observations of A with a %change greater than 200%
ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=filter(VESA_all.shp, A_change_percent < 200), 
          aes(fill=A_change_percent), color=NA) +
  scale_fill_gradient2(low = "#3d348b", mid = "#f5f5dc", high = "#f35b04",
                       midpoint = 0,
                       breaks = c(-78, 0, 100),
                       limits = c(-78, 100),
                       labels = c("-80%", "0%", "100%"),
                       #values = c("#3d348b", "#7678ed", "#f9c784", "#f7b801", "#f35b04"), 
                       #breaks = c(-78, -32, 0, 25, 214),
                       name="% Change",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  labs(title = "Vulnerability = E * S * A compared to V = E * S") +
  coord_sf(xlim = c(-7.9, 35), ylim = c(30, 70))


## Rank-rank comparison, A no A ------------------

# create new variables for plotting 
A_rank.shp <- VESA_all.shp %>%
  mutate(V_min = if_else(E_S_A_over65 < E_S_over65, E_S_A_over65, E_S_over65),
         V_max = if_else(E_S_A_over65 > E_S_over65, E_S_A_over65, E_S_over65)) %>%
  mutate(R_min = if_else(E_S_A_over65_rank > E_S_over65_rank, E_S_over65_rank, E_S_A_over65_rank),
         R_max = if_else(E_S_A_over65_rank < E_S_over65_rank, E_S_over65_rank, E_S_A_over65_rank))


# Add dummy variable to create legend
A_rank.shp$dummy <- c(rep("A", length(data_extreme$NUTS_ID) - 1), "B") 

# Absolute change in value
ggplot(data = A_rank.shp) +
  geom_linerange(aes(xmin = V_min, xmax = V_max, y = E_S_over65_rank), color = "grey") +
  geom_point(aes(x = E_S_over65, y = E_S_over65_rank), color = "#01665e", size = 0.3) +
  geom_point(aes(x = E_S_A_over65, y = E_S_over65_rank), color = "#b9375e", size = 0.3) +
  # geom_blank(aes(color = dummy)) +  # Dummy data 
  # scale_color_manual(name = "Legend", 
  #                    values = c("A" = "#b9375e", "B" = "#01665e"),
  #                    labels = c("V = ESA", "V = ES")) +
  labs(title = "Change in Vulnerability with Addition of Adaptive Capacity",
       #subtitle = "V = ES (green); V = ESA (pink)",
       x = "V (in person-days)") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

## FIX:: need to check units of this plot, total number of days but mean prop 65+
# density of V before/after inclusion of A
ggplot(data = A_rank.shp) +
  geom_density(aes(x = E_S_over65), color = "#01665e", lwd = 2) +
  geom_density(aes(x = E_S_A_over65), color = "#b9375e", lwd = 2) +
  labs(x = "V (in person-days)") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())




