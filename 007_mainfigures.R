######################################
## PLOTTING MAPS OF E, S, A, and VESA
## Date Created: Dec. 10th 2024
## Last Modified: Feb 3rd 2025
#####################################

# export path [Update path if necessary to match local environment]-------
export_path <- "Figures/"


# Figure 1 ------------------

## E (extreme) --------------------- 
#summary(data_shp$temp_extreme_mean)

fig1 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_total_rank), color=NA) +
  scale_fill_gradient2(low = "#C599C3", high = "#370035",
                       mid = "#7C1A79", 
                       midpoint = 716,
                       breaks = c(1, 716.5, 1414),
                       labels = c("low \n (2)", "mid \n (766)", "high \n (1756)"),
                       name="Relative Total Number of Days of Exposure",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.25, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))

ggsave("Figures/e_total_map.png", plot = fig1,
       width = 10, height = 6.7, units = "cm")

# for label parentheses 
summary(VESA_all.shp$temp_extreme_total)
summary(VESA_all.shp$E_total_rank)

## S ---------------------------------- 

#summary(data_shp$pop65rate_mean)

fig2 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=S_over65_rank), color=NA) +
  scale_fill_gradient2(low = "#ADBB96", high = "#253012",
                       mid = "#5d782e", 
                       midpoint = 716,
                       breaks = c(1, 716.5, 1429),
                       labels = c("low \n (27)", "mid \n (204)", "high \n (351)"),
                       name="Aging Population",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#000000") +
  theme_void() +
  #theme(legend.position = c(0.25, 0.87),
   #     legend.title = element_text(face = 2,
    #                                vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))

ggsave("Figures/s_map.png", plot = fig2,
       width = 10, height = 6.7, units = "cm")

# for label parentheses 
summary(VESA_all.shp$pop65rate_mean)


## V = E * S -----------------------
#summary(data_shp$E_S_over65)

fig3 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_S_over65_rank), color=NA) +
  scale_fill_gradient2(low = "#01665e", high = "#8c510a",
                       mid = "#f6e8c3", 
                       midpoint = 716,
                       breaks = c(1, 716.5, 1429),
                       labels = c("low \n (0.29)", "mid \n (156)", "high \n (405)"),
                       name="Vulnerability",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  #theme(legend.position = c(0.25, 0.83),
   #     legend.title = element_text(face = 2,
    #                                vjust = 1.5)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))

ggsave("Figures/ves_map.png", plot = fig3,
       width = 10, height = 6.7, units = "cm")

# values for parentheses
summary(VESA_all.shp$E_S_over65)

# Figure 2 -------------------

## Mapping cold days -----------
fig4 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_cold_rank), color=NA) +
  scale_fill_gradient2(low = "#9CD0F9", high = "#1D5179",
                       mid = "#3aa2f4", 
                       midpoint = 716,
                       breaks = c(1, 716.5, 1429),
                       labels = c("low \n (0.13)", "mid \n (99)", "high \n (220)"),
                       name="Relative Number of Days",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  #theme(legend.position = c(0.3, 0.89),
   #     legend.title = element_text(face = 2,
    #                                vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))

ggsave("Figures/e_cold_map.png", plot = fig4,
       width = 10, height = 6.7, units = "cm")

# for label parentheses 
summary(VESA_all.shp$temp_extreme_cold)
summary(VESA_all.shp$E_cold_rank)

## cold vs hot boxplot -------------
temp_comparison <- VESA_all %>%
  select(NUTS_ID, temp_extreme_cold, temp_extreme_hot) %>%
  pivot_longer(cols = c(temp_extreme_cold, temp_extreme_hot)) %>%
  mutate(temperature = if_else(str_detect(name, "hot"), "heat", "cold")) %>%
  mutate(days_per_year = value/7)


  
ggplot(data = temp_comparison) +
  geom_boxplot(aes(x = temperature, y = days_per_year, fill = temperature), color = "black") +
  scale_fill_manual(values = c("cold" = "#9CD0F9", "heat" = "#b04848")) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  theme_bw()


# Figure 3 ---------------------

## V = ESA -------------

## E * S * A ------------------
fig5 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_S_A_over65_rank), color=NA) +
  scale_fill_gradient2(low = "#01665e", high = "#8c510a",
                       mid = "#f6e8c3", 
                       midpoint = 716,
                       breaks = c(1, 716.5, 1429),
                       labels = c("low (0.37)", "mid (137)", "high (428)"),
                       name="Vulnerability",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  #theme(legend.position = c(0.25, 0.87),
   #     legend.title = element_text(face = 2,
    #                                vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))

ggsave("Figures/vesa_map.png", plot = fig5,
       width = 10, height = 6.7, units = "cm")

summary(VESA_all.shp$E_S_A_over65)

## % Change with A -----------------

## remove 2 outlier observations of A with a %change greater than 200%
fig6 <- ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=filter(VESA_all.shp, A_change_percent < 200), 
          aes(fill=A_change_percent), color=NA) +
  scale_fill_gradient2(low = "#3d348b", mid = "#f5f5dc", high = "#f35b04",
                       midpoint = 0,
                       breaks = c(-78, 0, 114),
                       limits = c(-78, 114),
                       labels = c("-80%", "0%", "115%"),
                       #values = c("#3d348b", "#7678ed", "#f9c784", "#f7b801", "#f35b04"), 
                       #breaks = c(-78, -32, 0, 25, 214),
                       name="% Change",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  # labs(title = "Vulnerability = E * S * A compared to V = E * S") +
  #theme(legend.position = c(0.25, 0.87),
   #     legend.title = element_text(face = 2,
    #                                vjust = 1.7)) +
  coord_sf(xlim = c(-7.9, 35), ylim = c(35, 70))

ggsave("Figures/vesachange_map.png", plot = fig6,
       width = 10, height = 6.7, units = "cm")

## Rank-rank comparison, A no A ------------------

# create new variables for plotting 
A_rank.shp <- VESA_all.shp %>%
  mutate(V_min = if_else(E_S_A_over65 < E_S_over65, E_S_A_over65, E_S_over65),
         V_max = if_else(E_S_A_over65 > E_S_over65, E_S_A_over65, E_S_over65)) %>%
  mutate(R_min = if_else(E_S_A_over65_rank > E_S_over65_rank, E_S_over65_rank, E_S_A_over65_rank),
         R_max = if_else(E_S_A_over65_rank < E_S_over65_rank, E_S_over65_rank, E_S_A_over65_rank))


# Add dummy variable to create legend
A_rank.shp$dummy <- c(rep("A", length(A_rank.shp$NUTS_ID) - 1), "B") 

# Absolute change in value
fig7 <- ggplot(data = A_rank.shp) +
  geom_linerange(aes(xmin = V_min, xmax = V_max, y = E_S_over65_rank), color = "grey") +
  geom_point(aes(x = E_S_over65, y = E_S_over65_rank, color="B"), size = 0.3) +
  geom_point(aes(x = E_S_A_over65, y = E_S_over65_rank, color="A"), size = 0.3) +
  geom_blank(aes(color = dummy)) +  # Dummy data
  scale_color_manual(values = c("A" = "#b9375e", "B" = "#01665e"),
                     labels = c("V = ESA", "V = ES")) +
  labs(#title = "Change in Vulnerability with Addition of Adaptive Capacity",
       #subtitle = "V = ES (green); V = ESA (pink)",
       x = "V (in rate of vulnerable person days)") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.8, 0.15),
        legend.title=element_blank()) + 
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("Figures/vesachange_graph.png", plot = fig7,
       width = 10, units = "cm")


# density of V before/after inclusion of A
fig8 <- ggplot(data = A_rank.shp) +
  geom_density(aes(x = E_S_over65), color = "#01665e", lwd = 2) +
  geom_density(aes(x = E_S_A_over65), color = "#b9375e", lwd = 2) +
  labs(x = "V (in rate of vulnerable person days)") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

ggsave("Figures/vesacompare_graph.png", plot = fig8,
       width = 10, units = "cm")


