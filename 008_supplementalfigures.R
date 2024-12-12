######################################
## SUPPLEMENTAL FIGURES
## Date Created: Dec. 10th 2024
## Last Modified: Dec. 10th 2024
#####################################

# Distribution of heat and cold extreme -----------

## heat ---------
ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=E_heat_rank), color=NA) +
  scale_fill_gradient2(low = "#EA9999", high = "#650000",
                       mid = "#cc0000", 
                       midpoint = 757,
                       breaks = c(77.5, 757, 1514),
                       labels = c("low \n (0)", "mid \n (5)", "high \n (105)"),
                       name="Relative Number of Days",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  theme(legend.position = c(0.35, 0.87),
        legend.title = element_text(face = 2,
                                    vjust = 1.7)) +
  # labs(title = "Average Yearly Days > 26C (2014-2021)") +
  coord_sf(xlim = c(-7.9, 35), ylim = c(30, 70))

## cold --------

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


# Distribution of A (operationalized as scaled HDI) ----------

ggplot() + 
  geom_sf(data=country_eu.shp, fill="#DFD5CC", color="#B6A699") +
  geom_sf(data=VESA_all.shp, aes(fill=A_rank), color=NA) +
  scale_fill_gradient2(low =  "#b9375e", high ="#ffc2d4",
                       mid = "#ff7aa2", 
                       midpoint = 757,
                       breaks = c(2, 757, 1514),
                       labels = c("low", "mid", "high"),
                       name="Adaptive Capacity",
                       na.value="#FFFFFE") +
  geom_sf(data=country_eu_sam.shp, fill=NA, color="#743A28") +
  theme_void() +
  labs(title = "Adaptive Capacity, Operationalized as Scaled HDI") +
  coord_sf(xlim = c(-7.9, 35), ylim = c(30, 70))
