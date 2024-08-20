#### Analysis for SURFO ####
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(ggpmisc)
library(dplyr)
library(lubridate)
# Bring in csv's from analyses in matlab
setwd("~/SURFO/")
bottom_co2sys=read.csv("Wickford_Oyster_Farm_Data_Bottom_CO2SYS.csv")
surface_co2sys=read.csv("surface_co2sys.csv")
# Plot TA vs Salinity at Bottom #
ggplot(data = bottom_co2sys, aes(x = IS_S, y = TA_umol_kg, fill = IS_T)) +
  geom_point(size = 3, shape = 21, color = "black", position="jitter") +
  scale_fill_gradientn(colours = heat.colors(10)) +
  labs(fill = "IS_T") +
  geom_smooth(method=lm, color="black", linetype="dashed")+
  theme_classic() +
  labs(title= "Bottom Salinity vs TA")+
  theme(legend.position = "right")
# plot DIC vs Salinity at Bototm #
ggplot(data = bottom_co2sys, aes(x = IS_S, y = DIC_umol_kg, fill = IS_T)) +
  geom_point(size = 3, shape = 21, color = "black", position="jitter") +
  scale_fill_gradientn(colours = heat.colors(10)) +
  labs(fill = "IS_T") +
  theme_classic() +
  labs(title= "Bottom Salinity vs DIC")+
  theme(legend.position = "right")
# Plot TA vs Salinity at Surface #
ggplot(data = surface_co2sys, aes(x = IS_S, y = TA_umol_kg, fill = IS_T)) +
  geom_point(size = 3, shape = 21, color = "black", position="jitter") +
  scale_fill_gradientn(colours = heat.colors(10)) +
  labs(fill = "IS_T") +
  geom_smooth(method=lm, color="black", linetype="dashed")+
  stat_poly_eq(aes(label = paste( ..rr.label..)),
               formula = y ~ x,
               parse = TRUE) +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(title= "Surface Salinity vs TA")+
  theme(legend.position = "right")
# plot DIC vs Salinity at Surface #
ggplot(data = surface_co2sys, aes(x = M_S, y = DIC_umol_kg, fill = IS_T)) +
  geom_point(size = 3, shape = 21, color = "black", position="jitter") +
  scale_fill_gradientn(colours = heat.colors(10)) +
  labs(fill = "IS_T") +
  geom_smooth(method=lm, color="black", linetype="dashed")+
  stat_poly_eq(aes(label = paste( ..rr.label..)),
               formula = y ~ x,
               parse = TRUE) +
  theme_classic() +
  labs(title= "Surface Salinity vs DIC")+
  theme(legend.position = "right")
#plot DIC vs TA at Bottom
ggplot(data = bottom_co2sys, aes(x = TA_umol_kg, y = DIC_umol_kg, fill = IS_T)) +
  geom_point(size = 3, shape = 21, color = "black", position="jitter") +
  scale_fill_gradientn(colours = heat.colors(10)) +
  labs(fill = "IS_T") +
  theme_classic() +
  labs(title= "Bottom TA vs DIC")+
  theme(legend.position = "right")
#plot DIC vs TA at Surface
ggplot(data = surface_co2sys, aes(x = TA_umol_kg, y = DIC_umol_kg, fill = IS_T)) +
  geom_point(size = 3, shape = 21, color = "black", position="jitter") +
  scale_fill_gradientn(colours = heat.colors(10)) +
  labs(fill = "IS_T") +
  theme_classic() +
  labs(title= "Surface TA vs DIC")+
  theme(legend.position = "right")
#### Pimenta Relationship ####

# Fit models
model_TA <- lm(TA ~ Salinity_ppt, data = Pimenta)
model_DIC <- lm(DIC ~ Salinity_ppt, data = Pimenta)
Coefficients_TA=coef(model_TA)
Coefficients_DIC=coef(model_DIC)
# Predict TA and DIC based on Measured Salinity
surface_co2sys$TA_predicted = Coefficients_TA[2]*surface_co2sys$IS_S+Coefficients_TA[1]
surface_co2sys$DIC_predicted = Coefficients_DIC[2]*surface_co2sys$IS_S+Coefficients_DIC[1]
bottom_co2sys$TA_predicted = Coefficients_TA[2]*bottom_co2sys$IS_S+Coefficients_TA[1]
bottom_co2sys$DIC_predicted = Coefficients_DIC[2]*bottom_co2sys$IS_S+Coefficients_DIC[1]

# Calculate DeltaTA and DeltaDIC
surface_co2sys$DeltaTA=(surface_co2sys$TA_umol_kg - surface_co2sys$TA_predicted)
surface_co2sys$DeltaDIC=(surface_co2sys$DIC_umol_kg - surface_co2sys$DIC_predicted)
bottom_co2sys$DeltaTA=(bottom_co2sys$TA_umol_kg - bottom_co2sys$TA_predicted)
bottom_co2sys$DeltaDIC=(bottom_co2sys$DIC_umol_kg - bottom_co2sys$DIC_predicted)


# Step 2: Create the main plot
library(RColorBrewer)
# Create the main plot
main_plot_surface <- ggplot(surface_co2sys, aes(x = DeltaDIC, y = DeltaTA, fill = IS_T)) +
  geom_point(data = subset(surface_co2sys), shape=21, color="black", position="jitter", size=3) +
  scale_fill_gradientn(colors=rev(brewer.pal(10, "RdBu")))+ 
  labs(y = expression("TA anomaly (µmol kg"^-1*")"),
       x = expression("DIC anomaly (µmol kg"^-1*")"),
       color = "Temperature") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_abline(slope = -17/106, intercept = 0, linetype = "dashed", color = "black") + 
  geom_abline(slope = 2, intercept = 0, linetype = "dashed", color = "black") + 
  xlim(-200, 200) +
  ylim(-200, 200) +
  theme_minimal(base_size = 10) + 
  labs(title= "Surface Anomolies")+
  
  theme(
    #panel.background = element_rect(fill = "white", color = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
print(main_plot_surface)
# Bottom
main_plot_bottom <- ggplot(bottom_co2sys, aes(x = DeltaDIC, y = DeltaTA, fill = IS_T)) +
  geom_point(data = subset(bottom_co2sys), shape=21, color="black", position="jitter", size=3) +
  scale_fill_gradientn(colors=rev(brewer.pal(10, "RdBu")))+ 
  labs(y = expression("TA anomaly (µmol kg"^-1*")"),
       x = expression("DIC anomaly (µmol kg"^-1*")"),
       colour = "Temperature") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_abline(slope = -17/106, intercept = 0, linetype = "dashed", color = "black") + 
  geom_abline(slope = 2, intercept = 0, linetype = "dashed", color = "black") + 
  xlim(-200, 200) +
  ylim(-200, 200) +
  theme_minimal(base_size = 10) +
  labs(title= "Bottom Anomolies",)+
  theme(
    #panel.background = element_rect(fill = "white", color = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
print(main_plot_bottom)

#### TA Carbon Emissions Quantification ####
surface_co2sys=read.csv("surface_co2sys.csv")
surface_co2sys=surface_co2sys %>%
  mutate(Date = dmy(Date))

# Extract the month from the DateTime column and add it as a new column called Month
surface_co2sys=surface_co2sys %>%
  mutate(Month = month(Date, label = TRUE))

# Make a new variable called DeltaTAjune which is the average DeltaTA calculated by Pimenta method 
# for only the month of June (a warm month)
surface_co2sys_june=subset(surface_co2sys,Month=='Jun')

surface_co2sys_june_mean_TApredicted=mean(surface_co2sys_june$TA_predicted, na.rm=T)
surface_co2sys_june_mean_DICpredicted=mean(surface_co2sys_june$DIC_predicted, na.rm=T)
surface_co2sys_june_mean_TAcalc=mean(surface_co2sys_june$TA_umol_kg, na.rm=T)
surface_co2sys_june_mean_DICcalc=mean(surface_co2sys_june$DIC_umol_kg, na.rm=T)


surface_co2sys_june_mean_T=mean(surface_co2sys_june$IS_T, na.rm=T)
surface_co2sys_june_mean_S=mean(surface_co2sys_june$IS_S, na.rm=T)


DeltaTAjune=surface_co2sys %>%
  filter(Month == "Jun") %>%
  summarise(mean_DeltaTA = mean(DeltaTA, na.rm = T)) %>%
  pull(mean_DeltaTA)
print(DeltaTAjune)
# DIC June 
DeltaDICjune=surface_co2sys %>%
  filter(Month == "Jun") %>%
  summarise(mean_DeltaDIC = mean(DeltaDIC, na.rm = T)) %>%
  pull(mean_DeltaDIC)
print(DeltaDICjune)
#
TAcaljune=DeltaTAjune -(DeltaTAjune-2*DeltaDICjune)/(1+2*107/16)
# Now we use DeltaTAjune to create a variable called IntegratedTAjune 
# with assumptions that there is a 1 week flush cycle in the oyster farm that is 4.19 acres
# and has a mean depth of 6 meters, therefore a volume of 103365.5 m^3. 
# Calculate Oyster Farm Volume
acre_to_sq_meters = 4046.86 # 1 Acre is 4046.86 meters
farm_area_acres = 4.19 
mean_depth_meters = 6
# From Pilson Paper- 10-40 day flushing period in NBay - Change to make the flushing factor different.
# Here I use 1 as the flushing factor, to 
flushing_factor1=1
# Weekly Flush
flushing_factor4=4
# Calculate the area in square meters
farm_area_sq_meters = farm_area_acres * acre_to_sq_meters
# Calculate the volume
Volume_OF = farm_area_sq_meters * mean_depth_meters
# Calculate the Integrated TA for June, Multiply by 4 because flush removes TA
IntegratedTAjune=(TAcaljune*Volume_OF)*flushing_factor1
# Calculate the Total CO2 Emissions from TA approach, assuming every 2 mol of TA removed relases 1 mol of CO2
CO2_Emissions_TAjune=1/2*IntegratedTAjune*0.6 #0.6 is caused by buffer capacity change because of cal
print(CO2_Emissions_TAjune)
# CO2 Emissions during growing season, in Moles CO2
CO2_Emissions_TAgrowing=4*CO2_Emissions_TAjune
# Convert moles into g of CO2 emitted, 44.01 g/mol
CO2e_TAjune_g=-CO2_Emissions_TAgrowing*44.01
CO2e_TAjune_g*374/4
10^-6*21050757313
# Conversion into metric tons is ~ 225 metric tons per growing season
## Bottom same thing ##
## Bottom 
bottom_co2sys=bottom_co2sys %>%
  mutate(Date = dmy(Date))

# Extract the month from the DateTime column and add it as a new column called Month
bottom_co2sys=bottom_co2sys %>%
  mutate(Month = month(Date, label = TRUE))

# Make a new variable called DeltaTAjune which is the average DeltaTA calculated by Pimenta method 
# for only the month of June (a warm month)
DeltaTAjuneb=bottom_co2sys %>%
  filter(Month == "Jun") %>%
  summarise(mean_DeltaTA = mean(DeltaTA, na.rm = TRUE)) %>%
  pull(mean_DeltaTA)
print(DeltaTAjuneb)
# DIC June 
DeltaDICjuneb=bottom_co2sys %>%
  filter(Month == "Jun") %>%
  summarise(mean_DeltaDIC = mean(DeltaDIC, na.rm = TRUE)) %>%
  pull(mean_DeltaDIC)
print(DeltaDICjuneb)
#
TAcaljuneb=DeltaTAjuneb -(DeltaTAjuneb-2*DeltaDICjuneb)/(1+2*107/16)
# Now we use DeltaTAjune to create a variable called IntegratedTAjune 
# with assumptions that there is a 1 week flush cycle in the oyster farm that is 4.19 acres
# and has a mean depth of 6 meters, therefore a volume of 103365.5 m^3. 
# Calculate Oyster Farm Volume
acre_to_sq_meters = 4046.86 # 1 Acre is 4046.86 meters
farm_area_acres = 4.19 
mean_depth_meters = 6
# From Pilson Paper- 10-40 day flushing period in NBay - Change to make the flushing factor different.
# Here I use 1 as the flushing factor, to 
flushing_factor1=1
# Weekly Flush
flushing_factor4=4
# Calculate the area in square meters
farm_area_sq_meters = farm_area_acres * acre_to_sq_meters
# Calculate the volume
Volume_OF = farm_area_sq_meters * mean_depth_meters
# Calculate the Integrated TA for June, Multiply by 4 because flush removes TA
IntegratedTAjuneb=(TAcaljuneb*Volume_OF)*flushing_factor1
# Calculate the Total CO2 Emissions from TA approach, assuming every 2 mol of TA removed relases 1 mol of CO2
CO2_Emissions_TAjuneb=1/2*IntegratedTAjuneb*0.6 #0.6 is caused by buffer capacity change because of cal
print(CO2_Emissions_TAjuneb)
# CO2 Emissions during growing season, in Moles CO2
CO2_Emissions_TAgrowingb=4*CO2_Emissions_TAjuneb
# Convert moles into g of CO2 emitted, 44.01 g/mol
CO2e_TAjune_g_b=-CO2_Emissions_TAgrowingb*44.01
#### CO2 Emission Oyster Harvest Approach ####
# Wickford tells us they harvest 1 million oysters per season
Oysters_Harvested=1000000
# Oyster shell makes up for 80% of biomass
Oyster_Shell_Pct=.80
# Oyster average mass is 50g
Oyster_Avg_Mass_g=50
# Shell is 95% calcium carbonate
Shell_Pct_CaCO3=.95
# Shell Mass Harvested Calculation
Shell_Mass_Harvested=Oysters_Harvested*(Oyster_Avg_Mass_g*Oyster_Shell_Pct)
# Carbonate Mass Harvested (g)
CaCO3_Mass_Harvested=Shell_Pct_CaCO3*Shell_Mass_Harvested
# Convert into Moles Carbonate - CaCO3 = 100g/mol
CaCO3_Moles_Harvested=CaCO3_Mass_Harvested/100
# Calcuate CO2 Emission from Oysters, 1 CaCO3 relases 1 CO2, CO2 = 44.01 g/mol
CO2_mass_released=CaCO3_Moles_Harvested* 44.01 *0.6

CO2e_differencejunes=CO2_mass_released/CO2e_TAjune_g
CO2e_differencejuneb=CO2_mass_released/CO2e_TAjune_g_b

mean_t_june_s=mean(surface_co2sys$IS_T[surface_co2sys$Month == "Jun"], na.rm = TRUE)
mean_TAan_june_s=mean(surface_co2sys$DeltaTA[surface_co2sys$Month == "Jun"], na.rm = TRUE)

#### CO2 Flux ####
mean_pCO2predicted_june=surface_co2sys %>%
  filter(Month == "Jun") %>%
  summarise(mean_pCO2predicted_june = mean(pCO2predicted, na.rm = TRUE),sd_pCO2predicted_june = sd(pCO2predicted, na.rm = TRUE))
print(mean_pCO2predicted_june)

mean_pCO2calc_june=surface_co2sys %>%
  filter(Month == "Jun") %>%
  summarise(mean_pCO2calc_june = mean(pCO2calc, na.rm = TRUE),sd_pCO2calc_june = sd(pCO2calc, na.rm = TRUE))
print(mean_pCO2calc_june)

ggplot(data = surface_co2sys, aes(x = Date)) +
  geom_point(aes(y = pCO2predicted, color = Month)) +
  geom_point(aes(y = pCO2calc, color = Month)) + # Different shape for differentiation
  labs(title = "pCO2 Predicted and Calculated over Time",
       x = "Date",
       y = "pCO2") +
  theme_minimal() +
  scale_color_discrete(name = "Month") +
  theme(legend.position = "right")



print(surface_co2sys$pCO2predicted)
print(surface_co2sys$pCO2calc)


write.csv(surface_co2sys,"~/SURFO/surface_co2sys.csv", row.names = TRUE)
#### From Hongjie, Pimenta Relationship ####

load("~/SURFO/PimentaEtAl2023.Rdata")
Pimenta=PimentaEtAl2023_carb
# Get the column names of your data frame (assuming df is your data frame)
(column_names <- colnames(Pimenta))
#Pimenta=Pimenta[,c(-2,-4,-9)]
# Create a vector of new column names (replace with your desired names)
new_column_names <- c("site", "datetime", "mydates",'Depth.m','depth','Salinity_ppt','Temp_C','DO_mg.L','DOpct','pH','DIC','TA')  # Add all new names
colnames(Pimenta) <- new_column_names
Pimenta$mydates=(as.Date(Pimenta$mydates))

# Assuming Pimenta is your dataframe and it has the necessary columns

# Step 1: Calculate DeltaTA and DeltaDIC
# Assuming TA and DIC are your original variables and Salinity_ppt is a predictor variable

# Fit regression models
model_TA <- lm(TA ~ Salinity_ppt, data = Pimenta)
model_DIC <- lm(DIC ~ Salinity_ppt, data = Pimenta)

# Predict TA and DIC based on Salinity_ppt
Pimenta$TA_predicted <- predict(model_TA, Pimenta)
Pimenta$DIC_predicted <- predict(model_DIC, Pimenta)

# Calculate DeltaTA and DeltaDIC
Pimenta$DeltaTA <- Pimenta$TA - Pimenta$TA_predicted
Pimenta$DeltaDIC <- Pimenta$DIC - Pimenta$DIC_predicted

# Update the Pimenta dataframe
Pimenta <- Pimenta %>%
  mutate(month_group = case_when(
    month(mydates) %in% c(12) ~ "Dec",
    month(mydates) %in% c(8) ~ "Aug",
    TRUE ~ "Other"
  ))



# Step 2: Create the main plot
# Create the main plot
main_plot <- ggplot(surface_co2sys, aes(x = DeltaDIC, y = DeltaTA, colour = month_group)) +
  geom_point(data = subset(surface_co2sys, month_group == "Other")) +
  geom_point(data = subset(surface_co2sys, month_group == "Dec")) +
  geom_point(data = subset(surface_co2sys, month_group == "Aug")) +
  scale_colour_manual(values = c("Other" = "grey", "Dec" = "blue", "Aug" = "red")) +
  labs(y = expression("TA anomaly (µmol kg"^-1*")"),
       x = expression("DIC anomaly (µmol kg"^-1*")"),
       colour = "Months") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_abline(slope = -17/106, intercept = 0, linetype = "dashed", color = "black") + 
  geom_abline(slope = 2, intercept = 0, linetype = "dashed", color = "black") + 
  xlim(-200, 200) +
  ylim(-200, 200) +
  theme_minimal(base_size = 10) + 
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )
  theme(legend.position.inside = c(0.75, 0.25))

# Print and save the plot
print(main_plot)
ggsave("DeltaTA_vs_DeltaDIC.png", plot = main_plot, width = 10, height = 10, units = "cm", dpi = 300)


# Conversion of CO2 SYS code to R ####