# 06/04/2024 SURFO 2024, Oyster Environmental Parameters Analysis #
#
# Written By: Ilana Jacobs #
#
# Packages used:
#ggplot2
#ggpmisc (ggplot extention)
#ggridges (ggplot extension)
#
#### Open Packages and setup WD for the Session ####
#set working directory for the entire session
setwd("/Users/ilanajacobs/SURFO/OysterFarm0617")
#
#
#Open any packages needed 
library("ggplot2")
library("ggpmisc")
library("tidyverse")
library("dplyr")
#### Bottom Buoy Upload ####
#
#open csv with data, but name the function first
bottom=read.csv("oysters_bottom_061824.csv")
#look at the structure of the file
str(bottom)
#
#
# creates a datetime column using data from date and time column.
bottom1 <- bottom %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
#
#
#### Salinity (bottom) ####
#mean
sal_mean_bottom=mean(bottom$Salinity_ppt, na.rm=TRUE)

#median
sal_median_bottom=median(bottom$Salinity_ppt, na.rm=TRUE)

#standard deviation
sal_sd_bottom=sd(bottom$Salinity_ppt, na.rm=TRUE)

#range
sal_range_bottom=range(bottom$Salinity_ppt, na.rm=TRUE)


#### Chl-a (bottom) ####

#mean
chl_mean_bottom=mean(bottom$Chla_rfu, na.rm=TRUE)

#median
chl_median_bottom=median(bottom$Chla_rfu, na.rm=TRUE)

#standard deviation
chl_sd_bottom=sd(bottom$Chla_rfu, na.rm=TRUE)

#range
chl_range_bottom=range(bottom$Chla_rfu, na.rm=TRUE)
#### DO (bottom) ####

#mean
do_mean_bottom=mean(bottom$DO_mgl, na.rm=TRUE)

#median
do_median_bottom=median(bottom$DO_mgl, na.rm=TRUE)

#standard deviation
do_sd_bottom=sd(bottom$DO_mgl, na.rm=TRUE)

#range
do_range_bottom=range(bottom$DO_mgl, na.rm=TRUE)

#### pH (bottom) ####

#mean
ph_mean_bottom=mean(bottom$pH, na.rm=TRUE)

#median
ph_median_bottom=median(bottom$pH, na.rm=TRUE)

#standard deviation
ph_sd_bottom=sd(bottom$pH, na.rm=TRUE)

#range
ph_range_bottom=range(bottom$pH, na.rm=TRUE)

#### Temp (bottom) ####

#mean
temp_mean_bottom=mean(bottom$Temperature_C, na.rm=TRUE)

#median
temp_median_bottom=median(bottom$Temperature_C, na.rm=TRUE)

#standard deviation
temp_sd_bottom=sd(bottom$Temperature_C, na.rm=TRUE)

#range
temp_range_bottom=range(bottom$Temperature_C, na.rm=TRUE)
#
#
#
#### Surface Buoy ####
#open csv with data, but name the function first
surface=read.csv("oysters_surface_061824.csv")
#look at the structure of the file
str(surface)
#
# creates a datetime column using data from date and time column.
surface1 <- surface %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
#
#
#### Salinity (surface) ####
#mean
sal_mean_surface=mean(surface$Salinity_ppt)

#median
sal_median_surface=median(surface$Salinity_ppt)

#standard deviation
sal_sd_surface=sd(surface$Salinity_ppt)

#range
sal_range_surface=range(surface$Salinity_ppt)


#### Chl-a (surface) ####

#mean
chl_mean_surface=mean(surface$Chla_rfu)

#median
chl_median_surface=median(surface$Chla_rfu)

#standard deviation
chl_sd_surface=sd(surface$Chla_rfu)

#range
chl_range_surface=range(surface$Chla_rfu)
#### DO (surface) ####

#mean
do_mean_surface=mean(surface$DO_mgl)

#median
do_median_surface=median(surface$DO_mgl)

#standard deviation
do_sd_surface=sd(surface$DO_mgl)

#range
do_range_surface=range(surface$DO_mgl)

#### pH (surface) ####

#mean
ph_mean_surface=mean(surface$pH)

#median
ph_median_surface=median(surface$pH)

#standard deviation
ph_sd_surface=sd(surface$pH)

#range
ph_range_surface=range(surface$pH)

#### Temp (surface) ####

#mean
temp_mean_surface=mean(surface$Temperature_C)

#median
temp_median_surface=median(surface$Temperature_C)

#standard deviation
temp_sd_surface=sd(surface$Temperature_C)

#range
temp_range_surface=range(surface$Temperature_C)

####  bottom plots ####
ggplot(data=bottom1, aes(x = datetime , y = Salinity_ppt, color=Salinity_ppt)) +
  geom_line() +
  scale_color_gradient2(low="navyblue", mid="lightpink", high="darkorange", midpoint=unique(sal_median_bottom))+
  labs(title = "Bottom Salinity in May 2024", x = "Date", y = "Salinity (PPT)") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
#
ggplot(data=bottom1, aes(x = datetime , y = Depth_m, color)) +
  geom_line() +
  ylim(2,8) +
  labs(title = "Bottom Depth in May 2024", x = "Date", y = "Depth (Meters)") +
  #geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
#
ggplot(data=bottom1, aes(x = datetime , y = DO_mgl, color=DO_mgl)) +
  geom_line() +
  scale_color_gradient2(low="yellow", mid="yellowgreen", high="cyan", midpoint=unique(do_median_bottom))+
  labs(title = "Bottom Dissolved Oxygen in May 2024", x = "Date", y = "Dissolved Oxygen (mg/L)") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
#
ggplot(data=bottom1, aes(x = datetime , y = Chla_rfu, color=Chla_rfu)) +
  geom_line()+
  scale_color_gradient2(low="wheat3", mid="darkkhaki", high="forestgreen", midpoint=unique(chl_median_bottom))+
  labs(title = "Bottom Chlorophyll in May 2024", x = "Date", y = "Chlorophyll (RFU)") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
#
ggplot(data=bottom1, aes(x = datetime , y = pH, color=pH)) +
  geom_line() +
  scale_color_gradient2(low="red", mid="green", high="purple", midpoint=unique(ph_median_bottom))+
  labs(title = "Bottom pH in May 2024", x = "Date", y = "pH") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
#
ggplot(data=bottom1, aes(x = datetime , y = Temperature_C, color=Temperature_C)) +
  geom_line() +
  scale_color_gradient2(low="blue2", mid="yellow", high="red2", midpoint=unique(temp_median_bottom))+
  labs(title = "Bottom Temperature in May 2024", x = "Date", y = "Temperature (ºC)") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
# 
#### SURFACE plots ####
#
ggplot(data=surface1, aes(x = datetime , y = Salinity_ppt, color=Salinity_ppt)) +
  geom_line() +
  scale_color_gradient2(low="navyblue", mid="lightpink", high="darkorange", midpoint=unique(sal_median_surface))+
  labs(title = "Surface Salinity in May 2024", x = "Date", y = "Salinity (PPT)") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
#
ggplot(data=surface1, aes(x = datetime , y = Depth_m)) +
  geom_line() +
  ylim(0,3) +
  labs(title = "Surface Depth in May 2024", x = "Date", y = "Depth (Meters)") +
  #geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
# DO
ggplot(data=surface1, aes(x = datetime , y = DO_mgl, color=DO_mgl)) +
  geom_line() +
  scale_color_gradient2(low="yellow", mid="yellowgreen", high="cyan", midpoint=unique(do_median_surface))+
  labs(title = "Surface Dissolved Oxygen in May 2024", x = "Date", y = "Dissolved Oxygen (mg/L)") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
# Chl-a
ggplot(data=surface1, aes(x = datetime , y = Chla_rfu, color=Chla_rfu)) +
  geom_line(aes(color=Chla_rfu)) +
  scale_color_gradient2(low="wheat3", mid="darkkhaki", high="forestgreen", midpoint=unique(chl_median_surface))+
  labs(title = "Surface Chlorophyll in May 2024", x = "Date", y = "Chlorophyll (RFU)") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
#
ggplot(data=surface1, aes(x = datetime , y = pH, color=pH)) +
  geom_line(aes(color=pH)) +
  scale_color_gradient2(low="red", mid="green", high="purple", midpoint=unique(ph_median_surface))+
  labs(title = "Surface pH in May 2024", x = "Date", y = "pH") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()
#
ggplot(data = surface1, aes(x = datetime, y = Temperature_C)) +
  geom_line(aes(color = Temperature_C))+
  scale_color_gradient2(low="blue2", mid="yellow", high="red2", midpoint=unique(temp_median_surface))+
  labs(title = "Surface Temperature in May 2024", x = "Date", y = "Temperature (ºC)") +
  geom_smooth(method = "lm", color = "steelblue") + 
  theme_bw()

#### Plots Both at same time ####
#open CSV
both=read.csv("oysters_both_061824.csv")
#add column for combined datetime
both1 <- both %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
#### Stacked Plots for Surface and Bottom ####
#
# Calculate the median temperature for each type

library(ggplot2)
library(dplyr)
library(gridExtra)

# Calculate the median temperature for each site
medians_temp <- both1 %>%
  group_by(site) %>%
  summarize(median_temp = median(Temperature_C, na.rm = TRUE))

# Add the median temperature to the original data
both1 <- both1 %>%
  left_join(medians_temp, by = "site")

# Function to create a plot for each site with respective median values
create_plot_temp <- function(data, site_label) {
  ggplot(data, aes(x = datetime, y = Temperature_C, color = Temperature_C)) +
    geom_line() +
    scale_color_gradient2(midpoint = unique(data$median_temp), low = "blue2", mid = "yellow", high = "red2", name="Temperature") +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = paste("Temperature in May 2024 -", site_label),x = "Date", y = "Temperature (ºC)") +
    theme_bw()
}

# Create the plots for each site
surface_plot_temp <- create_plot_temp(both1 %>% filter(site == "surface"), "Surface")
bottom_plot_temp <- create_plot_temp(both1 %>% filter(site == "bottom"), "Bottom")

# Combine the plots
grid.arrange(surface_plot_temp, bottom_plot_temp, ncol = 1)

# Salinity #

# Calculate the median temperature for each site
medians_sal = both1 %>%
  group_by(site) %>%
  summarize(median_sal = median(Salinity_ppt, na.rm = TRUE))

# Add the median temperature to the original data
both1 <- both1 %>%
  left_join(medians_sal, by = "site")

# Function to create a plot for each site with respective median values
create_plot_sal = function(data, site_label) {
  ggplot(data, aes(x = datetime, y = Salinity_ppt, color = Salinity_ppt)) +
    geom_line() +
    scale_color_gradient2(midpoint = unique(data$median_sal), low = "navyblue", mid = "lightpink", high = "darkorange", name="Salinity") +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = paste("Salinity in May 2024 -", site_label),x = "Date", y = "Salinity (ppt)") +
    theme_bw()
}

# Create the plots for each site
surface_plot_sal = create_plot_sal(both1 %>% filter(site == "surface"), "Surface")
bottom_plot_sal = create_plot_sal(both1 %>% filter(site == "bottom"), "Bottom")

# Combine the plots
grid.arrange(surface_plot_sal, bottom_plot_sal, ncol = 1)

# Dissolved Oxygen #
# Calculate the median temperature for each site
medians_do = both1 %>%
  group_by(site) %>%
  summarize(median_do = median(DO_mgl, na.rm = TRUE))

# Add the median temperature to the original data
both1 = both1 %>%
  left_join(medians_do, by = "site")

# Function to create a plot for each site with respective median values
create_plot_do = function(data, site_label) {
  ggplot(data, aes(x = datetime, y = DO_mgl, color = DO_mgl)) +
    geom_line() +
    scale_color_gradient2(midpoint = unique(data$median_do), low = "yellow", mid = "yellowgreen", high = "cyan", name="Dissolved Oxygen") +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = paste("Dissolved Oxygen in May 2024 -", site_label),x = "Date", y = "Dissolved Oxygen (mg/L)") +
    theme_bw()
}

# Create the plots for each site
surface_plot_do = create_plot_do(both1 %>% filter(site == "surface"), "Surface")
bottom_plot_do = create_plot_do(both1 %>% filter(site == "bottom"), "Bottom")

# Combine the plots
grid.arrange(surface_plot_do, bottom_plot_do, ncol = 1)

# Chlorophyll #

# Calculate the median temperature for each site
medians_chl = both1 %>%
  group_by(site) %>%
  summarize(median_chl = median(Chla_rfu, na.rm = TRUE))

# Add the median temperature to the original data
both1 = both1 %>%
  left_join(medians_chl, by = "site")

# Function to create a plot for each site with respective median values
create_plot_chl = function(data, site_label) {
  ggplot(data, aes(x = datetime, y = Chla_rfu, color = Chla_rfu)) +
    geom_line() +
    scale_color_gradient2(midpoint = unique(data$median_chl), low = "wheat3", mid = "darkkhaki", high = "forestgreen", name="Chlorophyll") +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = paste("Chlorophyll in May 2024 -", site_label),x = "Date", y = "Chlorophyll", color="Chlorophyll") +
    theme_bw()
}

# Create the combined plot
surface_plot_chl = create_plot_chl(both1 %>% filter(site == "surface"), "Surface")
bottom_plot_chl = create_plot_chl(both1 %>% filter(site == "bottom"), "Bottom")

# Combine the plots
grid.arrange(surface_plot_chl, bottom_plot_chl, ncol = 1)


# pH #
# Calculate the median temperature for each site
medians_pH = both1 %>%
  group_by(site) %>%
  summarize(median_pH = median(pH, na.rm = TRUE))

# Add the median temperature to the original data
both1 = both1 %>%
  left_join(medians_pH, by = "site")

# Function to create a plot for each site with respective median values
create_plot_pH = function(data, site_label) {
  ggplot(data, aes(x = datetime, y = pH, color = pH)) +
    geom_line() +
    scale_color_gradient2(midpoint = unique(data$median_pH), low = "red", mid = "green", high = "purple", name="pH") +
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = paste("pH in May 2024 -", site_label),x = "Date", y = "pH") +
    theme_bw()
}

# Create the plots for each site
surface_plot_pH = create_plot_pH(both1 %>% filter(site == "surface"), "Surface")
bottom_plot_pH = create_plot_pH(both1 %>% filter(site == "bottom"), "Bottom")

# Combine the plots
grid.arrange(surface_plot_pH, bottom_plot_pH, ncol = 1)

#### Both Plots on one axis ####
# Salinity #
ggplot(data=both1, aes(x = datetime, y = Salinity_ppt, color = site)) +
  geom_line() +
  scale_color_manual(values=c("darkgreen", "steelblue"))+
  labs(title = paste("Salinity in May 2024"),x = "Date", y = "Salinity") +
  theme_bw()
# Depth #
ggplot(data=both1, aes(x = datetime, y = Depth_m, color = site)) +
  geom_line() +
  scale_y_reverse()+
  scale_color_manual(values=c("darkgreen", "steelblue"))+
  labs(title = paste("Depth in May 2024"),x = "Date", y = "Depth (m)") +
  theme_bw()
# pH #
ggplot(data=both1, aes(x = datetime, y = pH, color = site)) +
  geom_line() +
  scale_color_manual(values=c("darkgreen", "steelblue"))+
  labs(title = paste("pH in May 2024"),x = "Date", y = "pH") +
  theme_bw()
# Temperature 
ggplot(data=both1, aes(x = datetime, y = Temperature_C, color = site)) +
  geom_line()+
  scale_color_manual(values=c("darkgreen", "steelblue"))+
  labs(title = paste("Temperature in May 2024"),x = "Date", y = "Temperature (ºC)") +
  theme_bw()
# DO #
ggplot(data=both1, aes(x = datetime, y = DO_mgl, color = site)) +
  geom_line() +
  scale_color_manual(values=c("darkgreen", "steelblue"))+
  labs(title = paste("Dissolved Oxygen in May 2024"),x = "Date", y = "Dissolved Oxygen (mg/L)") +
  theme_bw()
# Chl #
ggplot(data=both1, aes(x = datetime, y = Chla_rfu, color = site)) +
  geom_line() +
  scale_color_manual(values=c("darkgreen", "steelblue"))+
  labs(title = paste("Chlorophyll in May 2024"),x = "Date", y = "Chlorophyll") +
  theme_bw()
#
#
#
#
#### July Oyster Report ####
#
#
# we had barnacle growth on the sensor, so here I work with that data to find "expected DO" #
setwd("/Users/ilanajacobs/SURFO/OysterFarm0617")
  barnacles=read.csv("oysters_surface_barnacles.csv")
  cleaned=read.csv("Oysters_surface_cleaned.csv")
  
  ggplot(data=barnacles, aes(x=DO_mgl, y=pH))+
    geom_point(position="jitter", size=0.5)+
    stat_poly_eq()+
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = paste("DO and pH correlation" ),x = "Dissolved Oxygen (mg/L)", y = "pH") +
    theme_bw()
  ggplot(data=surface, aes(x=DO_mgl, y=pH))+
    geom_point(position="jitter", size=0.5)+
    stat_poly_eq()+
    geom_smooth(method = "lm", color = "steelblue") +
    labs(title = paste("DO and pH correlation" ),x = "Dissolved Oxygen (mg/L)", y = "pH") +
    theme_bw()
  ###
  library(ggplot2)
library(ggpmisc)  # For stat_poly_eq

ggplot() +
  # Add barnacles data
  geom_point(data = barnacles, aes(x = DO_mgl, y = pH), position = "jitter", size = 0.5, color = "red") +
  geom_smooth(data = barnacles, aes(x = DO_mgl, y = pH), method = "lm", color = "red", se = FALSE) +
  stat_poly_eq(data = barnacles, aes(x = DO_mgl, y = pH, label = ..rr.label.., sep = "~~~"), 
               formula = y ~ x, parse = TRUE, color = "red") +
  # Add surface data before 
  geom_point(data = surface, aes(x = DO_mgl, y = pH), position = "jitter", size = 0.5, color = "blue") +
  geom_smooth(data = surface, aes(x = DO_mgl, y = pH), method = "lm", color = "blue", se = FALSE) +
  stat_poly_eq(data = surface, aes(x = DO_mgl, y = pH, label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, parse = TRUE, color = "blue", label.x = 5, label.y = 8) +
  # add surface data after
  geom_point(data = cleaned, aes(x = DO_mgl, y = pH), position = "jitter", size = 0.5, color = "forestgreen") +
  geom_smooth(data = cleaned, aes(x = DO_mgl, y = pH), method = "lm", color = "forestgreen", se = FALSE) +
  stat_poly_eq(data = cleaned, aes(x = DO_mgl, y = pH, label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, parse = TRUE, color = "forestgreen", label.x = -5, label.y = 8) +
  # Add labels and theme
  labs(title = "DO and pH correlation", x = "Dissolved Oxygen (mg/L)", y = "pH") +
  theme_bw()

#### Buoy Upload ####
#
setwd("/Users/ilanajacobs/SURFO/OysterFarm0717")
#open csv with data, but name the function first
bottomjuly=read.csv("Oysters_Bottom_07172024.csv")
surfacejuly=read.csv("Oysters_Surface_07172024.csv")
#look at the structure of the files
str(bottomjuly)
str(surfacejuly)
#
#
# creates a datetime column using data from date and time column.
bottomjuly1 <- bottomjuly %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
surfacejuly1 <- surfacejuly %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
#
#
#### Salinity####
#mean
sal_mean_bottom=mean(bottomjuly$Salinity_ppt, na.rm=TRUE)
sal_mean_surfacejuly=mean(surfacejuly$Salinity_ppt, na.rm=TRUE)


#median
sal_median_bottomjuly=median(bottomjuly$Salinity_ppt, na.rm=TRUE)
sal_median_surfacejuly=median(surfacejuly$Salinity_ppt, na.rm=TRUE)

#standard deviation
sal_sd_bottomjuly=sd(bottomjuly$Salinity_ppt, na.rm=TRUE)
sal_sd_surfacejuly=sd(surfacejuly$Salinity_ppt, na.rm=TRUE)

#range
sal_range_bottomjuly=range(bottomjuly$Salinity_ppt, na.rm=TRUE)
sal_range_surfacejuly=range(surfacejuly$Salinity_ppt, na.rm=TRUE)

#### Chl-a ####

#mean
chl_mean_bottomjuly=mean(bottomjuly$Chla_rfu, na.rm=TRUE)
chl_mean_surfacejuly=mean(surfacejuly$Chla_rfu, na.rm=TRUE)

#median
chl_median_bottomjuly=median(bottomjuly$Chla_rfu, na.rm=TRUE)
chl_median_surfacejuly=median(surfacejuly$Chla_rfu, na.rm=TRUE)

#standard deviation
chl_sd_bottomjuly=sd(bottomjuly$Chla_rfu, na.rm=TRUE)
chl_sd_surfacejuly=sd(surfacejuly$Chla_rfu, na.rm=TRUE)

#range
chl_median_bottomjuly=median(bottomjuly$Chla_rfu, na.rm=TRUE)
chl_median_surfacejuly=median(surfacejuly$Chla_rfu, na.rm=TRUE)
#### DO ####

#mean
do_mean_bottomjuly=mean(bottomjuly$DO_mgl, na.rm=TRUE)
do_mean_surfacejuly=mean(surfacejuly$DO_mgl, na.rm=TRUE)

#median
do_median_bottomjuly=median(bottomjuly$DO_mgl, na.rm=TRUE)
do_median_surfacejuly=median(surfacejuly$DO_mgl, na.rm=TRUE)

#standard deviation
do_sd_bottomjuly=sd(bottomjuly$DO_mgl, na.rm=TRUE)
do_sd_surfacejuly=sd(surfacejuly$DO_mgl, na.rm=TRUE)

#range
do_range_bottomjuly=range(bottomjuly$DO_mgl, na.rm=TRUE)
do_range_surfacejuly=range(surfacejuly$DO_mgl, na.rm=TRUE)

#### pH ####

#mean
ph_mean_bottomjuly=mean(bottomjuly$pH, na.rm=TRUE)
ph_mean_surfacejuly=mean(surfacejuly$pH, na.rm=TRUE)
#median
ph_median_bottomjuly=median(bottomjuly$pH, na.rm=TRUE)
ph_median_surfacejuly=median(surfacejuly$pH, na.rm=TRUE)

#standard deviation
ph_sd_bottomjuly=sd(bottomjuly$pH, na.rm=TRUE)
ph_sd_surfacejuly=sd(surfacejuly$pH, na.rm=TRUE)

#range
ph_range_bottomjuly=range(bottomjuly$pH, na.rm=TRUE)
ph_range_surfacejuly=range(surfacejuly$pH, na.rm=TRUE)

#### Temp ####

#mean
temp_mean_bottomjuly=mean(bottomjuly$Temperature_F, na.rm=TRUE)
temp_mean_surfacejuly=mean(surfacejuly$Temperature_F, na.rm=TRUE)
#median
temp_median_bottomjuly=median(bottomjuly$Temperature_F, na.rm=TRUE)
temp_median_surfacejuly=median(surfacejuly$Temperature_F, na.rm=TRUE)

#standard deviation
temp_sd_bottomjuly=sd(bottomjuly$Temperature_F, na.rm=TRUE)
temp_sd_surfacejuly=sd(surfacejuly$Temperature_F, na.rm=TRUE)

#range
temp_range_bottomjuly=range(bottomjuly$Temperature_F, na.rm=TRUE)
temp_range_surfacejuly=range(surfacejuly$Temperature_F, na.rm=TRUE)
#
ggplot()+
  geom_line(data=bottomjuly1, aes(x = datetime, y = Salinity_ppt, color = Salinity_ppt)) +
  scale_color_gradient2(midpoint = unique(sal_median_bottomjuly), low = "navyblue", mid = "lightpink", high = "darkorange", name="Salinity") +
  #geom_smooth(data=bottomjuly1, method = "lm", color = "steelblue") +

geom_line(data=surfacejuly1, aes(x = datetime, y = Salinity_ppt, color = Salinity_ppt)) +
  #scale_color_gradient2(midpoint = unique(sal_median_surfacejuly), low = "navyblue", mid = "lightpink", high = "darkorange", name="Salinity") +
  #geom_smooth(data=surfacejuly1, method = "lm", color = "steelblue") +
  labs(title = paste("Salinity in July 2024" ),x = "Date", y = "Salinity (ppt)") +
  theme_bw()

#### Moving Mean For Loop ####
# Writing a loop for a 6-hour moving mean 
library(zoo)
#we first specify which columns we want to process. 
columns_to_process=c("Salinity_ppt", "Chla_RFU", "DO_mgl", "pH", "Temperature_F", "Depth_m")
#define the window size, which we for 6 hours will be 12, because there are recordings every 30 minutes
window_size=12
# Write a loop for Bottom Data: loop over each specified column, and calculate the moving mean
for (column in columns_to_process) {
  # Convert the column to a zoo object
  zoo_df <- zoo(bottomjuly1[[column]], order.by = bottomjuly1$datetime)
  
  # Calculate the moving mean
  moving_mean <- rollmean(zoo_df, k = window_size, fill = NA, align = "right")
  
  # Add the moving mean as a new column to the original data frame
  new_column_name <- paste(column, "moving_mean", sep = "_")
  bottomjuly1[[new_column_name]] <- coredata(moving_mean)
}

# Print the first few rows of the data frame to check the results
print(head(bottomjuly1))

# Write a loop for Surface Data: loop over each specified column, and calculate the moving mean
for (column in columns_to_process) {
  # Convert the column to a zoo object
  zoo_df <- zoo(surfacejuly1[[column]], order.by = surfacejuly1$datetime)
  
  # Calculate the moving mean
  moving_mean <- rollmean(zoo_df, k = window_size, fill = NA, align = "right")
  
  # Add the moving mean as a new column to the original data frame
  new_column_name <- paste(column, "moving_mean", sep = "_")
  surfacejuly1[[new_column_name]] <- coredata(moving_mean)
}

# Print the first few rows of the data frame to check the results
print(head(surfacejuly1))

# Writing a loop for a 12-hour moving mean 
library(zoo)
#we first specify which columns we want to process. 
columns_to_process=c("Salinity_ppt", "Chla_RFU", "DO_mgl", "pH", "Temperature_F", "Depth_m")
#define the window size, which we for 6 hours will be 12, because there are recordings every 30 minutes
window_size=24
# Write a loop for Bottom Data: loop over each specified column, and calculate the moving mean
for (column in columns_to_process) {
  # Convert the column to a zoo object
  zoo_df <- zoo(bottomjuly1[[column]], order.by = bottomjuly1$datetime)
  
  # Calculate the moving mean
  moving_mean_12hr <- rollmean(zoo_df, k = window_size, fill = NA, align = "right")
  
  # Add the moving mean as a new column to the original data frame
  new_column_name <- paste(column, "moving_mean_12hr", sep = "_")
  bottomjuly1[[new_column_name]] <- coredata(moving_mean_12hr)
}

# Print the first few rows of the data frame to check the results
print(head(bottomjuly1))

# Write a loop for Surface Data: loop over each specified column, and calculate the moving mean
for (column in columns_to_process) {
  # Convert the column to a zoo object
  zoo_df <- zoo(surfacejuly1[[column]], order.by = surfacejuly1$datetime)
  
  # Calculate the moving mean
  moving_mean_12hr <- rollmean(zoo_df, k = window_size, fill = NA, align = "right")
  
  # Add the moving mean as a new column to the original data frame
  new_column_name <- paste(column, "moving_mean_12hr", sep = "_")
  surfacejuly1[[new_column_name]] <- coredata(moving_mean_12hr)
}

# Print the first few rows of the data frame to check the results
print(head(surfacejuly1))


#### Plotting Moving Mean 6 Hour, 12 Hour ####
###
# Salinity 6 #
ggplot()+
  geom_line(data=bottomjuly1, aes(x = datetime, y = Salinity_ppt_moving_mean, color="Bottom"), na.rm = TRUE) +
  geom_line(data=surfacejuly1, aes(x = datetime, y = Salinity_ppt_moving_mean, color="Surface")) +
  scale_color_manual(values = c("Bottom" = "darkgreen", "Surface" = "steelblue")) +
  labs(title = paste("Salinity in July 2024" ),x = "Date", y = "Salinity (ppt)") +
  theme_classic()
# Dissolved Oxygen 6#
ggplot() +
  geom_line(data = bottomjuly1, aes(x = datetime, y = DO_mgl_moving_mean, color = "Bottom"), na.rm = TRUE) +
  geom_line(data = surfacejuly1, aes(x = datetime, y = DO_mgl_moving_mean, color = "Surface"), na.rm = TRUE) +
  scale_color_manual(values = c("Bottom" = "darkgreen", "Surface" = "steelblue")) +
  labs(title = "Dissolved Oxygen in July 2024", x = "Date", y = "Dissolved Oxygen, mg/L") +
  theme_classic()
# pH 6#
ggplot()+
  geom_line(data=bottomjuly1, aes(x = datetime, y = pH_moving_mean, color="Bottom")) +
  geom_line(data=surfacejuly1, aes(x = datetime, y = pH_moving_mean, color="Surface")) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "darkgreen", "Surface" = "steelblue")) +
  labs(title = paste("pH in July 2024" ),x = "Date", y = "pH") +
  theme_classic()
# Temperature F 6#
ggplot()+
  geom_line(data=bottomjuly1, aes(x = datetime, y = Temperature_F_moving_mean, color="Bottom")) +
  geom_line(data=surfacejuly1, aes(x = datetime, y = Temperature_F_moving_mean, color="Surface")) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "darkgreen", "Surface" = "steelblue")) +
  labs(title = paste("Temperature in July 2024" ),x = "Date", y = "Temperature, ºF") +
  theme_classic()

# Chlorophyll 6#
ggplot()+
  geom_line(data=bottomjuly1, aes(x = datetime, y = Chla_RFU, color="Bottom")) +
  geom_line(data=surfacejuly1, aes(x = datetime, y = Chla_RFU, color="Surface")) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = paste("Chlorophyll from June 17 to July 16 2024" ),x = "Date", y = "Chl-a RFU") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### ### ### ### ### ###
# Salinity 12#
ggplot()+
  geom_line(data=bottomjuly1, aes(x = datetime, y = Salinity_ppt_moving_mean_12hr, color="Bottom"), na.rm = TRUE) +
  geom_line(data=surfacejuly1, aes(x = datetime, y = Salinity_ppt_moving_mean_12hr, color="Surface")) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = paste("Salinity from June 17 to July 16 2024" ),x = "Date", y = "Salinity (ppt)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Salinity_June.png", plot = Salinity_June, width = 1039, height = 482, units = "px", dpi = 300)
# Dissolved Oxyden 12#
DO_June =ggplot() +
  geom_line(data = bottomjuly1, aes(x = datetime, y = DO_mgl_moving_mean_12hr, color = "Bottom"), na.rm = TRUE) +
  geom_line(data = surfacejuly1, aes(x = datetime, y = DO_mgl_moving_mean_12hr, color = "Surface"), na.rm = TRUE) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = "Dissolved Oxygen from June 17 to July 16 2024", x = "Date", y = "Dissolved Oxygen, mg/L") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("DO_June.png", plot = DO_June, width = 1039, height = 482, units = "px", dpi = 300)
# pH 12#
pH_June = ggplot()+
  geom_line(data=bottomjuly1, aes(x = datetime, y = pH_moving_mean_12hr, color="Bottom")) +
  geom_line(data=surfacejuly1, aes(x = datetime, y = pH_moving_mean_12hr, color="Surface")) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = paste("pH from June 17 to July 16 2024" ),x = "Date", y = "pH") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("pH_June.png", plot = pH_June, width = 1039, height = 482, units = "px", dpi = 300)
# Temperature F 12#
Temp_June = ggplot()+
  geom_line(data=bottomjuly1, aes(x = datetime, y = Temperature_F_moving_mean_12hr, color="Bottom")) +
  geom_line(data=surfacejuly1, aes(x = datetime, y = Temperature_F_moving_mean_12hr, color="Surface")) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = paste("Temperature from June 17 to July 16 2024" ),x = "Date", y = "Temperature, ºF") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("Temp_June.png", plot = Temp_June, width = 1039, height = 482, units = "px", dpi = 300)

## Check pH salinity correlation

ggplot() +
  # Add barnacles data
  geom_point(data = barnacles, aes(x = DO_mgl, y = pH), position = "jitter", size = 0.5, color = "red") +
  geom_smooth(data = barnacles, aes(x = DO_mgl, y = pH), method = "lm", color = "red", se = FALSE) +
  stat_poly_eq(data = barnacles, aes(x = DO_mgl, y = pH, label = ..rr.label..), 
               formula = y ~ x, parse = TRUE, color = "red") +
  # Add surface data before 
  geom_point(data = surface, aes(x = DO_mgl, y = pH), position = "jitter", size = 0.5, color = "blue") +
  geom_smooth(data = surface, aes(x = DO_mgl, y = pH), method = "lm", color = "blue", se = FALSE) +
  stat_poly_eq(data = surface, aes(x = DO_mgl, y = pH, label = paste(..rr.label..)), 
               formula = y ~ x, parse = TRUE, color = "blue", label.x = 5, label.y = 8) +
  # add surface data after
  geom_point(data = cleaned, aes(x = DO_mgl, y = pH), position = "jitter", size = 0.5, color = "forestgreen") +
  geom_smooth(data = cleaned, aes(x = DO_mgl, y = pH), method = "lm", color = "forestgreen", se = FALSE) +
  stat_poly_eq(data = cleaned, aes(x = DO_mgl, y = pH, label = paste(..rr.label..)), 
               formula = y ~ x, parse = TRUE, color = "forestgreen", label.x = -5, label.y = 8) +
  # add cleaned data to check r2
  geom_point(data = surfacejuly1, aes(x = DO_mgl, y = pH), position = "jitter", size = 0.5, color = "orange") +
  geom_smooth(data = surfacejuly1, aes(x = DO_mgl, y = pH), method = "lm", color = "orange", se = FALSE) +
  stat_poly_eq(data = surfacejuly1, aes(x = DO_mgl, y = pH, label = paste(..rr.label..)), 
               formula = y ~ x, parse = TRUE, color = "orange", label.x = -5, label.y = -0.5) +

  # Add labels and theme
  labs(title = "DO and pH correlation", x = "Dissolved Oxygen (mg/L)", y = "pH") +
  theme_classic()

# Check new sensor to make sure pH and DO are correlated
setwd("~/SURFO")
newsensor=read.csv("newsensor_bottom.csv")
ggplot() +
  geom_point(data = newsensor, aes(x = DO_mgl, y = pH), position = "jitter", size = 0.5, color = "forestgreen") +
  geom_smooth(data = newsensor, aes(x = DO_mgl, y = pH), method = "lm", color = "forestgreen", se = FALSE) +
  stat_poly_eq(data = newsensor, aes(x = DO_mgl, y = pH, label = ..rr.label..), 
               formula = y ~ x, parse = TRUE, color = "forestgreen")+
  labs(title = "New Bottom Sensor", x = "Dissolved Oxygen, mg/L", y = "pH") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# August Oyster Report ####
# load in files
setwd("/Users/ilanajacobs/SURFO/OysterFarm0817")
#open csv with data, but name the function first
bottomaug=read.csv("Oysters_Bottom_08172024.csv")
surfaceaug=read.csv("Oysters_Surface_08172024.csv")
# creating datetime column in the dataframe 
bottomaug1 <- bottomaug %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
surfaceaug1 <- surfaceaug %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))

#### 

summary(bottomaug) # prints summary stats for entire dataframe

# Salinity ####
#mean
sal_mean_bottomaug=mean(bottomaug$Salinity_ppt, na.rm=TRUE)
sal_mean_surfaceaug=mean(surfaceaug$Salinity_ppt, na.rm=TRUE)


#median
sal_median_bottomaug=median(bottomaug$Salinity_ppt, na.rm=TRUE)
sal_median_surfaceaug=median(surfaceaug$Salinity_ppt, na.rm=TRUE)

#standard deviation
sal_sd_bottomaug=sd(bottomaug$Salinity_ppt, na.rm=TRUE)
sal_sd_surfaceaug=sd(surfaceaug$Salinity_ppt, na.rm=TRUE)

#range
sal_range_bottomjuly=range(bottomaug$Salinity_ppt, na.rm=TRUE)
sal_range_surfacejuly=range(surfaceaug$Salinity_ppt, na.rm=TRUE)

# DO ####
#mean
do_mean_bottomaug=mean(bottomaug$DO_mgl, na.rm=TRUE)
do_mean_surfacejuly=mean(surfaceaug$DO_mgl, na.rm=TRUE)

#median
do_median_bottomaug=median(bottomaug$DO_mgl, na.rm=TRUE)
do_median_surfacejuly=median(surfaceaug$DO_mgl, na.rm=TRUE)

#standard deviation
do_sd_bottomaug=sd(bottomaug$DO_mgl, na.rm=TRUE)
do_sd_surfaceaug=sd(surfaceaug$DO_mgl, na.rm=TRUE)

#range
do_range_bottomjuly=range(bottomaug$DO_mgl, na.rm=TRUE) 
do_range_surfacejuly=range(surfaceaug$DO_mgl, na.rm=TRUE)

# pH ####
#mean
ph_mean_bottomaug=mean(bottomaug$pH, na.rm=TRUE)
ph_mean_surfaceaug=mean(surfaceaug$pH, na.rm=TRUE)
#median
ph_median_bottomaug=median(bottomaug$pH, na.rm=TRUE)
ph_median_surfaceaug=median(surfaceaug$pH, na.rm=TRUE)

#standard deviation
ph_sd_bottomaug=sd(bottomaug$pH, na.rm=TRUE)
ph_sd_surfacejuly=sd(surfaceaug$pH, na.rm=TRUE)

#range
ph_range_bottomaug=range(bottomaug$pH, na.rm=TRUE)
ph_range_surfaceaug=range(surfaceaug$pH, na.rm=TRUE)

# Temperature ####
#mean
temp_mean_bottomaug=mean(bottomaug$Temperature_F, na.rm=TRUE)
temp_mean_surfaceaug=mean(surfaceaug$Temperature_F, na.rm=TRUE)
#median
temp_median_bottomaug=median(bottomaug$Temperature_F, na.rm=TRUE)
temp_median_surfaceaug=median(surfaceaug$Temperature_F, na.rm=TRUE)

#standard deviation
temp_sd_bottomaug=sd(bottomaug$Temperature_F, na.rm=TRUE)
temp_sd_surfaceaug=sd(surfaceaug$Temperature_F, na.rm=TRUE)

#range
temp_range_bottomaug=range(bottomaug$Temperature_F, na.rm=TRUE)
temp_range_surfaceaug=range(surfaceaug$Temperature_F, na.rm=TRUE)


# Moving Mean using Zoo Pkg #### Same as in July report, but adapted for August data!
# Writing a loop for a 12-hour moving mean 
library(zoo)
#we first specify which columns we want to process. 
columns_to_process=c("Salinity_ppt", "DO_mgl", "pH", "Temperature_F", "Depth_m")
#define the window size, which we for 12 hours will be 24, because there are recordings every 30 minutes
window_size=24
# Write a loop for Bottom Data: loop over each specified column, and calculate the moving mean
for (column in columns_to_process) {
  # Convert the column to a zoo object
  zoo_df_aug <- zoo(bottomaug1[[column]], order.by = bottomaug1$datetime)
  
  # Calculate the moving mean
  moving_mean_12hr <- rollmean(zoo_df_aug, k = window_size, fill = NA, align = "right")
  
  # Add the moving mean as a new column to the original data frame
  new_column_name <- paste(column, "moving_mean_12hr", sep = "_")
  bottomaug1[[new_column_name]] <- coredata(moving_mean_12hr)
}

# Print the first few rows of the data frame to check the results
print(head(bottomaug1))

# Write a loop for Surface Data: loop over each specified column, and calculate the moving mean
for (column in columns_to_process) {
  # Convert the column to a zoo object
  zoo_df_aug <- zoo(surfaceaug1[[column]], order.by = surfaceaug1$datetime)
  
  # Calculate the moving mean
  moving_mean_12hr <- rollmean(zoo_df_aug, k = window_size, fill = NA, align = "right")
  
  # Add the moving mean as a new column to the original data frame
  new_column_name <- paste(column, "moving_mean_12hr", sep = "_")
  surfaceaug1[[new_column_name]] <- coredata(moving_mean_12hr)
}

# Print the first few rows of the data frame to check the results
print(head(surfaceaug1))

# Plots for Report ####
# Salinity 12#
sal_aug=ggplot()+
  geom_line(data=bottomaug1, aes(x = datetime, y = Salinity_ppt_moving_mean_12hr, color="Bottom"), na.rm = TRUE) +
  geom_line(data=surfaceaug1, aes(x = datetime, y = Salinity_ppt_moving_mean_12hr, color="Surface"), na.rm = TRUE) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = paste("Salinity from July 17 to August 16 2024" ),x = "Date", y = "Salinity (ppt)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(sal_aug)
# Dissolved Oxyden 12#
DO_aug=ggplot() +
  geom_line(data = bottomaug1, aes(x = datetime, y = DO_mgl_moving_mean_12hr, color = "Bottom"), na.rm = TRUE) +
  geom_line(data = surfaceaug1, aes(x = datetime, y = DO_mgl_moving_mean_12hr, color = "Surface"), na.rm = TRUE) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = "Dissolved Oxygen from July 17 to August 16 2024", x = "Date", y = "Dissolved Oxygen, mg/L") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# pH 12#
pH_aug = ggplot()+
  geom_line(data=bottomaug1, aes(x = datetime, y = pH_moving_mean_12hr, color="Bottom")) +
  geom_line(data=surfaceaug1, aes(x = datetime, y = pH_moving_mean_12hr, color="Surface")) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = paste("pH from July 17 to August 16 2024" ),x = "Date", y = "pH") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# Temperature F 12#
Temp_aug = ggplot()+
  geom_line(data=bottomaug1, aes(x = datetime, y = Temperature_F_moving_mean_12hr, color="Bottom")) +
  geom_line(data=surfaceaug1, aes(x = datetime, y = Temperature_F_moving_mean_12hr, color="Surface")) +
  scale_color_manual(name="Sensor", values = c("Bottom" = "purple4", "Surface" = "orange")) +
  labs(title = paste("Temperature from July 17 to August 16 2024" ),x = "Date", y = "Temperature, ºF") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
