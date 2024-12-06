
# OVERALL STEP 2 ---------------------------------------------------------------
# Next we need to bring in the bottle samples ----------------------------------

# we need to correct onto the NBS scale.(done in matlab)
# Bring the data back into R. surface_calculated=read.csv("surface_summer_pco2pred.csv")
bottom_calculated=read.csv("bottom_summer_pco2pred.csv")

surface_calculated$DateTime=as.POSIXct(surface_calculated$DateTime, 
                                         format="%d-%b-%Y %H:%M:%S") 
                                          # "b" for abbreviated month. 
bottom_calculated$DateTime=as.POSIXct(bottom_calculated$DateTime, 
                                        format="%d-%m-%Y %H:%M:%S")

# Read in the data
oysterfarm_ts=read.csv("/Users/ilanajacobs/SURFO/AGU_Data/combined_oyster_data.csv")

# Combine Date and Time into a datetime column
oysterfarm_ts=oysterfarm_ts %>%
  mutate(datetime=as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))

# Extract month from the new datetime
oysterfarm_ts=oysterfarm_ts %>%
  mutate(month=month(datetime, label=TRUE))

# Subset by depth
oysterfarm_surface=subset(oysterfarm_ts, layer=="surface")
oysterfarm_bottom=subset(oysterfarm_ts, layer=="bottom")

# Complete time series timestamps ----------------------------------------------
# Generate a complete sequence of 30-minute intervals for bottom data
complete_timestamps <- data.frame(
  datetime = seq(
    from = min(oysterfarm_bottom$datetime, na.rm = TRUE),
    to = max(oysterfarm_bottom$datetime, na.rm = TRUE),
    by = "30 min"
  )
)
# Merge with the complete sequence
oysterfarm_bottom_complete <- complete_timestamps %>%
  left_join(oysterfarm_bottom, by = "datetime")

# Generate a complete sequence of 30-minute intervals for surface data
complete_timestamps_surface <- data.frame(
  datetime = seq(
    from = min(oysterfarm_surface$datetime, na.rm = TRUE),
    to = max(oysterfarm_surface$datetime, na.rm = TRUE),
    by = "30 min"
  )
)

oysterfarm_surface_complete <- complete_timestamps_surface %>%
  left_join(oysterfarm_surface, by = "datetime")

# Fill missing values ----------------------------------------------------------

oysterfarm_bottom_complete <- oysterfarm_bottom_complete %>%
  mutate(across(everything(), ~ na.approx(.x, x = datetime, na.rm = FALSE)))

oysterfarm_bottom_complete <- oysterfarm_bottom_complete %>%
  mutate(across(everything(), ~ na.locf(.x, na.rm = FALSE)))

# Check that this worked

print(sum(is.na(oysterfarm_bottom_complete$datetime)))  # Should be 0
print(nrow(oysterfarm_bottom_complete))                # Should match the expected row count


# Apply linear interpolation to fill missing values in the surface dataset
oysterfarm_surface_complete <- oysterfarm_surface_complete %>%
  mutate(across(everything(), ~ na.approx(.x, x = datetime, na.rm = FALSE)))

# Apply last observation carried forward (LOCF) for any remaining missing values
oysterfarm_surface_complete <- oysterfarm_surface_complete %>%
  mutate(across(everything(), ~ na.locf(.x, na.rm = FALSE)))

# Check if there are any missing values in the surface dataset
print(sum(is.na(oysterfarm_surface_complete$datetime)))  # Should be 0

# Verify the number of rows
print(nrow(oysterfarm_surface_complete))  # Should match the expected row count




# Moving mean for combined dataset (12 hours) ----------------------------------

library(zoo)
#we first specify which columns we want to process. 
columns_to_process=c("Salinity_ppt", "DO_mgl", "pH", "Temperature_F", "Depth_m")
#define the window size, which we for 12 hours will be 24, because 
# there are recordings every 30 minutes
window_size=24

# Calculate moving mean --------------------------------------------------------
# surface
for (column in columns_to_process) {
  # Convert the column to a zoo object
  zoo_df=zoo(oysterfarm_surface[[column]], 
                order.by=oysterfarm_surface$datetime)
  
  # Calculate the moving mean
  moving_mean_12hr=rollmean(zoo_df, k=window_size, fill=NA, align="right")
  
  # Add the moving mean as a new column to the original data frame
  new_column_name=paste(column, "moving_mean_12hr", sep="_")
  oysterfarm_surface[[new_column_name]]=coredata(moving_mean_12hr)
}

# bottom
for (column in columns_to_process) {
  # Convert the column to a zoo object
  zoo_df=zoo(oysterfarm_bottom[[column]], 
                order.by=oysterfarm_bottom$datetime)
  
  # Calculate the moving mean
  moving_mean_12hr=rollmean(zoo_df, k=window_size, fill=NA, align="right")
  
  # Add the moving mean as a new column to the original data frame
  new_column_name=paste(column, "moving_mean_12hr", sep="_")
  oysterfarm_bottom[[new_column_name]]=coredata(moving_mean_12hr)
}

# Combine Surface and Bottom to one CSV ----------------------------------------

missing_columns=setdiff(colnames(oysterfarm_surface), colnames(oysterfarm_bottom))

for (col in missing_columns) {
  oysterfarm_bottom[[col]]=NA
}
oyster_farm_ts=rbind(oysterfarm_surface, oysterfarm_bottom)

# Subset only 2024 Bottle Samples ----------------------------------------------


# Extract the month from the DateTime column and add it as a new column called Month
bottom_calculated=bottom_calculated %>%
  mutate(Month=month(DateTime, label=TRUE))
bottom_calculated=bottom_calculated %>%
  mutate(datetime=as.POSIXct(paste(DateTime), format="%Y-%m-%d %H:%M:%S"))
bottom_calculated=bottom_calculated %>%
  mutate(year=lubridate::year(datetime))

bottom_calculated_aug=subset(bottom_calculated,Month=='Aug')
bottom_calculated_jul=subset(bottom_calculated,Month=='Jul')
bottom_calculated_sep=subset(bottom_calculated, Month=='Sep')
bottom_calculated_oct=subset(bottom_calculated, Month=='Oct')
bottom_calculated_jun=subset(bottom_calculated, Month =='Jun')

bottom_calculated_fall=rbind(
  bottom_calculated_aug,
  bottom_calculated_jul,
  bottom_calculated_sep,
  bottom_calculated_oct,
  bottom_calculated_jun
)

bottom_calculated_fall24=bottom_calculated_fall %>% 
  filter(year != 2023) 
# Subset the time series by month

oyster_farm_ts_aug=subset(oyster_farm_ts,month=='Aug')
oyster_farm_ts_jul=subset(oyster_farm_ts,month=='Jul')
oyster_farm_ts_sep=subset(oyster_farm_ts, month=='Sep')
oyster_farm_ts_oct=subset(oyster_farm_ts, month=='Oct')

oyster_farm_ts_fall=rbind(
  oyster_farm_ts_aug,
  oyster_farm_ts_jul,
  oyster_farm_ts_sep,
  oyster_farm_ts_oct
)
str(oyster_farm_ts_fall)

oyster_farm_ts_fall=oyster_farm_ts_fall %>%
  mutate(month=month(datetime, label=TRUE))
oyster_farm_ts_fall=oyster_farm_ts_fall %>%
  mutate(year=lubridate::year(datetime))
oyster_farm_ts_fall24=oyster_farm_ts_fall %>% 
  filter(year != 2023)


# Make some plots to look at the time series -----------------------------------

ggplot()+
  geom_line(data=oyster_farm_ts, aes(x=datetime, y=pH_moving_mean_12hr, color=layer))+
  scale_color_manual(values=c("steelblue", "hotpink"))+
  geom_point(data= surface_calculated, aes(x=DateTime, y=pHcalc), shape=22)+
  geom_point(data=bottom_calculated,aes(x=DateTime, y=pHcalc), shape=21)+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

# Plot to correct pH
ggplot()+
  geom_line(data=oysterfarm_bottom,aes(x=datetime, y=pH_moving_mean_12hr))+
  #geom_line(data=bottomaug1, aes(x=datetime, y=corrected_pH_moving_mean_12hr)
  #, linetype="dotted") +
  #geom_point(data=bottom_calculated,aes(x=DateTime, y=pHcalc, color="Calculated"))+
  scale_color_manual(name="Measurement", 
                     values=c("Calculated"="green4", 
                                "Lab"="red", "In-Situ"="blue"))+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))


ggplot() +
  geom_line(
    data=oyster_farm_ts_fall24 %>% filter(layer == "bottom", month %in% c("Sep", "Oct")),
    aes(x=datetime, y=pH_moving_mean_12hr, color=layer)
  ) +
  geom_point(
    data=bottom_calculated_fall24,
    aes(x=datetime, y=pHcalc)
  ) +
  theme_classic()

# pH Correction ----------------------------------------------------------------

# Filter data to include only "bottom" layer
bottle_samples=bottom_calculated_fall24
time_series=oyster_farm_ts_fall24 %>% filter(layer == "bottom")


# Step 3: Generally Fix the Time Series ----------------------------------------
# Find the closest time series point for each bottle sample
matched_data=bottle_samples %>%
  rowwise() %>%
  mutate(
    closest_time=time_series$datetime[which.min(abs(time_series$datetime - datetime))],
    time_series_value=time_series$pH_moving_mean_12hr[which.min(abs(time_series$datetime - datetime))]
  ) %>%
  ungroup() %>%
  mutate(correction_value=pHcalc - time_series_value)


time_series=time_series %>%
  left_join(
    matched_data %>% select(datetime=closest_time, correction_value),
    by="datetime"
  ) %>%
  mutate(
    correction_value=na.approx(correction_value, x=datetime, na.rm=FALSE),
    corrected_pH=pH_moving_mean_12hr + correction_value
  )

# The corrected time series now has corrected_pH
time_series_corrected=time_series

#Plot the corrected --------------------------------------------------------

bottom_Sep$corrected_pH_moving_mean_12hr=bottom_Sep$pH_moving_mean_12hr-mean_pH_calc_oct

ggplot()+
  #geom_line(data=time_series, aes(x=datetime, y=pH_moving_mean_12hr), color="red")+
  geom_point(data=bottle_samples, aes(x=datetime, y=pHcalc))+
  geom_line (data=time_series_corrected, aes(x=datetime, y=corrected_pH), color="forestgreen")+
  theme_classic()
# Combine the newly fixed bottom data to the full time series ------------------

oyster_farm_ts$pH_moving_mean_12hr[oyster_farm_ts$layer == "bottom"] <- 
  time_series_corrected$corrected_pH

# Fix the surface --------------------------------------------------------------
# Because I don't have the time to troubleshoot this I am using old, already analyzed data from
# the file "Oyster_Chemistry.R" to fix the July- August Surface data. 

# Replace surface data in oyster_farm_ts with values from bottomaug1
oyster_farm_ts <- oyster_farm_ts %>%
  mutate(
    # Replace surface data with corresponding bottomaug1 data
    Salinity_ppt_moving_mean_12hr = if_else(
      layer == "surface" & datetime %in% bottomaug1$datetime, 
      bottomaug1$Salinity_ppt_moving_mean_12hr[match(datetime, bottomaug1$datetime)], 
      Salinity_ppt_moving_mean_12hr
    ),
    DO_mgl_moving_mean_12hr = if_else(
      layer == "surface" & datetime %in% bottomaug1$datetime, 
      bottomaug1$DO_mgl_moving_mean_12hr[match(datetime, bottomaug1$datetime)], 
      DO_mgl_moving_mean_12hr
    ),
    pH_moving_mean_12hr = if_else(
      layer == "surface" & datetime %in% bottomaug1$datetime, 
      bottomaug1$pH_moving_mean_12hr[match(datetime, bottomaug1$datetime)], 
      pH_moving_mean_12hr
    ),
    Temperature_F_moving_mean_12hr = if_else(
      layer == "surface" & datetime %in% bottomaug1$datetime, 
      bottomaug1$Temperature_F_moving_mean_12hr[match(datetime, bottomaug1$datetime)], 
      Temperature_F_moving_mean_12hr
    )
  )

# Check if the replacement worked
print(head(oyster_farm_ts))


colnames(oyster_farm_ts)

write.csv(oyster_farm_ts, "oysterfarm_full_ts.csv")

# Plot two panels of plots. ----------------------------------------------------

# Define colors for the variables, but with different shades for surface and bottom
parameter_colors <- c("Temperature" = "blue",                        
                      "Salinity" = "green",                        
                      "DO" = "purple",                        
                      "pH" = "red")

depth_colors <- c("surface" = "lightblue",    # Lighter blue for surface temperature
                  "bottom" = "darkblue",    # Darker blue for bottom temperature
                  "surface_salinity" = "lightgreen",    # Lighter green for surface salinity
                  "bottom_salinity" = "darkgreen",    # Darker green for bottom salinity
                  "surface_DO" = "mediumpurple",  # Lighter purple for surface DO
                  "bottom_DO" = "purple4",   # Darker purple for bottom DO
                  "surface_pH" = "lightcoral",   # Lighter red for surface pH
                  "bottom_pH" = "darkred")      # Darker red for bottom pH

# Plot 1: Temperature and Salinity with depth shades
temp_sal_plot <- ggplot(oyster_farm_ts) +   
  geom_line(aes(x = datetime, y = Temperature_F_moving_mean_12hr,                
                color = ifelse(layer == "surface", "surface", "bottom"),
                linetype = layer), size = 1.25) +   # Increase line thickness
  geom_line(aes(x = datetime, y = Salinity_ppt_moving_mean_12hr,                  
                color = ifelse(layer == "surface", "surface_salinity", "bottom_salinity"),
                linetype = layer), size = 1.25) +   # Increase line thickness
  scale_color_manual(values = depth_colors) +   
  scale_linetype_manual(values = c("solid", "solid")) +  # Set dashed line to dotted
  labs(title = "Temperature and Salinity",         
       x = "Datetime", y = "Values",         
       color = "Parameters", linetype = "Layer") +   
  theme_classic()  

# Plot 2: Dissolved Oxygen and pH with depth shades
do_ph_plot <- ggplot(oyster_farm_ts) +   
  geom_line(aes(x = datetime, y = DO_mgl_moving_mean_12hr,                  
                color = ifelse(layer == "surface", "surface_DO", "bottom_DO"),
                linetype = layer), size = 1.25) +   # Increase line thickness
  geom_line(aes(x = datetime, y = pH_moving_mean_12hr,                  
                color = ifelse(layer == "surface", "surface_pH", "bottom_pH"),
                linetype = layer), size = 1.25) +   # Increase line thickness
  scale_color_manual(values = depth_colors) +   
  scale_linetype_manual(values = c("solid", "solid")) +  # Set dashed line to dotted
  labs(title = "Dissolved Oxygen and pH",         
       x = "Datetime", y = "Values",         
       color = "Parameters", linetype = "Layer") +   
  theme_classic()  

# Combine plots vertically
combined_plot <- temp_sal_plot / do_ph_plot  

# Print combined plot
print(combined_plot)  

