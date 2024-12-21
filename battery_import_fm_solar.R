# Load required libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(hms)
library(lubridate)
library(gridExtra)



# 18-12-2024

day <- "2024-11-15"
month <- "11"
year <- "2024"

file_directory <- "C:/Project_R/tesla_download/"
  
  
# power
  
df_power <- read.csv(paste0(file_directory,"power/",day,".csv"))
  
  
# energy

df_energy <- read.csv(paste0(file_directory,"energy/",year,"-",month,".csv"))
  

# soe

df_soe <- read.csv(paste0(file_directory,"soe/",day,".csv"))



#########################
# plot df energy 
# across each day 
# usage from battery 
# vs 
# export to solar
# create summary 




#battery energy import from solar 
# Convert the timestamp to a datetime format
df_energy$timestamp <- as.POSIXct(df_energy$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Plot the column 'battery_energy_imported_from_solar'
ggplot(df_energy, aes(x = timestamp, y = battery_energy_imported_from_solar)) +
  geom_line(color = "blue") +
  labs(title = "Battery Energy Imported from Solar",
       x = "Time",
       y = "Energy (kWh)") +
  theme_minimal()



# Assuming the CSV file is already loaded into df_energy
# Convert the timestamp to date and time formats
df_energy$timestamp <- as.POSIXct(df_energy$timestamp, format = "%Y-%m-%d %H:%M:%S")
df_energy$date <- as.Date(df_energy$timestamp) # Extract date
df_energy$time <- format(df_energy$timestamp, "%H:%M:%S") # Extract time as a string

# Summarize the data by day
daily_summary <- df_energy %>%
  group_by(date) %>%
  summarise(total_imported = sum(battery_energy_imported_from_solar, na.rm = TRUE))

# Create a boxplot
ggplot(df_energy, aes(x = time, y = battery_energy_imported_from_solar)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red") +
  labs(title = "Boxplot of Battery Energy Imported from Solar",
       x = "Time of Day",
       y = "Battery Energy Imported (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

