# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For working with dates and times
library(hms)        # For handling time-of-day data
library(gridExtra)  # For arranging multiple plots in a grid

# Define date parameters for file paths
day <- "2024-11-15"
month <- "11"
year <- "2024"

# Define file directory path
file_directory <- "C:/Project_R/tesla_download/"

# Load power data for the specified day
df_power <- read.csv(paste0(file_directory, "power/", day, ".csv"))

# Load energy data for the specified month
df_energy <- read.csv(paste0(file_directory, "energy/", year, "-", month, ".csv"))

# Load state-of-energy (SOE) data for the specified day
df_soe <- read.csv(paste0(file_directory, "soe/", day, ".csv"))

#########################
# Plot and analyze energy data
# Analyze usage from battery vs export to solar
#########################

# Convert the 'timestamp' column to datetime format
df_energy$timestamp <- as.POSIXct(df_energy$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Extract date and time components from the timestamp
df_energy$date <- as.Date(df_energy$timestamp)        # Extract the date
df_energy$time <- format(df_energy$timestamp, "%H:%M:%S") # Extract the time as a string

# Summarize daily energy imported from solar
daily_summary <- df_energy %>%
  group_by(date) %>%
  summarise(total_imported = sum(battery_energy_imported_from_solar, na.rm = TRUE))

# Line plot: Battery energy imported from solar across times
ggplot(df_energy, aes(x = timestamp, y = battery_energy_imported_from_solar)) +
  geom_line(color = "blue") +
  labs(title = "Battery Energy Imported from Solar",
       x = "Time",
       y = "Energy (kWh)") +
  theme_minimal()

# Boxplot: Distribution of battery energy imported from solar by time of day
ggplot(df_energy, aes(x = time, y = battery_energy_imported_from_solar)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red") +
  labs(title = "Boxplot of Battery Energy Imported from Solar",
       x = "Time of Day",
       y = "Battery Energy Imported (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
