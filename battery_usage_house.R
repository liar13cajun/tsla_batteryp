# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For working with dates and times
library(hms)        # For handling time-of-day data
library(gridExtra)  # For arranging multiple plots in a grid

# Define date and file path parameters
day <- "2024-11-15"
month <- "11"
year <- "2024"
file_directory <- "C:/Project_R/tesla_download/"

# Load CSV files
df_power <- read.csv(paste0(file_directory, "power/", day, ".csv"))     # Power data for a specific day
df_energy <- read.csv(paste0(file_directory, "energy/", year, "-", month, ".csv")) # Energy data for the month
df_soe <- read.csv(paste0(file_directory, "soe/", day, ".csv"))        # State-of-energy data for a specific day

#########################
# Analyze and visualize energy usage
# Focus: Usage from battery vs export to solar
#########################

# Convert 'timestamp' to a datetime format
df_energy$timestamp <- as.POSIXct(df_energy$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Extract date and time components
df_energy$date <- as.Date(df_energy$timestamp)        # Extract the date
df_energy$time <- format(df_energy$timestamp, "%H:%M:%S") # Extract the time as a string

# Summarize daily energy imported from the battery
daily_summary <- df_energy %>%
  group_by(date) %>%
  summarise(total_imported = sum(consumer_energy_imported_from_battery, na.rm = TRUE))

# Line plot: Consumer energy imported from the battery over time
ggplot(df_energy, aes(x = timestamp, y = consumer_energy_imported_from_battery)) +
  geom_line(color = "blue") +
  labs(title = "Consumer Energy Imported from Battery Over Time",
       x = "Time",
       y = "Energy (kWh)") +
  theme_minimal()

# Boxplot: Distribution of energy imported from the battery by time of day
ggplot(df_energy, aes(x = time, y = consumer_energy_imported_from_battery)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.shape = NA) +
  labs(title = "Distribution of Consumer Energy Imported from Battery (Nov)",
       x = "Time of Day",
       y = "Battery Energy Usage (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
