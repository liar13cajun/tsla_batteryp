# Load required libraries
library(tidyverse)

# Define file directory and months range
file_directory <- "C:/Project_R/tesla_download/energy/"
months <- 3:11
year <- "2024"

# Create an empty data frame to store the results
monthly_summary <- data.frame(
  Month = character(),
  battery_energy_fm_grid = numeric(),
  battery_energy_fm_solar = numeric(),
  usage_fm_battery = numeric(),
  usage_fm_solar = numeric(),
  solar_to_grid = numeric(),
  solar_generation = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each month to load data and calculate totals
for (month in months) {
  # Format month to two digits
  month_str <- sprintf("%02d", month)
  
  # Construct file path and load the data
  file_path <- paste0(file_directory, year, "-", month_str, ".csv")
  df_energy <- read.csv(file_path)
  
  # Calculate the total values for each metric
  battery_energy_fm_grid <- sum(df_energy$battery_energy_imported_from_grid, na.rm = TRUE)
  battery_energy_fm_solar <- sum(df_energy$battery_energy_imported_from_solar, na.rm= TRUE)
  usage_fm_battery <- sum(df_energy$consumer_energy_imported_from_battery, na.rm=TRUE)
  usage_fm_solar <- sum(df_energy$consumer_energy_imported_from_solar, na.rm=TRUE)
  solar_to_grid <- sum(df_energy$grid_energy_exported_from_solar, na.rm=TRUE)
  solar_generation <- sum(df_energy$total_solar_generation, na.rm=TRUE)
  
  # Append the results to the summary data frame
  monthly_summary <- rbind(
    monthly_summary,
    data.frame(
      Month = paste(year, month_str, sep = "-"), 
      usage_fm_battery = usage_fm_battery,
      battery_energy_fm_grid = battery_energy_fm_grid,
      battery_energy_fm_solar = battery_energy_fm_solar,
      usage_fm_solar = usage_fm_solar,
      solar_to_grid = solar_to_grid,
      solar_generation = solar_generation
    )
  )
}

# Print the monthly summary data
print(monthly_summary)

#########################################
# Define cost rates for energy and solar feed-in
peak_rate <- 0.55         # Peak rate (in $/kWh)
shoulder_rate <- 0.2061620 # Shoulder rate (in $/kWh)
off_peak_rate <- 0.33     # Off-peak rate (in $/kWh)
solar_feed_in_rate <- 0.045 # Solar feed-in rate (in $/kWh)

# Calculate savings from solar battery usage
tesla_battery_solar_saving <- sum(monthly_summary$battery_energy_fm_solar) * peak_rate / 1000
print(tesla_battery_solar_saving)

# Calculate the saving per months (assuming 9 months)
tesla_battery_solar_saving_per_months <- tesla_battery_solar_saving / 9
print(tesla_battery_solar_saving_per_month)

# Calculate time-of-use savings (based on grid energy usage and average rate)
average_rate <- mean(c(shoulder_rate, off_peak_rate)) # Calculate average of shoulder and off-peak rates
time_of_use_saving <- sum(monthly_summary$battery_energy_fm_grid) * (peak_rate - average_rate) / 1000
print(time_of_use_saving)

# Calculate the saving per months (assuming 7 months)
time_of_use_saving_per_month <- time_of_use_saving / 7
print(time_of_use_saving_per_month)
