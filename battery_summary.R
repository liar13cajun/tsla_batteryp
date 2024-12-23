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

# Loop through each month
for (month in months) {
  # Format month to two digits
  month_str <- sprintf("%02d", month)
  
  # Construct file path and load the data
  file_path <- paste0(file_directory, year, "-", month_str, ".csv")
  df_energy <- read.csv(file_path)
  
  # Calculate the total values for each metric
 
  battery_energy_fm_grid <- sum(df_energy$battery_energy_imported_from_grid, na.rm = TRUE)
  battery_energy_fm_solar <- sum(df_energy$battery_energy_imported_from_solar, na.rm= TRUE)
  usage_fm_battery <- sum(df_energy$consumer_energy_imported_from_battery,na.rm=TRUE)
  usage_fm_solar <- sum(df_energy$consumer_energy_imported_from_solar,na.rm=TRUE)
  solar_to_grid <- sum(df_energy$grid_energy_exported_from_solar,na.rm=TRUE)
  solar_generation <- sum(df_energy$total_solar_generation,na.rm=TRUE)
  
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

#
print(monthly_summary)

#########################################
# average of stuff 
# energy Australia 
# peak rate 0.55 / kwh
# shoulder 0.2061620 / kwh
# off peak 0.33 / kwh

# solar feed in 0.045 kwh

peak_rate <- 0.55
shoulder_rate <- 0.2061620
off_peak_rate <- 0.33
solar_feed_in_rate <- 0.045

summary(monthly_summary$battery_energy_fm_solar)

tesla_battery_solar_saving <- sum(monthly_summary$battery_energy_fm_solar) * peak_rate / 1000
tesla_battery_solar_saving

tesla_battery_solar_saving / 9


time_of_use_saving <- sum(monthly_summary$battery_energy_fm_grid) * (peak_rate - mean(shoulder_rate,off_peak_rate))/1000
time_of_use_saving / 7
