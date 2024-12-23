# Load required libraries
library(tidyverse)
library(lubridate)
library(readxl)

# Define file directory and months to process
file_directory <- "C:/Project_R/tesla_download/energy/"
months <- 3:11
year <- "2024"

# Initialize a list to store plots
plots <- list()

# Loop through each month, read data, and create plots
for (month in months) {
  # Format the month as two digits (e.g., "03" for March)
  month_str <- sprintf("%02d", month)
  
  # Construct the file path and read the data
  file_path <- paste0(file_directory, year, "-", month_str, ".csv")
  df_energy <- read.csv(file_path)
  
  # Convert timestamp to datetime format
  df_energy$timestamp <- as.POSIXct(df_energy$timestamp, format = "%Y-%m-%d %H:%M:%S")
  df_energy$time <- format(df_energy$timestamp, "%H:%M:%S")
  
  # Create a boxplot for the current month
  p <- ggplot(df_energy, aes(x = time, y = consumer_energy_imported_from_battery)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", outlier.shape = NA) +
    labs(title = paste("Distribution of Consumer Energy Imported from Battery (", month_str, ")", sep = ""),
         x = "Time of Day",
         y = "Battery Energy Usage (kWh)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add the plot to the list
  plots[[month_str]] <- p
}

# Display all plots (or save them)
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))  # Display in a grid (2 columns)


###################################


# Create an empty data frame to store the results
monthly_summary <- data.frame(Month = character(), Total_Imported = numeric(), stringsAsFactors = FALSE)

# Loop through each month
for (month in months) {
  # Format month to two digits
  month_str <- sprintf("%02d", month)
  
  # Construct file path and load the data
  file_path <- paste0(file_directory, year, "-", month_str, ".csv")
  df_energy <- read.csv(file_path)
  
  # Calculate the total imported energy for the month
  total_imported <- sum(df_energy$consumer_energy_imported_from_battery, na.rm = TRUE)
  
  # Append the result to the summary data frame
  monthly_summary <- rbind(monthly_summary, data.frame(Month = paste(year, month_str, sep = "-"), 
                                                       Total_Imported = total_imported))
}

# Print the monthly summary
print(monthly_summary)

