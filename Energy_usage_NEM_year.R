# Get data NEM 
# plot across usage
#library set up
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(sf)
library(janitor)
library(data.table)
library(lubridate)
library(patchwork)

# Read the data from the CSV file
df_NEM_supply <- read.csv("D:/Wakelin/Tesla battery_doc/NEM_supply_summary_06DEC2024_combine_stable.csv", check.names = TRUE)

# Remove the first four columns
df_NEM_supply <- df_NEM_supply[, -c(1:4)]

# Rename the columns
colnames(df_NEM_supply) <- c("start_date", "end_date", "usage", "x0", "solar_generation","max_demand","unit")

# Remove the first four columns
df_NEM_supply <- df_NEM_supply[, -c(4,7)]

# Preview the cleaned-up data
head(df_NEM_supply)

# Convert start_date to Date type for proper plotting
df_NEM_supply$start_date <- as.Date(df_NEM_supply$start_date, format = "%d/%m/%Y")

# Create a long format dataframe for better ggplot compatibility
library(reshape2)
# Convert start_date to Date type for proper plotting
df_NEM_supply$start_date <- as.Date(df_NEM_supply$start_date, format = "%d/%m/%Y")
# Calculate the global y-axis range
y_min <- min(df_NEM_supply$usage, df_NEM_supply$solar_generation, na.rm = TRUE)
y_max <- max(df_NEM_supply$usage, df_NEM_supply$solar_generation, na.rm = TRUE)

# Plot for Usage
p1 <- ggplot(df_NEM_supply, aes(x = start_date, y = usage)) +
  geom_line(color = "blue", size = 1) +
  scale_y_continuous(limits = c(y_min, y_max)) +
  labs(
    title = "Usage Over Time",
    x = "Date",
    y = "Units"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot for Solar Generation
p2 <- ggplot(df_NEM_supply, aes(x = start_date, y = solar_generation)) +
  geom_line(color = "green", size = 1) +
  scale_y_continuous(limits = c(y_min, y_max)) +
  labs(
    title = "Solar Generation Over Time",
    x = "Date",
    y = "Units"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine the plots side by side
combined_plot <- p1 + p2

# Print the combined plot
print(combined_plot)
