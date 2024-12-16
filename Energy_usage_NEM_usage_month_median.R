# Load required libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(hms)
library(lubridate)

# Define a function to group rows from files based on specified column values
group_rows_from_files <- function(file_paths, column_index, group_values) {
  combined_data <- data.frame()
  
  for (file_path in file_paths) {
    data <- read.csv(file_path, header = FALSE)[, 1:291]
    combined_data <- rbind(combined_data, data)
  }
  
  combined_data$Group_Column <- combined_data[[column_index]]
  combined_data$Group <- NA
  
  current_group <- NA
  for (i in seq_len(nrow(combined_data))) {
    if (combined_data$Group_Column[i] %in% group_values) {
      current_group <- combined_data$Group_Column[i]
    }
    combined_data$Group[i] <- current_group
  }
  
  grouped_data <- split(combined_data, combined_data$Group)
  grouped_data <- lapply(grouped_data, function(df) {
    df$Group <- NULL
    df$Group_Column <- NULL
    return(df)
  })
  
  return(grouped_data)
}

# File paths
file_paths <- c(
  "C:/Project_R/Tesla_battery/Tesla_battery/NEM_older_history_SAPN_DETAILED.csv",
  "C:/Project_R/Tesla_battery/Tesla_battery/NEM_06DEC2024_SAPN_DETAILED.csv"
)

# Group data
grouped_data <- group_rows_from_files(file_paths, column_index = 4, group_values = c("E1", "B1"))

group_E1 <- grouped_data[["E1"]]  # Usage from grid
group_B1 <- grouped_data[["B1"]]  # Solar generation

# Filter rows and clean column names
filtered_group_E1 <- group_E1 %>% filter(V1 == 300)
filtered_group_B1 <- group_B1 %>% filter(V1 == 300)

new_colnames <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/Colname_directory.csv")
if (ncol(new_colnames) >= 2) {
  colnames(filtered_group_E1) <- new_colnames[[2]]
  colnames(filtered_group_B1) <- new_colnames[[2]]
} else {
  stop("The 'new_colnames' dataset does not have at least two columns.")
}

# Convert columns to numeric and dates
filtered_group_E1[, 3:290] <- lapply(filtered_group_E1[, 3:290], as.numeric)
filtered_group_B1[, 3:290] <- lapply(filtered_group_B1[, 3:290], as.numeric)

filtered_group_E1$date <- as.Date(as.character(filtered_group_E1$date), format = "%Y%m%d")
filtered_group_B1$date <- as.Date(as.character(filtered_group_B1$date), format = "%Y%m%d")

########################################################

month_d <- 12
year_y <- 2023



################################################
# Filter data for March and clean the data
df_usage_day <- filtered_group_E1 %>%
  filter(month(date) == month_d & year(date) == year_y) %>%
  select(-date, -NEM_code, -DataQualifyFlag) %>%  # Exclude these columns
  gather(key = "time_slot", value = "usage_value") %>%
  mutate(usage_value = as.numeric(usage_value))  # Ensure usage_value is numeric

# Convert time_slot to a time object using hm()
df_usage_day <- df_usage_day %>%
  mutate(time_slot = hm(time_slot))  # Convert to time object in HH:MM format

# If you need numeric time for sorting, convert time object to total minutes
df_usage_day <- df_usage_day %>%
  mutate(time_slot_numeric = as.numeric(time_slot) / 60)  # Convert to total minutes

# Calculate the median usage value for each time slot across all days
df_median_usage <- df_usage_day %>%
  group_by(time_slot_numeric) %>%
  summarise(median_usage = median(usage_value, na.rm = TRUE))  # Calculate the median usage for each time slot

# Calculate the median usage value for each time slot across all days
df_avg_usage <- df_usage_day %>%
  group_by(time_slot_numeric) %>%
  summarise(avg_usage = mean(usage_value, na.rm = TRUE))  # Calculate the median usage for each time slot

# Plot the median usage values across time slots
ggplot(df_median_usage, aes(x = time_slot_numeric, y = median_usage)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = paste0("Median Usage from Grid Across Time Slots  month ",month_d , "   year  ", year_y),
    x = "Time Slot",
    y = "Median Usage Value (kWh)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(0, max(df_median_usage$time_slot_numeric), by = 30),  # Show every 30th minute (or adjust as needed)
    labels = function(x) sprintf("%02d:%02d", floor(x / 60), x %% 60)  # Format time in HH:MM
  )


# Plot the mean usage values across time slots
ggplot(df_avg_usage, aes(x = time_slot_numeric, y = avg_usage)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = paste0("Mean Usage from Grid Across Time Slots  month ",month_d , "   year  ", year_y),
    x = "Time Slot",
    y = "Mean Usage Value (kWh)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(0, max(df_avg_usage$time_slot_numeric), by = 30),  # Show every 30th minute (or adjust as needed)
    labels = function(x) sprintf("%02d:%02d", floor(x / 60), x %% 60)  # Format time in HH:MM
  )


# lets do box plot 
# Create a box plot for usage values across time slots
ggplot(df_usage_day, aes(x = as.factor(time_slot_numeric), y = usage_value)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  labs(
    title = paste0("Distribution of Usage from Grid Across Time Slots (Month ", month_d, ", Year ", year_y, ")"),
    x = "Time Slot (HH:MM)",
    y = "Usage Value (kWh)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Rotate x-axis labels for better readability
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(
    labels = function(x) sprintf("%02d:%02d", floor(as.numeric(x) / 60), as.numeric(x) %% 60),  # Format time in HH:MM
    breaks = seq(0, max(df_avg_usage$time_slot_numeric), by = 30),  # Show every 30th minute (or adjust as needed)
  )


# suppress outlier
ggplot(df_usage_day, aes(x = as.factor(time_slot_numeric), y = usage_value)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  labs(
    title = paste0("Distribution of Usage from Grid Across Time Slots (Month ", month_d, ", Year ", year_y, ")"),
    x = "Time Slot (HH:MM)",
    y = "Usage Value (kWh)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(
    labels = function(x) sprintf("%02d:%02d", floor(as.numeric(x) / 60), as.numeric(x) %% 60),
    breaks = seq(0, max(df_avg_usage$time_slot_numeric), by = 30),  # Show every 30th minute (or adjust as needed)
  ) 
  #ylim(0, 0.35)  # Set Y-axis limits
