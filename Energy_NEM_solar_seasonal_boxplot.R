# Load required libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(hms)
library(lubridate)
library(gridExtra)

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

group_E1 <- grouped_data[["E1"]]  # solar from grid
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

year_y <- 2024

# duck curve significant 
# summer Dec Jan Feb
# autumn Mar Apr May
# winter Jun Jul Aug
# Spring sep oct nov 


################################################
# Define month groups
month_groups <- list(
  "Dec-Jan-Feb" = c(12, 1, 2),
  "Mar-Apr-May" = c(3, 4, 5),
  "Jun-Jul-Aug" = c(6, 7, 8),
  "Sep-Oct-Nov" = c(9, 10, 11)
)
# Define a list to store plots
plots <- list()

# Iterate over month groups and generate plots
for (group_name in names(month_groups)) {
  months <- month_groups[[group_name]]
  
  # Filter and clean the data
  df_solar_day <- filtered_group_B1 %>%
    filter(month(date) %in% months & year(date) == year_y) %>%
    select(-date, -NEM_code, -DataQualifyFlag) %>%
    gather(key = "time_slot", value = "solar_value") %>%
    mutate(solar_value = as.numeric(solar_value))  # Ensure solar_value is numeric
  
  df_solar_day$solar_value <- df_solar_day$solar_value * -1 #(flip the graph)
  
  # Convert time_slot to time object and numeric for sorting
  df_solar_day <- df_solar_day %>%
    mutate(
      time_slot = hm(time_slot),  # Convert to time object in HH:MM format
      time_slot_numeric = as.numeric(time_slot) / 60  # Convert to total minutes
    )
  
  # Calculate the median solar value for each time slot across all days
  df_median_solar <- df_solar_day %>%
    group_by(time_slot_numeric) %>%
    summarise(median_solar = median(solar_value, na.rm = TRUE))
  
  # Calculate the median solar value for each time slot across all days
  df_avg_solar <- df_solar_day %>%
    group_by(time_slot_numeric) %>%
    summarise(avg_solar = mean(solar_value, na.rm = TRUE))  # Calculate the median solar for each time slot
  
  # Create a box plot
  p <- ggplot(df_solar_day, aes(x = as.factor(time_slot_numeric), y = solar_value)) +
    geom_boxplot(outlier.shape = NA, fill = "lightblue") +
    labs(
      title = paste0("Solar export to Grid Across Time Slots (", group_name, " Year ", year_y, ")"),
      x = "Time Slot (HH:MM)",
      y = "solar Value (kWh)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
      panel.grid.minor = element_blank()
    ) +
    scale_x_discrete(
      labels = function(x) sprintf("%02d:%02d", floor(as.numeric(x) / 60), as.numeric(x) %% 60),
      breaks = seq(0, max(df_avg_solar$time_slot_numeric), by = 30),  # Show every 30th minute (or adjust as needed)
    ) 
  
  # Add the plot to the list
  plots[[group_name]] <- p
}

# Combine and display the plots using gridExtra
grid.arrange(grobs = plots, nrow = 2, ncol = 2)


############################################################

# Define the months for calculation
months_to_sum <- 1:11

# Filter and clean the data for March to November
df_solar_sum <- filtered_group_B1 %>%
  filter(month(date) %in% months_to_sum & year(date) == year_y) %>%
  select(-date, -NEM_code, -DataQualifyFlag)  # Exclude unnecessary columns

# Convert columns to numeric (if not already)
df_solar_sum[, 3:ncol(df_solar_sum)] <- lapply(df_solar_sum[, 3:ncol(df_solar_sum)], as.numeric)

# Calculate the total solar export
total_solar_export <- sum(df_solar_sum[, 3:ncol(df_solar_sum)], na.rm = TRUE)

# Display the result
print(paste("Total solar export :", total_solar_export, "kWh"))

# solar feed in
solar_feed_in_rate <- 0.045
total_solar_export*solar_feed_in_rate /11 # each month 
