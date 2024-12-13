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
# df_NEM_supply <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/NEM_older_history_SAPN_DETAILED.csv", check.names = TRUE)


# read and clean

# seperate the data solar and usage

#B1 B1 solar_generation
#E1 E1 usage_from_grid - export from SA powernetwork 

group_rows_from_files <- function(file_paths, column_index, group_values) {
  # Initialize an empty list to hold all rows
  combined_data <- data.frame()
  
  # Load and append data from each file, limiting to the first 291 columns
  for (file_path in file_paths) {
    # Load the data without a header
    data <- read.csv(file_path, header = FALSE)
    
    # Limit to first 291 columns
    data <- data[, 1:291]
    
    # Add data to the combined data frame
    combined_data <- rbind(combined_data, data)
  }
  
  # Identify the specified column for grouping (e.g., column 4)
  combined_data$Group_Column <- combined_data[[column_index]]
  
  # Create a new column for grouping
  combined_data$Group <- NA  # Initialize the Group column
  
  # Assign groups based on changes in the specified column
  current_group <- NA
  for (i in seq_len(nrow(combined_data))) {
    if (combined_data$Group_Column[i] %in% group_values) {
      current_group <- combined_data$Group_Column[i]
    }
    combined_data$Group[i] <- current_group
  }
  
  # Split the data into groups based on the 'Group' column
  grouped_data <- split(combined_data, combined_data$Group)
  
  # Remove the temporary 'Group' and 'Group_Column' columns from each group
  grouped_data <- lapply(grouped_data, function(df) {
    df$Group <- NULL
    df$Group_Column <- NULL
    return(df)
  })
  
  return(grouped_data)
}
# Example Usage
# Specify the file paths, column index (4th column), and group values (e.g., "E1" and "B1")
file_path_1 <- "C:/Project_R/Tesla_battery/Tesla_battery/NEM_older_history_SAPN_DETAILED.csv"
file_path_2 <- "C:/Project_R/Tesla_battery/Tesla_battery/NEM_06DEC2024_SAPN_DETAILED.csv"
file_paths <- c(file_path_1, file_path_2)
grouped_data <- group_rows_from_files(file_paths, column_index = 4, group_values = c("E1", "B1"))

# Access specific groups
group_E1 <- grouped_data[["E1"]] # usage from grid
group_B1 <- grouped_data[["B1"]] # solar generation


# Filter only with 300
filtered_group_E1 <- group_E1 %>% filter(V1 == 300)
filtered_group_B1 <- group_B1 %>% filter(V1 == 300)


# change col name
col_names_1 <- as.data.frame(colnames(filtered_group_E1))

new_colnames <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/Colname_directory.csv")

# Ensure the second column of new_colnames is used
if (ncol(new_colnames) >= 2) {
  # Assign the second column as the new column names of filtered_group_E1 & B1
  colnames(filtered_group_E1) <- new_colnames[[2]]
  colnames(filtered_group_B1) <- new_colnames[[2]]
} else {
  stop("The 'new_colnames' dataset does not have at least two columns.")
}

# Verify the updated column names
# print(colnames(filtered_group_E1))


# testign 123


