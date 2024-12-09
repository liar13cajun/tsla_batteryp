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


# Load the data
data <-  read.csv("C:/Project_R/Tesla_battery/Tesla_battery/NEM_older_history_SAPN_DETAILED.csv", header = FALSE)


# Rename columns for clarity
colnames(data) <- paste0("col_", seq_len(ncol(data)))

# Identify column D (4th column)
data$Group_Column <- data$col_4

# Create a group identifier based on changes in column D (E1 or B1)
data$Group <- NA  # Initialize the Group column

# Fill the Group column based on the last non-NA value in Group_Column
current_group <- NA
for (i in seq_len(nrow(data))) {
  if (data$Group_Column[i] %in% c("E1", "B1")) {
    current_group <- data$Group_Column[i]
  }
  data$Group[i] <- current_group
}

# Split data into two groups based on the 'Group' column
group_E1 <- subset(data, Group == "E1")
group_B1 <- subset(data, Group == "B1")
