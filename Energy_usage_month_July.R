#monthly differ
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


#read excel _
# excel already sightly pre modified 
# https://www.aemo.com.au/Electricity/National-Electricity-Market-NEM/Retail-and-metering/-/media/EBA9363B984841079712B3AAD374A859.ashx
# df_NEM_gen <- read.xlsx("C:/Project_R/Tesla_battery/Tesla_battery/Nem_gen_July.xlsx",check.names = TRUE)  #B1 B1
# df_NEM_supply <- read.xlsx("C:/Project_R/Tesla_battery/Tesla_battery/Nem_supply_July.xlsx",check.names = TRUE) #E1 E1

# 
# tsla_March <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/March_2024.csv",encoding ="UTF-8")
# tsla_April <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/April_2024.csv",encoding ="UTF-8")
# tsla_May <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/May_2024.csv",encoding ="UTF-8")
tsla_July <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/July_2024.csv",encoding ="UTF-8")
# tsla_May <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/July_2024.csv",encoding ="UTF-8")

tsla <- tsla_July
########################################################################################
#clean up tsla


tsla$Date.time <- as.Date(tsla$Date.time)

tsla_summary <- tsla %>% 
  mutate(month = floor_date(Date.time,"month")) %>% 
  group_by(month) %>% 
  summarise(
    tsla_from_grid = sum(From.Grid..kWh.,na.rm= TRUE)
  )
tsla_July_clean <- tsla
tsla_summary <- tsla_summary %>% 
  mutate(month_year_tsla = format(month, "%b%y"))
print(tsla_summary)
tsla_summary$tsla_from_grid <- tsla_summary$tsla_from_grid 




#######################


######################

EA_July <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/EA-jul-2024-usage.csv",encoding ="UTF-8")
EA_July$READ.DATE <- as.Date(EA_July$READ.DATE, format = "%d %B %Y")

#############

df_both <- cbind(EA_July,tsla)
###################               



# Create a bar chart
ggplot(df_both, aes(x = Date.time, y = CONSUMPTION.KWH.)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Daily Consumption in July 2024", x = "Date", y = "Consumption (kWh)") +
  theme_minimal()



# Sample data: Assuming df_both already has the relevant columns
# For demonstration purposes, let's say there's another column called 'Another.Column'
df_both$tsla_from_grid <- tsla$From.Grid..kWh.  # Example additional column

# Reshape the data into a long format
df_long <- df_both %>%
  select(Date.time, CONSUMPTION.KWH., tsla_from_grid) %>%
  gather(key = "Variable", value = "Value", -Date.time)

# Create the bar chart
ggplot(df_long, aes(x = Date.time, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Daily Consumption and Tsla usage in July",
       x = "Date", y = "Value (kWh)") +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  theme_minimal()
