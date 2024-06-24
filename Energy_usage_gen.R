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

#investigate 10/6
#investigate 13/6
#read excel _
# excel already sightly pre modified 
df_NEM_gen <- read.xlsx("C:/Project_R/Tesla_battery/Tesla_battery/NEM_generation.xlsx",check.names = TRUE)
df_NEM_supply <- read.xlsx("C:/Project_R/Tesla_battery/Tesla_battery/NEM_supply.xlsx",check.names = TRUE)

################################################################
#filter out 200 only need 300
df_NEM_gen_300 <- df_NEM_gen %>% filter(NEM12 == 300)
df_NEM_supply_300 <- df_NEM_supply %>% filter(NEM12 == 300)

#remove column the last and type
df_NEM_gen_300_c <- df_NEM_gen_300[,!names(df_NEM_gen_300) %in% c("QulityMethod","Unknown")]
df_NEM_supply_300_c <- df_NEM_supply_300[,!names(df_NEM_supply_300) %in% c("QulityMethod","Unknown")]

#must be numeric
df_NEM_gen_300_c$time_0005 <- as.numeric(df_NEM_gen_300_c$time_0005)
df_NEM_supply_300_c$time_0005 <- as.numeric(df_NEM_supply_300_c$time_0005)

#change column Date to datetime
df_NEM_gen_300_c$Date <- as.character(df_NEM_gen_300_c$Date)
df_NEM_supply_300_c$Date <- as.character(df_NEM_supply_300_c$Date)

df_NEM_gen_300_c$Date <- as.Date(df_NEM_gen_300_c$Date,tryFormats = c("%Y%m%d"))
df_NEM_supply_300_c$Date <- as.Date(df_NEM_supply_300_c$Date , tryFormats = c("%Y%m%d"))

#rowSums
row_sums <- rowSums(df_NEM_gen_300_c[, 3:ncol(df_NEM_gen_300_c)], na.rm = TRUE)
df_NEM_gen_300_c$row_sum <- row_sums

row_sums <- rowSums(df_NEM_supply_300_c[, 3:ncol(df_NEM_supply_300_c)], na.rm = TRUE)
df_NEM_supply_300_c$row_sum <- row_sums
####################################################################################
#Check NEM supply usage 
specific_date <- as.Date("2024-06-13")
filtered_df <- df_NEM_supply_300_c %>% filter(Date == specific_date)
f_row_sum <- filtered_df$row_sum
print("Filtered Data Frame (specific date):")
print(f_row_sum)
########################################################################################

#consider usage part
#filter months with tesla battery installed
#only consider March April May
tsla_March <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/March_2024.csv",encoding ="UTF-8")
tsla_April <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/April_2024.csv",encoding ="UTF-8")
tsla_May <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/May_2024.csv",encoding ="UTF-8")
tsla <- bind_rows(tsla_March,tsla_April,tsla_May)
########################################################################################
#clean up tsla










tsla_summary <- tsla %>% 
  mutate(month = floor_date(Date.time)) %>% 
  group_by(month) %>% 
  summarise(
    tsla_app_usage = sum(From.Grid..kWh., na.rm = TRUE)
  )



### monthly summary for usage partk

monthly_summary <- df_NEM_supply_300_c %>% 
  mutate(month = floor_date(Date,"month")) %>% #extract the month
  group_by(month) %>% 
  summarise(
    usage_value = sum(row_sum,na.rm = TRUE)
  )
  
monthly_summary <- monthly_summary %>% 
  mutate(month_year = format(month, "%b%y"))
print(monthly_summary)
# put in from March 24 o May 24
df_NEM_invetigation <- monthly_summary %>% filter(month_year %in% c("Mar23","Apr24","May24"))

                                                  