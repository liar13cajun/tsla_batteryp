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

#9pm hot water using 
# 7-8pm cooking


#read excel _
# excel already sightly pre modified 
# https://www.aemo.com.au/Electricity/National-Electricity-Market-NEM/Retail-and-metering/-/media/EBA9363B984841079712B3AAD374A859.ashx
df_NEM_gen <- read.xlsx("C:/Project_R/Tesla_battery/Tesla_battery/Nem_gen_July.xlsx",check.names = TRUE)  #B1 B1
df_NEM_supply <- read.xlsx("C:/Project_R/Tesla_battery/Tesla_battery/Nem_supply_July.xlsx",check.names = TRUE) #E1 E1

################################################################
#filter out 200 only need 300
df_NEM_gen_300_c <- df_NEM_gen %>% filter(NEM12 == 300)
df_NEM_supply_300_c <- df_NEM_supply %>% filter(NEM12 == 300)

# #remove column the last and type
# df_NEM_gen_300_c <- df_NEM_gen_300[,!names(df_NEM_gen_300) %in% c("QulityMethod","Unknown")]
# df_NEM_supply_300_c <- df_NEM_supply_300[,!names(df_NEM_supply_300) %in% c("QulityMethod","Unknown")]

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
specific_date <- as.Date("2024-07-23")
filtered_df <- df_NEM_supply_300_c %>% filter(Date == specific_date)
f_row_sum <- filtered_df$row_sum
print("Filtered Data Frame (specific date):")
print(f_row_sum)
########################################################################################
#consider usage part
##investigate 10/6
#investigate 13/6

#home
tsla_240723 <- read.csv("C:/Project_R/Tesla_battery/Tesla_battery/20240723_tsla.csv",encoding ="UTF-8")


tsla_240723 <- rownames_to_column(tsla_240723)

colnames(tsla_240723)[1] <- "ID"

tsla_240723$Home..kW.<-tsla_240723$Home..kW./10
tsla_240723$Powerwall..kW.<-tsla_240723$Powerwall..kW./10
tsla_240723$Solar..kW.<- tsla_240723$Solar..kW./10
tsla_240723$Grid..kW.<- tsla_240723$Grid..kW./10


########################################################################################
#clean up tsla
#PICK THE SINGLE DAY

specific_date <- as.Date("2024-07-23")
specified_df <- df_NEM_supply_300_c %>% filter(Date == specific_date)
#subset
specified_df_x <- specified_df[,3:ncol(specified_df)] #remove first to row
specified_df_x_t <- t(specified_df_x) # transpose
specified_df_x_t <- specified_df_x_t[-nrow(specified_df_x_t),] # remove row sum
df_xx <- as.data.frame(specified_df_x_t) # back to dataframe

df_xx <- rownames_to_column(df_xx)
colnames(df_xx)[1] <- "time_xxxx"
df_xx <- rownames_to_column(df_xx)
colnames(df_xx)[1] <- "ID"
##########################################################


### change here
df_join <- merge(tsla_240723,df_xx)
df_join$ID <- as.numeric(df_join$ID)
df_join <- df_join[order(df_join$ID, decreasing = FALSE),]



####################################################


# Extract text between 'T' and '+'
df_join$extracted_time <- str_extract(df_join$Date.time, "(?<=T).*?(?=\\+)")
# df_join$extracted_time <- as_datetime(df_join$extracted_time) 
df_join$Date.time <- as_datetime(df_join$Date.time) 

df_join$local_time <- with_tz(df_join$Date.time, tz = "Australia/Adelaide")
# Convert UTC to Adelaide time (ACST)
# datetime_adelaide <- with_tz(datetime_utc, tz = "Australia/Adelaide")

str(df_join)
#####################################################
ggplot()+
  geom_line(data = df_join, aes(x = local_time      , y= Grid..kW., color = "tsla")) +
  geom_line(data = df_join, aes(x = local_time      , y= specified_df_x_t, color = "NEM"))+
  labs(x = "time", y = "kWh", title = "2024-07-23")
#######################################################
#tsla home
tsla_home <- sum(tsla_240723$Home..kW.)
tsla_powerwall <- sum(tsla_240723$Powerwall..kW.)
tsla_solar <- sum(tsla_240723$Solar..kW.)
tsla_grid <- sum(tsla_240723$Grid..kW.)




#############
#difference between grid
print(tsla_grid)
print(f_row_sum - tsla_grid)
print(f_row_sum)