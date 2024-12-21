# Load required libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(hms)
library(lubridate)
library(gridExtra)



# 18-12-2024

day <- "2024-11-15"
month <- "11"
year <- "2024"

file_directory <- "C:/Project_R/tesla_download/"
  
  
# power
  
df_power <- read.csv(paste0(file_directory,"power/",day,".csv"))
  
  
# energy

df_energy <- read.csv(paste0(file_directory,"energy/",year,"-",month,".csv"))
  

# soe

df_soe <- read.csv(paste0(file_directory,"soe/",day,".csv"))



#########################
# plot df energy 
# across each day 
# usage from battery 
# vs 
# export to solar
# create summary 
