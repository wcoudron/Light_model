#requirements: 64 bit R
graphics.off()
rm(list = ls())
options(error=stop)

library(rstudioapi)
library(tidyverse)
library(data.table)
library(lubridate)
library(pracma)

tic()
setwd(dir = getActiveDocumentContext()$path %>% dirname())

source("Spitters.R")

#timeresolution for output values
res <- 3600 #in seconds

#10 minutely global radiation file starting the 21st of december 
rad <- fread(file = "./Data_Global_Radiation/Data_Willem.csv", sep=";") %>% 
  dplyr::select(TIMESTAMP, Radiation)

#######################################
#Convert 10 minutely values to hourly
#######################################
rad <- rad %>% 
  dplyr::mutate(TIMESTAMP = TIMESTAMP - 3600, #change winter summer time
                day = yday(TIMESTAMP),
                hour = hour(TIMESTAMP)+1,
                `time (s)` = hour(TIMESTAMP)*3600
                ) %>%
  dplyr::group_by(day , hour, `time (s)`) %>%
  dplyr::summarise(Radiation = sum(Radiation)*600/1e6)

################################
#Parameters for spitters modules
################################
lat <- 50.5
RAD <- pi/180 #radialen
lon <- 3.8
timezone <- 1 
winsumT <- 1 #summer (1) or wintertime (0)
timeinterval <- 1 #in uur

#######################################################################################
#For every global radiation value, calculate the amount of diffuse and direct radiation
#######################################################################################
diffdir <- rad %>%
  dplyr::group_by(day, hour,`time (s)`) %>%
  dplyr::mutate(dfdr = calcDiffAndDirectHourly(timeinterval, lon, lat, winsumT, timezone, day, hour, Radiation),
                diff = calcSpittersDiff(timeinterval, lon, lat, winsumT, timezone, day, hour, Radiation),
                dir = (1-dfdr)*diff/dfdr
                )

#############################################################################
#take means of sensors for direct component to have a value for every hour
#############################################################################
#reference simulation files
Direct_Reference <- fread(file = "./output/Direct_Reference1024.csv", sep = ",") %>%
  dplyr::rowwise(day, `time (s)`) %>%
  dplyr::summarise(DirectReference = rowMeans(across(starts_with("S"))))

RefDir <- Direct_Reference %>% 
  dplyr::mutate(day = ifelse(day>355, day-366, day)+11) #align model output with radiation data


combined <- RefDir %>% left_join(diffdir, by = c("day", "time (s)"))

################################################################
#same for diffuse radiation (all values same actually)
################################################################
Diffuse_Reference <- fread(file = "./output/Diffuse_Reference1024.csv", sep = ",")%>%
  dplyr::rowwise(day) %>%
  dplyr::summarise(mean = rowMeans(across(starts_with("S"))))

DiffuseReference <- mean(Diffuse_Reference$mean)

#####################################################################################
#Calculate conversion factors to change output values from toon light model and save
#####################################################################################
combined <- combined %>%
  dplyr::mutate(ConvFactorDiffuse = diff/DiffuseReference,
                ConvFactorDirect = dir/DirectReference)
toc()
saveRDS(combined, "C:/Doctoraat/Agroforestry/Paper_1_LIDAR/Simulation_Scenes/output/convfactors1024.rds")
