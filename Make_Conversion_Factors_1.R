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
######################################
#files
######################################
setwd(dir = getActiveDocumentContext()$path %>% dirname())
source("Spitters.R") #code from spitters to convert output

path <- "path" #insert path with simulation files and global radiation data

radfile <- paste0(path, "Data_Willem.csv") #10 minutely global radiation file starting the 21st of December, 1 column contains the time, the other column the radiation in W/m2
DirectRef <- paste0(path, "sunlight_reference.csv") #saved file with direct light output values from simulation for an empty scene
DiffuseRef <- paste0(path, "diffuse_light_reference.csv") #saved file with diffuse light output values from simulation for an empty scene

rad <- fread(file = radfile, sep=";") %>% 
  dplyr::select(TIMESTAMP, Radiation)

################################
#Parameters for spitters modules
################################
lat <- 51
RAD <- pi/180 #radialen
lon <- 3.8
timezone <- 1 
winsumT <- 1 #summer (1) or wintertime (0)
timeinterval <- 1 #in hours

######################################
#Convert 10 minutely values to hourly
######################################
rad <- rad %>% 
  dplyr::mutate(TIMESTAMP = TIMESTAMP - 3600, #change winter summer time
                day = yday(TIMESTAMP),
                hour = hour(TIMESTAMP)+1,
                `time (s)` = hour(TIMESTAMP)*3600
                ) %>%
  dplyr::group_by(day ,hour,`time (s)`) %>%
  dplyr::summarise(Radiation = sum(Radiation)*600/1e6)

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
Direct_Reference <- fread(file = DirectRef, sep = ",") %>%
  dplyr::rowwise(day, `time (s)`) %>%
  dplyr::summarise(DirectReference = rowMeans(across(starts_with("S"))))

RefDir <- Direct_Reference %>% 
  dplyr::mutate(day = ifelse(day>355, day-366, day)+11) #align model output with radiation data


combined <- RefDir %>% left_join(diffdir, by = c("day", "time (s)"))

################################################################
#same for diffuse radiation (all values same actually)
################################################################
Diffuse_Reference <- fread(file = DiffuseRef, sep = ",")%>%
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

#save output
saveRDS(combined, paste0(path, "convfactors.csv"))
