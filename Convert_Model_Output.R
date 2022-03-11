######################################################
#convert output from model to megajoules/m2 Parrallel
######################################################
graphics.off()
rm(list = ls())
options(error=stop)

library(pracma)
library(doParallel)
library(parallel)
library(reshape2)
library(data.table)
library(lubridate)
library(tidyverse)

tic()

##########
#Paths
##########
setwd(dir = "C:/Doctoraat/Agroforestry/Paper_1_LIDAR/Simulation_Scenes/output")

for (folders in c("alder_young_mid","alder_medium_mid", "alder_old_mid",
                  "birch_young_mid","birch_medium_mid", "birch_old_mid",
                  "oak_young_late","oak_medium_late","oak_old_late",
                  "alder_young_late","alder_medium_late", "alder_old_late",
                  "birch_young_late","birch_medium_late", "birch_old_late")){
  
outpath <- paste0("./Model_OUTPUT_02_UPDATE/DATA_2020/",folders) #directory to save converted output to

###############
#Needed files
###############
convfactors <- readRDS("./convfactors2048.rds") #the conversion factors
Diffuse_tree <- fread(paste0("./Model_OUTPUT_01_UPDATE/diffuse_light_", folders,".csv"))
Direct_tree <- fread(paste0("./Model_OUTPUT_01_UPDATE/sunlight_",folders,".csv"))

#####################
#Alterations to files
#####################
convfactors[sapply(convfactors, is.infinite)] <- NaN
convfactors[sapply(convfactors, is.nan)] <- 0

Diffuse_tree <- Diffuse_tree[rep(seq_len(nrow(Diffuse_tree)), each = 24), ] #Diffuse output only has values for every day, change to values for every hour

################################################################################
#Multiply output of model with conversion factors and sum diffuse and direct
################################################################################
DIR <- sapply(Direct_tree[,3:ncol(Direct_tree)], `*`, convfactors$ConvFactorDirect)
DIFF <- sapply(Diffuse_tree[,2:ncol(Diffuse_tree)], `*`, convfactors$ConvFactorDiffuse)

tot <- DIFF + DIR  

##################################################
#Write away data to txt files per hour and per day
##################################################
cl <- makeCluster(6, type='PSOCK') 
registerDoParallel(cl) 

foreach (i=1:8784) %dopar% { #for each hour convert output 8784
  library(tidyverse)
  mat <- matrix(tot[i,], 120, 90) %>% 
    t() %>%
    as.data.frame()
  write.table(mat, file = paste(outpath, "/day_", ceiling(1/24*i),"hour_",i - ceiling(1/24*i)*24+24, ".txt",sep = ""))
}

stopCluster(cl)
}
toc()