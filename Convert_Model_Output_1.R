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

timestep <- 1 #timestep of the simulation in hours 

fieldsizex <- 120 #fieldsize in the x direction
fieldsizey <- 90 #fieldsize in the y direction

tic()

##########
#Paths
##########
simulpath <- "C:/Doctoraat/Agroforestry/Paper_1_LIDAR/Manuscript/OPNIEUW/Leaf_parameters/" #path with output files of website simulations and conversion factors

for (folders in c("folder")){ #give here the names of the sunlight en diffuse light output files of the simulation model, the name of the folder where this output is saved should also be this name
  
outpath <- paste0(simulpath,folders) #directory to save converted output to, for every timestep a different text file is generated

###############
#Needed files
###############
convfactors <- readRDS(paste0(simulpath, "convfactors.csv")) #the conversion factors generated in the R script "Make_conversion_factors"
Diffuse_tree <- fread(paste0(simulpath, "diffuse_light_", folders,".csv")) #the diffuse simulation file with trees
Direct_tree <- fread(paste0(simulpath, "sunlight_",folders,".csv")) #the direct simulation file with trees

#####################
#Alterations to files
#####################
convfactors[sapply(convfactors, is.infinite)] <- NaN
convfactors[sapply(convfactors, is.nan)] <- 0

Diffuse_tree <- Diffuse_tree[rep(seq_len(nrow(Diffuse_tree)), each = 24/timestep), ] #Diffuse output only has values for every day, change to values for every timestep (1 hour in this case)

################################################################################
#Multiply output of model with conversion factors and sum diffuse and direct
################################################################################
DIR <- sapply(Direct_tree[,3:ncol(Direct_tree)], `*`, convfactors$ConvFactorDirect)
DIFF <- sapply(Diffuse_tree[,2:ncol(Diffuse_tree)], `*`, convfactors$ConvFactorDiffuse)

tot <- DIR

##################################################
#Write away data to txt files per hour and per day
##################################################
cl <- makeCluster(6, type='PSOCK')  #Code for parallelization (making use of more cores for calculation of output to improve speed)
registerDoParallel(cl) 

foreach (i=1:(24/timestep*366)) %dopar% { #for each timestep convert output
  library(tidyverse)
  mat <- matrix(tot[i,], fieldsizex, fieldsizey) %>% 
    t() %>%
    as.data.frame()
  write.table(mat, file = paste(outpath, "/day_", ceiling(1/(24/timestep)*i),"timestep_",i - ceiling(1/(24/timestep)*i)*(24/timestep)+(24/timestep), ".txt",sep = ""))
}

stopCluster(cl)
}
toc()
