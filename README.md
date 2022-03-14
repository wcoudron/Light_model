# Light_model
Convert light model output from agroforestry.ugent.be

## Requirements
R (version 4.1.2)  
Rstudio (version 1.2)
R packages doParrallel, parallel, reshape2, data.table, lubridate, tidyverse, pracma, rstudioapi

## practical guide
1. Perform light simulations on agroforestry.ugent.be for an empty scene.
2. Generate conversion factors based on real light measurements and model output from agroforestry.ugent.be for an empty scene. (Make_Conversion_Factors.R)
3. Convert output from the light model with conversion factors generated in step 2. (Convert_Model_Output.R)
