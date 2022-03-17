# Lightmodel Conversion
Convert light model output from <https://agroforestry.ugent.be>

## Requirements
R (version 4.1.2)  
Rstudio (version 1.2)
R packages doParrallel, parallel, reshape2, data.table, lubridate, tidyverse, pracma, rstudioapi

## practical guide
1. Perform light simulations on <https://agroforestry.ugent.be> for an empty scene.
2. Generate conversion factors based on real light measurements and model output from <https://agroforestry.ugent.be> for an empty scene. [Make_Conversion_Factors.R](https://github.com/wcoudron/Lightmodel_Conversion/blob/main/Make_Conversion_Factors.R)
3. Convert output from the light model with conversion factors generated in step 2. [Convert_Model_Output.R](https://github.com/wcoudron/Lightmodel_Conversion/blob/main/Convert_Model_Output.R)
