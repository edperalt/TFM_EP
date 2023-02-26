

###### variables escogidas por un geologo   #############

## librerias

library(readr)
library(flextable)
library(tidyverse)
library(tidymodels)
library(h2o)


## Los datos

xcore_train <- read_rds("data/modelos/00_xcore_train.rds") 


## las variables a utilizar

las_var <- read_rds("data/var_geo.rds")



xcore_train <- xcore_train %>% 
  dplyr::select(lith_name,"re_dtc", "re_rhob_clean", "re_gr", "re_log_RSHA","re_sp",
                "re_log_RMED" ,"re_z" , "re_nphi","log_RDEP" , "pef_clean", "CALI" )


## comenzamos h2o 

h2o.init(nthreads=-1)


train <- as.h2o(xcore_train)

start_time <- Sys.time()  


aml <- h2o.automl(x = 2:40,y=1,training_frame = train,
                  max_models = 20,
                  seed = 1,
                  keep_cross_validation_predictions=TRUE)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

write_rds(aml, "data/modelos/04_aml_h2o_geo_full_mod.rds")



h2o.shutdown()

