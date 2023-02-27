
  

library(flextable)
library(tidymodels)
library(tidyverse) 
library(xgboost)



xcore <- read_rds("data/xcore.rds") %>% dplyr::select(-c(fm, bit_size))
xcore$lith_name <- as.factor(xcore$lith_name) 

xcore <- xcore %>% 
  relocate(lith_name, .before = well)


set.seed(123) 
xcore_split <- initial_split(xcore, prop = 0.7, strata = lith_name) # 70% training, 30% testing

# extract the training and testing sets
xcore_tr <- training(xcore_split)
xcore_te <-   testing(xcore_split)

xcore_train <- xcore_tr %>%  dplyr::select(-well)
xcore_test <- xcore_te %>%  dplyr::select(-well)

write_rds(xcore_train, "data/modelos/00_xcore_train.rds")
write_rds(xcore_test, "data/modelos/00_xcore_test.rds")

las_metricas <- metric_set(roc_auc, accuracy,kap,sens)

#### XG boost  #########

los_datos <- xcore_train %>% select(-c(x_loc, y_loc, z_loc,DEPT))

boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), 
             trees = tune(), 
             learn_rate = tune(), 
             min_n = tune(), 
             loss_reduction = tune(), 
             sample_size = tune(), 
             stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')

base_rec <-
  recipe(lith_name ~ . , data = los_datos ) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv() %>% 
  step_corr(all_numeric_predictors(), threshold = .7) 

xg_workflow<-
  workflow() %>% 
  add_recipe(base_rec) %>% 
  add_model(boost_tree_xgboost_spec) 


wflow_param <-
  xg_workflow %>% 
  extract_parameter_set_dials() %>% 
  finalize(los_datos)



#### Hyperparametros ###############

wflow_param %>% extract_parameter_dials("tree_depth")
wflow_param %>% extract_parameter_dials("trees")
wflow_param %>% extract_parameter_dials("learn_rate")
wflow_param %>% extract_parameter_dials("min_n")
wflow_param %>% extract_parameter_dials("loss_reduction")
wflow_param %>% extract_parameter_dials("sample_size")
wflow_param %>% extract_parameter_dials("stop_iter")



###### Tuning xg.  ###########
set.seed(123)

xg_param<- xg_workflow %>% 
  extract_parameter_set_dials() %>% 
  grid_regular(levels = 3)

step_fold <- vfold_cv(los_datos, v=5, repeats = 5, strata = lith_name)

xg_initial <-
  xg_workflow %>%
  tune_grid(resamples = step_fold, grid = xg_param , metrics = las_metricas )

write_rds(xg_initial, "data/modelos/05_tuned_xg2.rds")



#### XG boost todo PCA #########

los_datos <- xcore_train %>% select(-c(x_loc, y_loc, z_loc,DEPT))



boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')


base_rec <-
  recipe(lith_name ~ . , data = los_datos ) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv() %>% 
  step_corr(all_numeric_predictors(), threshold = .7) %>% 
  step_pca(num_comp = tune()) %>% 
  step_normalize()


xg_workflow<-
  workflow() %>% 
  add_recipe(base_rec) %>% 
  add_model(boost_tree_xgboost_spec) 


wflow_param <-
  xg_workflow %>% 
  extract_parameter_set_dials() %>% 
  finalize(los_datos)



#### Hyperparametros ###############

wflow_param %>% extract_parameter_dials("tree_depth")
wflow_param %>% extract_parameter_dials("trees")
wflow_param %>% extract_parameter_dials("learn_rate")
wflow_param %>% extract_parameter_dials("min_n")
wflow_param %>% extract_parameter_dials("loss_reduction")
wflow_param %>% extract_parameter_dials("sample_size")
wflow_param %>% extract_parameter_dials("stop_iter")
wflow_param %>% extract_parameter_dials("num_comp")


###### Tuning xg2  ###########
set.seed(123)

xg_param<- xg_workflow %>% 
  extract_parameter_set_dials() %>% 
  update(
    num_comp = num_comp(c(0, 40))
  ) %>% 
  grid_regular(levels = 3)


step_fold <- vfold_cv(los_datos, v=5, repeats = 5, strata = lith_name)

xg_initial <-
  xg_workflow %>%
  tune_grid(resamples = step_fold, grid = xg_param , metrics = las_metricas )

write_rds(xg_initial, "data/modelos/05_tuned_xg3.rds")

