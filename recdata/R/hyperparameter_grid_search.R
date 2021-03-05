library(tidyverse)
library(gridExtra)
library(tidymodels)
library(lubridate)
library(readxl)
library(ggcorrplot)
library(knitr)
library(rpart.plot)
library(randomForestExplainer)
library(doParallel)
library(vip)


#  Load pollock data, get rid of NAs and log transformed all count data.
datwide_r <- read_excel("EBS_Pollock/Data/Data_with_lags_survey.xlsx",sheet="Sheet1") %>% 
  filter(!is.na(recruits) & year>=1980) %>% 
  #dplyr::select(-c(year)) %>% 
  mutate(across(c(recruits,n1_3,n2_2,n3_1,atf_3,pcod_3,sole_3,atf_2,pcod_2,sole_2,atf_1,pcod_1,sole_1,atf,pcod,sole),log)) %>% 
  filter(!is.na(n1_3)) %>% dplyr::select(-n3_1)

#create a rolling resample
ebs_rolling <- rolling_origin(datwide_r, initial =10)

# Set-up a grid for mtry and min_n.
mygrid <- expand.grid(mtry=2:35,min_n=2:45)

#  Specify the model for the mtry and min_n grid search. Hold trees constant.
tune_spec <- rand_forest(mtry = tune(),
                         trees = 500,
                         min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

#  Model formula
mod_rec <- recipe(recruits~.,data=datwide_r)

#  Setup parallel processing
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

#  Run the grid search (this is the function that takes time)
tune_res <- tune_grid(
  tune_spec,
  mod_rec,
  resamples = ebs_rolling,
  grid = mygrid
)

#  Save it so you never have to do that again.
tune_res %>% 
  saveRDS("Hyperparameter_grid.RDS")

#  Plot the contour of hyperparameters with their RMSE
png("hyperparameter_gridsearch_contour.png",width=6,height=6,units="in",res=300)
temp %>%
  ggplot() +
  geom_tile(aes(x=mtry,y=min_n,fill=mean)) +
  geom_contour(aes(x=mtry,y=min_n,z=mean),color="black") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_gradientn("RMSE",colours = terrain.colors(10))
dev.off()

#  Plot without contour lines, hyperparameters with their RMSE
png("hyperparameter_gridsearch_nocontour.png",width=6,height=6,units="in",res=300)
temp %>%
  ggplot() +
  geom_tile(aes(x=mtry,y=min_n,fill=mean)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_gradientn("RMSE",colours = terrain.colors(10))
dev.off()

#  Extract the best hyperparamters based on RMSE
select_best(tune_res,"rmse")

tune_res %>%
  collect_metrics() %>% 
  filter(.metric=="rmse") %>% 
  arrange(mean)

#  Now use the best mtry and min_n values to see what effect the number of trees has.
treevec <- seq(100,10000,by=100)

#  Model spec with our best mtry and min_n, searching over the vector of trees
tune_spec <- rand_forest(mtry = 34,
                         trees = tune(),
                         min_n = 20) %>%
  set_engine("ranger") %>%
  set_mode("regression")

#  Probably don't need to call this again but doesn't seem to hurt.
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

#  Run grid search over all tree numbers
tune_res <- tune_grid(
  tune_spec,
  mod_rec,
  resamples = ebs_rolling,
  grid = treevec
)

# Save it!
tune_res %>% 
  saveRDS("tree_gridsearch.RDS")

#  How's it look?
tune_res %>%
  collect_metrics() %>% 
  filter(.metric=="rmse") %>% 
  arrange(mean) %>% 
  ggplot(aes(trees,mean)) + 
  geom_point()


