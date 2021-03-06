#' ---
#' title: "A report generated from a pure R script"
#' output:
#'   pdf_document:
#'     keep_tex: true
#' ---
#'
#' This is a report generated by `knitr::spin()`.
#'
#' Let's try some **knitr** options:

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
require(ggplot2)
require(dplyr)
require(ggcorrplot)
require(broom)


data.dir <- "./recdata/data/"
petrale <- read.csv(file.path(data.dir,"Petrale_analyzed.data.csv"))
sable <- read.csv(file.path(data.dir,"Sablefish_Analyzed_Data_north.csv"))
sable_DFA <- read.csv(file.path(data.dir,"Sablefish_DFA_Data.csv"))

names(petrale)

#Plot recruits vs year and spawning stock biomass
plot(age.0~year,data=petrale, type="l", main="Recruits per year")
plot(age.0~sp.bio,data=petrale, main="Recruits vs SSB")


#Plot correlation
round(cor(petrale %>% 
            dplyr::select(names(petrale)[18:37]) %>% data.frame), 1) %>% 
ggcorrplot(tl.cex=6)

modpetrale <- petrale %>%
  select(-c(X,total.bio,depletion,spr,expl.rate,sp.bio.sd,age.0.sd,yrminusone,resids,log.resids,SR_pred,Label,dev,devsd, LSTbjuv.a,CSTbjuv.a,CSTbjuv.b, Tpre.b, DDegg2)) %>%
  pivot_longer(
    cols=-c(year,sp.bio,age.0),
    values_to="covs_val")

#Do regression with different environmental time series
#remove LSTbjuv.a, CSTbjuv, Tpre.b, DDegg2 values since these are perfectly correlated
modpetrale %>% 
  group_by(name) %>%
  do(glance(lm(age.0~sp.bio+covs_val,data=.))) %>%
  bind_rows(modpetrale %>% 
              dplyr::select(-c(covs_val,name)) %>%
              distinct() %>% 
              do(glance(lm(age.0~sp.bio,data=.))) %>%
              mutate(name="sp.bio only")) %>%
  arrange(-adj.r.squared) %>% 
  mutate(across(where(is.double), round, 3)) %>% 
  dplyr::select(-c(r.squared,logLik)) %>% 
  data.frame 

#The best covariates are MLDegg (Mean mixed layer depth) and spawner preconditioning degree-days, none other <.05

wide_petrale <- modpetrale %>%
  pivot_wider(names_from = name, values_from=covs_val) %>%
  select(-c(year))

set.seed(100)
pet_split <- initial_split(wide_petrale) #By default, 3/4 data going into training; 1/4 test
pet_train <- training(pet_split)
pet_test <- testing(pet_split)

pet_split # View the pet_split object.

#Package not working for now, fix later
source("R/fit_split.R")

lm_spec <- linear_reg() %>% 
  set_engine("lm")

#  Use the workflows and tidymodels function from above to fit a model to the training data
lm_fit <- fit_split(age.0~sp.bio+MLDegg, # specify formula
                    model=lm_spec, # specify model
                    split=pet_split) # specify data

lm_fit %>% 
  collect_metrics()

lm_fit %>%
  collect_predictions()

lm_fit %>% 
  collect_predictions %>% # from tune package, predicting using test data
  mutate(resid=(age.0-.pred)^2) %>% #calculate squared residuals
  summarise(rmse=sqrt(mean(resid)))

fit_split(age.0~sp.bio+MLDegg, # specify formula
          model=lm_spec, # specify model
          metrics=metric_set(rmse,mae,mape), #specify model evaluation criteria.
          split=pet_split) %>% 
  collect_metrics()


#Create new split
set.seed(100)
pet_split <- initial_split(wide_petrale)
pet_train <- training(pet_split)
pet_test <- testing(pet_split)

#Look at range of data to see if we need to center/scale
apply(wide_petrale,2,range)

#  Create a recipe.
pet_recipe <- recipe(age.0~.,data=pet_train) %>% 
  step_center(MLDegg) %>% 
  step_scale(MLDegg) 

#  Prep the recipe. 
pet_prep <- pet_recipe %>% 
  prep(training=pet_train,retain=TRUE)

pet_prep


dt_model <- decision_tree(min_n=5,tree_depth=10) %>% 
  set_engine("rpart") %>%  # Specify the R library
  set_mode("regression") %>%  # Specify regression or classification
  fit(age.0~.,data=juice(pet_prep))

rpart.plot::rpart.plot(dt_model$fit,
                       type=4,
                       extra=101,
                       branch.lty=3,
                       nn=TRUE,
                       roundint=FALSE) 

set.seed(100)
pet_split <- initial_split(wide_petrale,prop=0.90)
pet_train <- training(pet_split)

pet_recipe <- recipe(age.0~.,data=pet_train)

pet_prep <- pet_recipe %>% 
  prep(training=pet_train,retain=TRUE)

rf_model <- rand_forest(trees=2000,mtry=4,mode="regression") %>% #rand_forest is a function in parsnip.
  set_engine("ranger",importance="permutation") %>% # rand_forest is part of the ranger package. We have several options for importance measures.
  fit(age.0~.,data=juice(pet_prep))


#library(randomForestExplainer)
impt_frame<-measure_importance(rf_model$fit)

#impt_frame %>% head()
#  I like this plot as a way to illustate how several of the different RF hyperparameters fall out for different features.
plot_multi_way_importance(impt_frame,no_of_labels = 6)


