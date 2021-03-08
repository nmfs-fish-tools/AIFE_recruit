#  Load pollock survey data
datwide_r <- read_excel("EBS_Pollock/Data/Data_with_lags_survey.xlsx",sheet="Sheet1") %>% 
  filter(!is.na(recruits) & year>=1980) %>% 
  #dplyr::select(-c(year)) %>% 
  mutate(across(c(recruits,n1_3,n2_2,n3_1,atf_3,pcod_3,sole_3,atf_2,pcod_2,sole_2,atf_1,pcod_1,sole_1,atf,pcod,sole),log)) %>% 
  filter(!is.na(n1_3)) #%>% dplyr::select(-n3_1)

#  Create rolling splits
ebs_rolling <- rolling_origin(datwide_r, initial =20)

#  Rolling_origin fitting and testing function from Christine
fit_splits <- function(mtry = 3, min_n=27,trees=500, split){
  analysis_set_rf <- analysis(split)

#  Assess a random forest  
  model <-
    rand_forest(mtry = mtry, trees = trees,mode="regression") %>%
    set_engine("ranger") %>%
    fit(recruits ~ ., data = analysis_set_rf %>% dplyr::select(-year))

#  For the same splits, assess a linear model
  model2 <-
    linear_reg(mode="regression") %>%
    set_engine("lm") %>%
    fit(recruits ~ n3_1+strata, data = analysis_set_rf)
  
  assessment_set_rf <- assessment(split)
  
  # return(assessment_set_rf)
  return( assessment_set_rf %>%
             select(recruits, year) %>%
             mutate(.pred = unlist(predict(model, new_data = assessment_set_rf)), # Extract RF predictions
                    .predlm = unlist(predict(model2, new_data = assessment_set_rf))) %>% # Extract LM predictions
             select(recruits, year, .pred,.predlm))
}

#Fit the splits & calculate errors
model_results <-
  map_df(.x = ebs_rolling$splits,
         ~fit_splits(mtry = 27,min_n=10, trees = 1000, split = .x)) %>% 
  mutate(error=(recruits-.pred)/recruits,
         errorlm=(recruits-.predlm)/recruits,
         errorlm10=(exp(recruits)-exp(.predlm))/exp(recruits),
         error10=(exp(recruits)-exp(.pred))/exp(recruits))

model_results %>% 
  summarise(mean(error),
            mean(errorlm),
            mean(error10),
            mean(errorlm10))


png("Model_predictions.png",width=10,height=8,units="in",res=300)
model_results %>% 
  dplyr::select(year,RF=.pred,LM_roll=.predlm,Age4=recruits) %>% 
  gather(Model,fish,-c(year)) %>% 
  mutate(fish=exp(fish)) %>% 
  ggplot(aes(year,fish/10^9,col=Model)) + 
  geom_line() +
  geom_point(data=. %>% filter(Model=="Age4"),aes(year,fish/10^9,color=Model),size=2) +
  theme(legend.position=c(0.2,0.8)) +
  theme_bw() +
  ylab("Age-4 pollock (billion)") + 
  xlab("Year") + 
  theme(legend.position=c(0.2,0.75),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12))
dev.off()