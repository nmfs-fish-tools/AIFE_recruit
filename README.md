# README

# (AIFE_recruit) AI for Earth Recruitment Forecasting

**************

https://nmfs-fish-tools.github.io/AIFE_recruit/

**************

## Project Description
1. Questions to study
2.  Approach

## Project Outline

1.  Data Inputs
- further focus data types and sets from data sources below
2.  Data Prep and Inital Analysis
- possible ML algorithms for clustering within this seciton
3. Data Viz from previous
4.  Test vs Scoring Data Set Choices
5.  Modeling
6.  Scoreing Model
7. Evaluation of Models
8. Reporting



## Notes to lead to Outline
- Sort through RAM data sets
- Decide on variables
    - predator prey?
    - areaid?
- Do we want to poll species
  -  use K-means clustering as its own possibility for relationshops between datasets
  (other clustering algs: https://towardsdatascience.com/the-5-clustering-algorithms-data-scientists-need-to-know-a36d136ef68)
  - Time series clustering and classification:
    - http://www.rdatamining.com/examples/time-series-clustering-classification
    - Dynamic Time Warping: http://dtw.r-forge.r-project.org/
  - look for shared trends
- Environmental Data
  - do we want to use large scale and/or local environmental [indicies](https://psl.noaa.gov/data/climateindices/list/)
    - Consider time lag when using large scale climate indices?
  - sst/other physical ocean variables on area scale?
    - Chlorophyll a concentrations (food environment) if our focus is larvae or juveniles? 
    - Limit the number of environmental variables? Adding one more environmental variable into the model needs at least 15 more observation points to support the analysis.
- Time Series issues:
  - https://machinelearningmastery.com/time-series-forecasting-supervised-learning/
  - https://machinelearningmastery.com/backtest-machine-learning-models-time-series-forecasting/
  - use a step to cluster time series such as Kendall Seasonal trend analysis or similar

- analysis and clean choice data sets
    - set of Pacific data sets to compare to steig et al
- add tidymodels to DSVM if not there already
- develop multiple models
    - one for data pooling
    - for comparisons
      - Models with/without consideration of environmental variables?
      - Models that identify linear/nonlinear relationships between recruits and environmental variables?
    - for recruitment forecasting
      - Compare recruit forecasting from using observed recruits and using the stock assessment recruits output from RMA legacy database?
      - Jordan looking at gradient-boosting models for pink salmon using tidymodels package in R, uses age 1 outmigrating pinks
        - temperature at point in the sound , shelf break, and central GOA from satellite data over month
- What metrics to use? RMSE, MAE, R squared? 


## Project TODO's and milestones

- Look into larval data

- 6-7 data sets for west coast and alaska (sablefish, petrale, hake, pollock, pink salmon), compare to more traditional approaches, summarize the data.

## Data sources

- RAM legacy database - https://zenodo.org/record/3676088#.Xl7943BRf3g

- Extracted recruitment time series - https://drive.google.com/open?id=1_9cBLjmFbKmAm6x5__GUGLIMiVYsnlEZ

- EcoFOCI program - Bering Sea climate data and ichthyoplankton https://www.ecofoci.noaa.gov/data-links

- EcoFOCI program has data from larval survey in GOA, goes back to 70's - targets pollock
    - eco data like temp, chlorophyll, known gaps in chlorophyll coverage
    
- Bering Sea BASIS survey - targets age-0 pollock

- SECM - southeast coastal monitoring of pink salmon - which spatial extents and temporal lags are important (includes zooplankton data); https://www.fisheries.noaa.gov/alaska/sustainable-fisheries/forecasting-pink-salmon-harvest-southeast-alaska

- many gappy years 

- akfin/pacfin data - Jordan is working on integrating environmental and fishery dependent data in the AKFIN/PacFIN database backends for easier modeling access by end users.

- Calcofi ichthyoplankton ERDAP: https://coastwatch.pfeg.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=calcofi

- Puget sound herring: this is an egg survey

- Comparison to different efforts to predict recruitment on west coast stuff - petrale sole, hake, sablefish

## References

Rethinking forecasting: https://docs.google.com/document/d/1ob1zexI_558hzpEd12casTVtOsQnTnpEXCQmkaCy0z8/edit?usp=sharing



**************



## Disclaimer

“The United States Department of Commerce (DOC) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.”

- This project code is made available through GitHub but is managed by NOAA at
 https://vlab.ncep.noaa.gov/redmine/projects/integrated-fisheries-toolbox/files

***** *******
