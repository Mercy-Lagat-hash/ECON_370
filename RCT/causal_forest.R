
## CAUSAL FOREST


setwd("/Users/mercylagat/Documents/ECON_370/Data and Do-Files")
library(grf)
library(fixest)
library(tidyverse)
library(fastDummies)
library(haven)

# step 1: load and prep EMERGE data --------------------------------------------

## load the data set ECON370-lab9-cf-data.csv directly from github
## convert the enumerator and strata variables to category dummies
## drop observations missing the outcome variable, story_comp
## impute other missing values 
data_path <-"/Users/mercylagat/Documents/ECON_370/Data and Do-Files/Data_Households.dta"

Data_Households <- read_dta("Data_Households.dta")
Data_Households <- Data_Households %>%
  select(
    -`_ENDLINE_HOUSEHOLD_DATA_______`,
    -`_BASELINE_VILLAGE_DATA_______`,
    -`_ENDLINE_VILLAGE_DATA_______`,
    -`_NGO_WORKERS_XTICS_______`,
    -`_BASELINE_HOUSEHOLD_DATA_______`,
    -`_MORE_______`
  )
# Define variables and matrices needed for causal forest ---------------

Y <- as.vector(Data_Households$EL_care_GvtChw)
sum(is.na(Y))
W <- as.vector(Data_Households$treat)
village <- as.integer(Data_Households$village_codeU)


## 3d. define covars as your clean data frame minus the variables above...
## ... and define X as a matrix version of covars
covars <- Data_Households[, c(
  "BL_GvtChw",            
  "BL_scarce",
  "BL_care_GvtChw",       
  "BL_care_Hosp",         
  "BL_Dist_PrivClinic10", 
  "BL_Dist_Hosp10",       
  "BL_HH_number",         
  "BL_index_wealth"       
)]
X <- as.matrix(zap_labels(covars))
str(covars)
## 3e. define W_hat and Y_hat, your predicted values of treatment and Y

W_hat <- ave(W, Data_Households$area_code)


##causal forest
cf <- causal_forest(X, Y, W, W.hat = W_hat,
                    clusters = village, tune.parameters = "all", seed = 8675309)

ATE<- average_treatment_effect(cf,target.sample= "all")

#CATE
CATE <- predict(cf)$predictions
CATE_df <- data.frame(CATE = as.vector(CATE))
# Compare regions with high and low estimated CATEs
high_effect = CATE > median (CATE)
ate_high = average_treatment_effect ( cf , subset = high_effect )
ate_low = average_treatment_effect( cf , subset = !high_effect )


#Calibration
cf_test <- test_calibration(cf)
cf_test
#important predictors
var_imp<- variable_importance(cf)
imp_measure<- data.frame(variable=names(covars), importance = var_imp)

imp_measure <- imp_measure %>%
  arrange(desc(importance))

head(imp_measure, 5)
##Results
ATE
ate_high
ate_low

