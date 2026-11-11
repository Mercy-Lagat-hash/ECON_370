
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


###########
cate_hist <- ggplot(CATE_df, aes(x = CATE)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "white"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of Estimated Treatment Effects",
    x = "Estimated CATE",
    y = "Number of Households"
  ) +
  theme_minimal()

cate_hist

ggsave(
  "cate_histogram.pdf",
  plot = cate_hist,
  width = 7,
  height = 5
)
#######



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

## FIGURES ---------------------------------------------------------

# Figure 2: Variable importance
imp_measure$variable <- factor(imp_measure$variable,
                               levels = rev(imp_measure$variable))

fig_imp <- ggplot(imp_measure, aes(x = importance, y = variable)) +
  geom_col(fill = "steelblue") +
  labs(x = "Variable importance", y = NULL,
       title = "Predictors of treatment effect heterogeneity") +
  theme_minimal(base_size = 13)
fig_imp
ggsave("Figure2_variable_importance.png", fig_imp, width = 8, height = 4.5, dpi = 300)

# Figure 3: Distribution of CATEs, by gov worker at baseline
CATE_df$group <- ifelse(Data_Households$BL_GvtChw == 1,
                        "Gov worker in 2010", "No gov worker in 2010")

fig_cate <- ggplot(CATE_df, aes(x = CATE, fill = group)) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Estimated treatment effect (CATE)", y = "Households", fill = NULL,
       title = "Estimated treatment effects by baseline government presence") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
fig_cate
ggsave("Figure3_CATE_histogram.png", fig_cate, width = 8, height = 4.5, dpi = 300)

## TABLE: Causal forest results ------------------------------------

ate_gov   <- average_treatment_effect(cf, subset = Data_Households$BL_GvtChw == 1)
ate_nogov <- average_treatment_effect(cf, subset = Data_Households$BL_GvtChw == 0)

cf_table <- data.frame(
  Estimate = c("ATE, all households",
               "ATE, villages with gov worker in 2010",
               "ATE, villages without gov worker in 2010",
               "ATE, above-median CATE",
               "ATE, below-median CATE",
               "Calibration: mean forest prediction",
               "Calibration: differential forest prediction"),
  Coefficient = c(ATE["estimate"], ate_gov["estimate"], ate_nogov["estimate"],
                  ate_high["estimate"], ate_low["estimate"],
                  cf_test[1, 1], cf_test[2, 1]),
  Std_Error   = c(ATE["std.err"], ate_gov["std.err"], ate_nogov["std.err"],
                  ate_high["std.err"], ate_low["std.err"],
                  cf_test[1, 2], cf_test[2, 2])
)
cf_table[, 2:3] <- round(cf_table[, 2:3], 3)

cf_table
write.csv(cf_table, "Table3_causal_forest.csv", row.names = FALSE)
