##Caleb

library(haven)
library(tidyverse)
library(glmnet)
library(hdm)
library(fastDummies)
library(broom)

setwd("/Users/calebphillips/Downloads/ECON PROJECT 2")
Household_data <- read_dta("Data_Households.dta")
Baseline_data <- read_dta("Data_Households_BL.dta")

##Step 1
Y <- Household_data$EL_care_GvtChw
var(Y)

Household_data$area_code <- factor(Household_data$area_code)
Household_data$village_codeU <- factor(Household_data$village_codeU)

##X <- Household_data[, -c(1,3,7, 23:38, 59:63, 71)]
##X <- as.matrix(X)

X <- as.data.frame(
  Household_data[, -c(1, 3, 7, 23:38, 59:63, 71)]
)

## Step 2
ols_data <- data.frame(Y = Y, X)
model <- lm(Y ~ ., data = ols_data)



##Step 2
##model <- lm(Y ~ X)
ols_results <- tidy(model)
ols_results

rss_ols <- sum(residuals(model)^2)
rss_ols

significant_vars <- filter(ols_results,
                           term != "(Intercept)",
                           p.value < 0.05)
significant_vars


##Step 3

###
# Keep rows where the outcome is observed
keep <- !is.na(Y)

X_clean <- as.data.frame(X[keep, , drop = FALSE])
Y_clean <- Y[keep]

# Remove any columns that are entirely missing
X_clean <- X_clean[
  , !vapply(X_clean, function(z) all(is.na(z)), logical(1)),
  drop = FALSE
]

# Impute missing predictors and create dummy variables
X_imputed <- makeX(X_clean, na.impute = TRUE)

# Confirm there are no missing values
sum(is.na(X_imputed))

# Fit logistic lasso
lasso_low <- glmnet::glmnet(
  X_imputed, Y_clean,
  alpha = 1,
  lambda = 0.001,
  family = "binomial"
)
###



lasso_low <- glmnet(X_imputed, Y_clean, alpha = 1, lambda = 0.001)
lasso_low$beta

lasso_high <- glmnet(X_imputed, Y_clean, alpha = 1, lambda = 1)
lasso_high$beta

lasso_path <- glmnet(X_imputed, Y_clean, alpha = 1)
min(lasso_path$lambda[lasso_path$df == 0])




