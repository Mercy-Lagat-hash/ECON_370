## Caleb: Post-double-selection lasso

library(haven)
library(glmnet)
library(hdm)

setwd("/Users/calebphillips/Downloads/ECON PROJECT 2")
Household_data <- read_dta("Data_Households.dta")

## 1. Define outcome and treatment

Y <- as.numeric(Household_data$EL_care_GvtChw)
D <- as.numeric(Household_data$treat)

# Check that treatment is coded 0/1
stopifnot(all(na.omit(D) %in% c(0, 1)))

## 2. Select candidate baseline controls

# Review this list to confirm all are measured before treatment
baseline_vars <- grep(
  "^BL_",
  names(Household_data),
  value = TRUE
)

print(baseline_vars)

X <- as.data.frame(Household_data[, baseline_vars])

## 3. Keep rows with observed outcome AND treatment

keep <- !is.na(Y) & !is.na(D)

Y_clean <- Y[keep]
D_clean <- D[keep]
X_clean <- X[keep, , drop = FALSE]

# Remove entirely missing or constant controls
usable <- vapply(
  X_clean,
  function(z) length(unique(z[!is.na(z)])) > 1L,
  logical(1)
)

X_clean <- X_clean[, usable, drop = FALSE]

## 4. Impute missing predictors and encode categorical controls

# Convert any numeric categorical controls to factors before this step
X_imputed <- makeX(X_clean, na.impute = TRUE)

# Remove any constant columns after preprocessing
nonconstant <- apply(X_imputed, 2, function(z) var(z) > 0)
X_imputed <- X_imputed[, nonconstant, drop = FALSE]

stopifnot(
  nrow(X_imputed) == length(Y_clean),
  length(Y_clean) == length(D_clean),
  all(is.finite(X_imputed))
)

## 5. Fit post-double-selection lasso

set.seed(123)

pds_model <- rlassoEffect(
  x = X_imputed,
  y = Y_clean,
  d = D_clean,
  method = "double selection",
  post = TRUE
)

## 6. Report the treatment effect

summary(pds_model)
confint(pds_model)

pds_model$coefficients.reg

