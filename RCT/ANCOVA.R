## RCT Project 
#install.packages("haven")
#install.packages("fixest")
setwd("/Users/mercylagat/Documents/ECON_370/Data and Do-Files")
library(haven)
library(dplyr)
library(fixest)
data_path <-"/Users/mercylagat/Documents/ECON_370/Data and Do-Files/Data_Households.dta"
Data_Households <- read_dta("Data_Households.dta")

##IMPUTING (baseline variables only; drop rows with missing outcome)
Data_Households <- Data_Households %>% filter(!is.na(EL_care_GvtChw))

bl_missing <- names(Data_Households)[startsWith(names(Data_Households), "BL_") &
                                       colSums(is.na(Data_Households)) > 0]
for (v in bl_missing) {
  Data_Households[[paste0(v, "_miss")]] <- as.numeric(is.na(Data_Households[[v]]))
  Data_Households[[v]][is.na(Data_Households[[v]])] <- mean(Data_Households[[v]], na.rm = TRUE)
}

required_var <- c(
  "EL_care_GvtChw",
  "BL_care_GvtChw",
  "treat",
  "area_code",
  "village_codeU"
)

setdiff(required_var, names(Data_Households))

##ANCOVA
ancova <- feols(
  EL_care_GvtChw ~ treat + BL_care_GvtChw | area_code,
  data = Data_Households,
  vcov = ~village_codeU
)

summary(ancova)

#sample size
nobs(ancova)
#se
se(ancova)
