## RCT Project 
#install.packages("haven")
#install.packages("fixest")
setwd("/Users/mercylagat/Documents/ECON_370/Data and Do-Files")
library(haven)
library(dplyr)
library(fixest)
data_path <-"/Users/mercylagat/Documents/ECON_370/Data and Do-Files/Data_Households.dta"

Data_Households <- read_dta("Data_Households.dta")

required_var <- c(
  "EL_care_GvtChw",
  "BL_care_GvtChw",
  "treat",
  "BL_GvtChw",
  "BL_Dist_PrivClinic10",
  "BL_HH_number",
  "area_code",
  "village_codeU"
)

setdiff(required_var, names(Data_Households))

village_controls <- c(
  "BL_Dist_PrivClinic10",
  "BL_HH_number"
)

for (x in village_controls) {
  
  Data_Households[[paste0(x, "_c")]] <-
    Data_Households[[x]] - mean(Data_Households[[x]], na.rm = TRUE)
  
  Data_Households[[paste0(x, "_c_gov")]] <-
    Data_Households[[paste0(x, "_c")]] * Data_Households$BL_GvtChw
}

##basic ancova
ancova_basic <- feols(
  EL_care_GvtChw ~
    treat_BL_GvtChw +
    treat_BL_GvtChwNOT +
    BL_GvtChw +
    BL_care_GvtChw +
    BL_Dist_PrivClinic10_c +
    BL_Dist_PrivClinic10_c_gov +
    BL_HH_number_c +
    BL_HH_number_c_gov |
    area_code,
  
  data = Data_Households,
  vcov = ~village_codeU
)

##expanded version 
ancova_expanded <- feols(
  EL_care_GvtChw ~
    treat_BL_GvtChw +
    treat_BL_GvtChwNOT +
    BL_GvtChw +
    BL_care_GvtChw +
    
    # Other baseline healthcare use
    BL_care_Hosp +
    BL_care_PrivHospClin +
    BL_care_DrugStore +
    
    # Household characteristics
    BL_Age_Head +
    BL_AbovePrimary +
    BL_farmer +
    BL_Number_Infants +
    
    # Baseline health behavior
    BL_health_index +
    
    # Village healthcare availability
    BL_Dist_Hosp10 +
    BL_TradHealer +
    BL_DrugStore +
    
    # Village economic conditions
    BL_index_wealth +
    
    # Original village controls
    BL_Dist_PrivClinic10_c +
    BL_Dist_PrivClinic10_c_gov +
    BL_HH_number_c +
    BL_HH_number_c_gov |
    
    area_code,
  
  data = Data_Households,
  vcov = ~village_codeU
)

summary(ancova_expanded)
summary(ancova_basic)

##comparing the two 
etable(
  ancova_basic,
  ancova_expanded,
  headers = c("Basic ANCOVA", "Expanded ANCOVA")
)
#sample size for the two 
nobs(ancova_basic)
nobs(ancova_expanded)
#se for the two
se(ancova_basic)
se(ancova_expanded)