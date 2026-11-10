# ECON 370: Project 2
# Standard Difference-in-Means Specification
# Paper: The Impact of NGO-Provided Aid on Government Capacity


# Step 0: Load packages ------------------------------------------------
# Install packages (only need to do this once)
install.packages("dplyr")
install.packages("fixest")
install.packages("haven")

# Load packages
library(dplyr)
library(fixest)
library(haven)
setwd("/Users/rediet/Econ 370/Project 2")

# Load the household dataset
Data_Households <- read_dta("Data_Households.dta")

# Step 1: Prepare the data --------------------------------------------

# Keep only villages with a government health worker in 2010
# Remove observations with missing outcome, treatment, or village ID

data <- Data_Households %>%
  filter(BL_GvtChw == 1,
         !is.na(EL_care_GvtChw),
         !is.na(treat))


# Step 2: Calculate treatment and control means -----------------------

# Calculate average government healthcare receipt in 2012
# treat = 1: NGO treatment group
# treat = 0: control group

group_means <- data %>%
  group_by(treat) %>%
  summarise(
    mean_healthcare = mean(EL_care_GvtChw),
    observations = n(),
    .groups = "drop"
  )

# Display treatment and control means
print(group_means)


# Step 3: Calculate the difference in means ---------------------------

# Treatment effect = treatment mean - control mean

difference <- group_means$mean_healthcare[group_means$treat == 1] -
  group_means$mean_healthcare[group_means$treat == 0]

# Display the treatment effect
print(difference)
