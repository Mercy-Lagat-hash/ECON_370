## ECON 370 Project 1:  EXPLORATORY DATA ANALYSIS 
## NAME:  Mercy, Rediet, Caleb
## TOPIC: Exploring Patterns in Tourism, Economic Development, 
##and Employment in the Tourism Sector Across Countries
##Cleaning 

##Retaining only real countries 
##install.packages("countrycode")
##library(countrycode)
datapath<- "/Users/mercylagat/Downloads/Exploratory_Project_data/WDI_project1_data.csv" ##renamed the csv file
wditourism <- read_csv(datapath)
## uses the country codes in the countrycode library to retain valid countries. 
wditourism <- wditourism %>%
  filter(!is.na(countrycode(`Country Code`, "iso3c", "country.name")))


##Removing other years except 2018, and relabels the missing data with N/A
wdi2018 <- wditourism %>% select(`Country Name`, `Country Code`, `Series Name`, `2018 [YR2018]`) %>%
  mutate(`2018 [YR2018]` = as.numeric(na_if(`2018 [YR2018]`, "..")))

##Reshaping to have variables as columns in the data

wdi2018 <- wdi2018 %>% pivot_wider(names_from = `Series Name`, values_from = `2018 [YR2018]`)

## Renaming variables
wdi2018 <- 


##Removing countries with missing data


