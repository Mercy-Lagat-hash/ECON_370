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

