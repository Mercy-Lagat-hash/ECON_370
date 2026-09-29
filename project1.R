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
wdi2018 <- wdi2018 %>%
  rename(
    country = `Country Name`,
    isocode = `Country Code`,
    gdp_pc2018 = `GDP per capita (current US$)`,
    population = `Population, total`,
    tourist_arrivals = `International tourism, number of arrivals`,
    quality_service = `Government Effectiveness - Governance score (0-100)`,
    internet = `Individuals using the Internet (% of population)`,
    trade = `Net trade in goods (BoP, current US$)`,
    life_expectancy = `Life expectancy at birth, total (years)`
  )

# STEP 5: load the ILO employment data, and build gender split percentages ----------

# First load the raw file
emp_raw <- read_csv("EMP_TEMP_SEX_AGE_NB_A-filtered-2026-09-29.csv", show_col_types = FALSE)

# This file has THREE rows per country (Total, Male, Female) - actually only
# Total and Female are present (Male isn't given directly, we calculate it).
# We pull out the Total employment numbers first:
emp_total <- emp_raw %>%
  filter(sex.label == "Total") %>%
  select(country = ref_area.label, year = time, total_employment_thousands = obs_value)

# Then we pull out the Female employment numbers separately:
emp_female <- emp_raw %>%
  filter(sex.label == "Female") %>%
  select(country = ref_area.label, year = time, female_employment_thousands = obs_value)

# Now we join Total and Female together into one table, matching on country + year.
# left_join() keeps every row from emp_total, and adds the matching female number.
emp_temp <- emp_total %>%
  left_join(emp_female, by = c("country", "year"))

# Now calculate the percentages:
# - female_pct = what share of total employment is female
# - male_pct = whatever is left over (100 - female_pct)
emp_temp <- emp_temp %>%
  mutate(
    female_pct = (female_employment_thousands / total_employment_thousands) * 100,
    male_pct = 100 - female_pct
  )


# STEP 6: fix country name mismatches between the two files -------------------------

# The two data sources sometimes spell country names differently
# (e.g. ILO says "Cape Verde", WDI says "Cabo Verde"). We fix the ILO names
# to match WDI's spelling, one at a time, using case_when().
# case_when() just checks each condition in order and replaces the value if it matches.
emp_temp <- emp_temp %>%
  mutate(country = case_when(
    country == "Bolivia, Plurinational State of"                      ~ "Bolivia",
    country == "Cape Verde"                                           ~ "Cabo Verde",
    country == "Curaçao"                                              ~ "Curacao",
    country == "Egypt"                                                ~ "Egypt, Arab Rep.",
    country == "Gambia"                                               ~ "Gambia, The",
    country == "Hong Kong, China"                                     ~ "Hong Kong SAR, China",
    country == "Iran, Islamic Republic of"                            ~ "Iran, Islamic Rep.",
    country == "Kyrgyzstan"                                           ~ "Kyrgyz Republic",
    country == "Macau, China"                                         ~ "Macao SAR, China",
    country == "Republic of Korea"                                    ~ "Korea, Rep.",
    country == "Republic of Moldova"                                  ~ "Moldova",
    country == "Saint Lucia"                                          ~ "St. Lucia",
    country == "Slovakia"                                             ~ "Slovak Republic",
    country == "Türkiye"                                              ~ "Turkiye",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    country == "United States of America"                             ~ "United States",
    country == "Venezuela, Bolivarian Republic of"                    ~ "Venezuela, RB",
    TRUE ~ country   # if none of the above match, just keep the name as-is
  ))

# Check which country names STILL don't match anything in WDI.
# (Taiwan, Palestine, and Wallis and Futuna aren't in WDI at all, so they'll show up here - that's expected.)
emp_temp %>%
  filter(!country %in% wdi_wide$country_name) %>%
  distinct(country)


# STEP 7: merge the two datasets together --------------------------------------------

# inner_join() keeps only the rows where country + year match in BOTH data frames.
merged <- wdi_2018_complete %>%
  inner_join(emp_temp, by = c("country_name" = "country", "year"))

glimpse(merged)   # shows all the columns and a preview of the data
nrow(merged)      # how many country-rows made it into the final merged dataset

View(merged)


##Removing countries with missing data


