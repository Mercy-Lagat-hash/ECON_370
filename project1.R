## ECON 370 Project 1:  EXPLORATORY DATA ANALYSIS 
## NAME:  Mercy, Rediet, Caleb
## TOPIC: Exploring Patterns in Tourism, Economic Development, 
##and Employment in the Tourism Sector Across Countries
##CLEANING

##Retaining only real countries 

##install.packages("countrycode")
##library(countrycode)
datapath<- "/Users/mercylagat/Downloads/Exploratory_Project_data/WDI_project1_data.csv"
wditourism <- read_csv(datapath)
## uses the country codes in the countrycode libary to retain valid countries. 
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
    net_trade = `Net trade in goods (BoP, current US$)`,
    life_expectancy = `Life expectancy at birth, total (years)`
  )


##Cleaning secondary data
data2path<- "/Users/mercylagat/Downloads/ILOemployement.csv"
emp_total <- read_csv(data2path)

emp_total <- emp_total %>%
  filter(sex.label == "Total") %>%
  select(country = ref_area.label,total_employment_thousands = obs_value)

# Then we pull out the Female employment numbers separately:
emp_female <- ILOempl %>%
  filter(sex.label == "Female") %>%
  select(country = ref_area.label, female_employment_thousands = obs_value)

# Now we join Total and Female together into one table, matching on country + year.
# left_join() keeps every row from emp_total, and adds the matching female number.
emp_temp <- emp_total %>%
  left_join(emp_female, by = c("country"))

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
  filter(!country %in% wdi2018$country) %>%
  distinct(country)


# STEP 7: merge the two datasets together --------------------------------------------

# inner_join() keeps only the rows where country + year match in BOTH data frames.
tourismdata_2018 <- wdi2018 %>%
  inner_join(emp_temp, by = c("country" = "country")) ##china Kosovo, Palestine, State of Taiwan, China, Wallis and Futuna removed

glimpse(tourismdata_2018)   # shows all the columns and a preview of the data
nrow(tourismdata_2018)      # how many country-rows made it into the final merged dataset

View(tourismdata_2018)

tourismdata_2018 <- tourismdata_2018 %>%drop_na(-c())
tourismdata_2018<- tourismdata_2018 %>% mutate(tourism_density = `tourist_arrivals`/`population`)

pca_tourism <- prcomp(tourismdata_2018, scale. = TRUE)
biplot(pca_tourism )
pca_loadings_tourism <- as.data.frame(round(pca_tourism $rotation, digits = 3))
pca_loadings_tourism <- pca_loadings_tourism |>
  mutate(variable = rownames(pca_tourism $rotation)) |>
  relocate(variable, .before = 1)

print(pca_loadings_tourism)

#####PCA ANALSYSIS###
##countries_to_drop <- c("United States", "India")
tourismdata_2018_num <- tourismdata_2018%>%
  ##filter(!country %in% countries_to_drop) %>%
  column_to_rownames("country")%>%
  select(internet,
         net_trade,
         life_expectancy,
         quality_service,
         tourism_density,
         gdp_pc2018,
         total_employment_thousands,
         female_pct)
pca_results <- prcomp(tourismdata_2018_num, scale. = TRUE)
pca_loadings_tourism <- pca_results$rotation %>%
  round(3) %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable")
print(pca_loadings_tourism)
sum(pca_results$rotation[, "PC1"] > 0)
biplot(pca_results, scale = 0)

###KMEANS CLUSTERING###
tourism_scaled <- scale(tourismdata_2018_num)
set.seed(178999)
num_clust <- 4
km_results <- kmeans(tourism_scaled, centers = num_clust, nstart = 20)
sort(km_results$size)
tourismdata_2018$country[km_results$cluster %in% which(km_results$size == 1)]

clust_centers <- round(t(km_results$centers), 3)
print(clust_centers)

plot_data <- as.data.frame(pca_results$x) %>%
  rownames_to_column(var = "country")
plot_data$cluster <- factor(km_results$cluster)
ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Country Clusters on the First Two Principal Components",
    x = "First Principal Component",
    y = "Second Principal Component",
    color = "Cluster"
  ) +
  theme_minimal()

plot_data %>% filter(country == "United States")
plot_data %>% filter(country == "Macao SAR, China")
plot_data %>% filter(country == "India")
plot_data %>% filter(country == "Brazil")
plot_data %>% filter(country == "France")

plot_data %>%
  arrange(cluster, country) %>%
  select(cluster, country) %>%
  View()
##United States has very low PC2 because of population and total employment, PC3 and PC4 low because of female pct
# and PC5 low because of internet and life expectancy.
##India PC1 and PC2 because of employment and population.

##Macao has a very low PC3 and very high PC4 because of its high tourism density


##COMBINING TOURISM WITH CLUSTERS AND PC1
cluster_data <- tibble(country = rownames(tourismdata_2018_num),
                       cluster = factor(km_results$cluster))

pc_density <- pc_density %>%
  left_join(cluster_data, by = "country")

plot1<- ggplot(pc_density, aes(x = PC1, y = tourism_density)) +
  geom_point(aes(color = cluster), size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Tourism Density vs. First Principal Component, by Cluster",
       x = "First Principal Component",
       y = "Tourism Density (Tourists per Resident)",
       color = "Cluster") +
  theme_minimal()

ggsave("tourism_density_vs_pc1.pdf", plot = plot1, width = 8, height = 6, dpi = 300)

