# Installing and loading the package to import excel files

install.packages("readxl")
library(readxl)

setwd("C:/Users/mahik/Downloads/data sets/data sets")

# Loading the datasets

continents <- read.csv("continents-according-to-our-world-in-data.csv")
gdp <- read.csv("gdp-per-capita-worldbank.csv")
youth <- read.csv("youth-not-in-education-employment-training.csv")

head(continents)
head(gdp)
head(youth)

str(continents)
str(gdp)
str(youth)

View(continents)
View(gdp)
View(youth)

#Use ctrl enter for each line to run that line... view might only work then

install.packages("dplyr")
library(dplyr)

# Filtering the continents together

filtered_continents <- continents %>%
  filter(Continent %in% c("Europe", "North America", "South America", "Oceania", "Asia", "Africa"))%>%
  select(Country, Year, NEET %, Continent)

View(filtered_continents)

# Filtering the continents separately...

#filtered_europe <- filter(continents, Continent == "Europe")
#View(filtered_europe)

filtered_europe <- youth %>%
  filter(Continent == "Europe") %>%
  select(Entity, 
         Year, 
         'Share of youth not in education, employment or training, total (% of youth population)')

View(filtered_europe)

filtered_samerica <- filter(continents, Continent == "South America")
View(filtered_samerica)

filtered_namerica <- filter(continents, Continent == "North America")
View(filtered_namerica)

colnames(continents)
colnames(youth)

merged_data <- youth %>%
  left_join(continents, by = "Entity")

View(merged_data)

cleaned_data <- merged_data %>%
  select(-Code.x, -Code.y, -Year.y)

View(cleaned_data)

new_data <- cleaned_data %>%
  rename(
    Year = Year.x, 
    NEET_percent = `Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.`,
    Country = Entity
  )

View(new_data)

neet_summary <- new_data %>%
  group_by(Country) %>%
  summarise(
    first_year = min(Year),
    last_year = max(Year),
    neet_first = NEET_percent[Year == first_year],
    neet_last = NEET_percent[Year == last_year]
  )

neet_normalised <- neet_summary %>%
  mutate(
    years_span = last_year - first_year,
    arithmetic_change = neet_first - neet_last,
    normalised_NEET_decrease = (arithmetic_change / years_span) / neet_first *100
  )

View(neet_normalised)
