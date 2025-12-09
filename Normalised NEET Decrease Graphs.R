# Installing and loading the package to import excel files

install.packages("readxl")
library(readxl)

library(tidyverse)

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

# Filtering for Asia

cleaned_data <- cleaned_data %>%
  rename(Country = Entity)

asia_data <- neet_normalised %>%
  inner_join(cleaned_data %>% select(Country, Continent) %>% distinct(),
             by = "Country") %>%
  filter(Continent == "Asia")

install.packages("ggplot2")
library(ggplot2)

ggplot(asia_data, aes(
  x = normalised_NEET_decrease,
  y = reorder(Country, normalised_NEET_decrease)
)) +
  geom_col(fill = "pink", alpha = 0.7) +
  geom_vline(xintercept = mean(asia_data$normalised_NEET_decrease),
             linetype = "dashed", color = "red", size = 1) +
  annotate(
    "text",
    x = mean(asia_data$normalised_NEET_decrease, na.rm = TRUE),  # same x-value
    y = Inf,                # place it at the top of the plot
    label = "Average normalised NEET decrease",
    color = "red",
    hjust = -0.1,           # adjust horizontal position
    vjust = 1.5,            # adjust vertical position
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Normalized Yearly Average NEET Decrease by Country in Asia",
    x = "Normalized Yearly Average NEET Decrease (%)",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

# EUROPE

europe_data <- neet_normalised %>%
  inner_join(cleaned_data %>% select(Country, Continent) %>% distinct(),
             by = "Country") %>%
  filter(Continent == "Europe")

View(europe_data)

ggplot(europe_data, aes(
  x = normalised_NEET_decrease,
  y = reorder(Country, normalised_NEET_decrease)
)) +
  geom_col(fill = "pink", alpha = 0.7) +
  geom_vline(xintercept = mean(europe_data$normalised_NEET_decrease),
             linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Normalized Yearly Average NEET Decrease by Country in Europe",
    x = "Normalized Yearly Average NEET Decrease (%)",
    y = "Country"
  ) +
  annotate(
    "text",
    x = mean(europe_data$normalised_NEET_decrease, na.rm = TRUE),  # same x-value
    y = Inf,                # place it at the top of the plot
    label = "Average normalised NEET decrease",
    color = "red",
    hjust = -0.1,           # adjust horizontal position
    vjust = 1.5,            # adjust vertical position
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

# AFRICA

africa_data <- neet_normalised %>%
  inner_join(cleaned_data %>% select(Country, Continent) %>% distinct(),
             by = "Country") %>%
  filter(Continent == "Africa")

View(africa_data)

ggplot(africa_data, aes(
  x = normalised_NEET_decrease,
  y = reorder(Country, normalised_NEET_decrease)
)) +
  geom_col(fill = "pink", alpha = 0.7) +
  geom_vline(xintercept = mean(africa_data$normalised_NEET_decrease),
             linetype = "dashed", color = "red", size = 1) +
  annotate(
    "text",
    x = mean(samerica_data$normalised_NEET_decrease, na.rm = TRUE),  # same x-value
    y = Inf,                # place it at the top of the plot
    label = "Average normalised NEET decrease",
    color = "red",
    hjust = -0.1,           # adjust horizontal position
    vjust = 1.5,            # adjust vertical position
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Normalized Yearly Average NEET Decrease by Country in Africa",
    x = "Normalized Yearly Average NEET Decrease (%)",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

# NORTH AMERICA

namerica_data <- neet_normalised %>%
  inner_join(cleaned_data %>% select(Country, Continent) %>% distinct(),
             by = "Country") %>%
  filter(Continent == "North America")

View(namerica_data)

ggplot(namerica_data, aes(
  x = normalised_NEET_decrease,
  y = reorder(Country, normalised_NEET_decrease)
)) +
  geom_col(fill = "pink", alpha = 0.7) +
  geom_vline(xintercept = mean(namerica_data$normalised_NEET_decrease),
             linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Normalized Yearly Average NEET Decrease by Country in North America",
    x = "Normalized Yearly Average NEET Decrease (%)",
    y = "Country"
  ) +
  annotate(
    "text",
    x = mean(namerica_data$normalised_NEET_decrease, na.rm = TRUE),  # same x-value
    y = Inf,                # place it at the top of the plot
    label = "Average normalised NEET decrease",
    color = "red",
    hjust = -0.1,           # adjust horizontal position
    vjust = 1.5,            # adjust vertical position
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

# SOUTH AMERICA

samerica_data <- neet_normalised %>%
  inner_join(cleaned_data %>% select(Country, Continent) %>% distinct(),
             by = "Country") %>%
  filter(Continent == "South America")

View(samerica_data)

ggplot(samerica_data, aes(
  x = normalised_NEET_decrease,
  y = reorder(Country, normalised_NEET_decrease)
)) +
  geom_col(fill = "pink", alpha = 0.7) +
  geom_vline(xintercept = mean(samerica_data$normalised_NEET_decrease, na.rm = TRUE),
             linetype = "dashed", color = "red", size = 1) +
  annotate(
    "text",
    x = mean(samerica_data$normalised_NEET_decrease, na.rm = TRUE),  # same x-value
    y = Inf,                # place it at the top of the plot
    label = "Average normalised NEET decrease",
    color = "red",
    hjust = -0.1,           # adjust horizontal position
    vjust = 1.5,            # adjust vertical position
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Normalized Yearly Average NEET Decrease by Country in South America",
    x = "Normalized Yearly Average NEET Decrease (%)",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

# OCEANIA

oceania_data <- neet_normalised %>%
  inner_join(cleaned_data %>% select(Country, Continent) %>% distinct(),
             by = "Country") %>%
  filter(Continent == "Oceania")

View(oceania_data)

ggplot(samerica_data, aes(
  x = normalised_NEET_decrease,
  y = reorder(Country, normalised_NEET_decrease)
)) +
  geom_col(fill = "pink", alpha = 0.7) +
  geom_vline(xintercept = mean(samerica_data$normalised_NEET_decrease, na.rm = TRUE),
             linetype = "dashed", color = "red", size = 1) +
  annotate(
    "text",
    x = mean(samerica_data$normalised_NEET_decrease, na.rm = TRUE),  # same x-value
    y = Inf,                # place it at the top of the plot
    label = "Average normalised NEET decrease",
    color = "red",
    hjust = -0.1,           # adjust horizontal position
    vjust = 1.5,            # adjust vertical position
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Normalized Yearly Average NEET Decrease by Country in Oceania",
    x = "Normalized Yearly Average NEET Decrease (%)",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  )
