# IDS_Project

#code to get data from worldhdi.xlsx (you might have to do install.packages("readxl") first)

library(readxl)
HDI <- read_excel("worldhdi.xlsx")
HDI <- HDI %>% rename("Code" = iso3c)
