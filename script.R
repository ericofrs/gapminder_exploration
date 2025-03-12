#install.packages("gapminder")
#install.packages("tidyverse")
#install.packages("rio")
library(gapminder)
library(tidyverse)
library(rio)
library(highcharter)

data(gapminder)

rio::export(gapminder, "data/gapminder.rds")

str(gapminder)
summary(gapminder)
colSums(is.na(gapminder))

gap_with_colors <- gapminder %>%
  mutate(color = country_colors[country])

gap_clean <- gapminder %>%
  filter(year >= 1982)

rio::export(gap_clean, "data/gap_clean.rds")

gap_clean <- import("data/gap_clean.rds") %>% as_tibble()

data <- 

list(gap_year_gdp = ,
     )  
  gap_clean %>%
  filter(year == 2007) %>% top_n(10, lifeExp) 



# Get top 50 countries by GDP per capita
top_gdp <- data_filtered %>% 
  arrange(desc(gdpPercap)) %>% 
  head(50)

# Get top 50 countries by life expectancy
top_lifeExp <- data_filtered %>% 
  arrange(desc(lifeExp)) %>% 
  head(50)

# Combine both lists (remove duplicates)
top_countries <- bind_rows(top_gdp, top_lifeExp) %>% distinct(country, .keep_all = TRUE)