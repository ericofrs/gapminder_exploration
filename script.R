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

gap_year <- list(gap_year_gdp = gap_clean %>%
               filter(year == 2007) %>% 
               top_n(10, gdpPercap),
             gap_year_life = gap_clean %>%
               filter(year == 2007) %>% 
               top_n(10, lifeExp),
             gap_year_combined = bind_rows(
               gap_clean %>%
                 filter(year == 2007) %>%
                 top_n(10, gdpPercap),
               gap_clean %>%
                 filter(year == 2007) %>%
                 top_n(10, lifeExp)
               ) %>%
               distinct(country, .keep_all = TRUE)
             )

gap_year <- reactive({
  subset(gap_clean,
         year == input$year)
})

gap_year <- reactive({
  list(gap_year_gdp = gap_clean %>%
           filter(year == input$year) %>% 
           top_n(number_top, gdpPercap),
         gap_year_life = gap_clean %>%
           filter(year == input$year) %>% 
           top_n(number_top, lifeExp),
         gap_year_combined = bind_rows(
           gap_clean %>%
             filter(year == input$year) %>%
             top_n(number_top, gdpPercap),
           gap_clean %>%
             filter(year == input$year) %>%
             top_n(number_top, lifeExp)
         ) %>%
           distinct(country, .keep_all = TRUE)
    )
})