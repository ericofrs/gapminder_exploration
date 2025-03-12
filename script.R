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

ggplot(gapminder %>% filter(year == input$year) %>% top_n(50, gdpPercap),
       aes(x = reorder(country, gdpPercap), y = gdpPercap)) +
  geom_bar(stat = "identity", width = 0.8)  +
  labs(title = paste("Top 50 Countries by GDP per Capita in", input$year), 
       x = "Country", 
       y = "GDP per capita in USD PPP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = country_colors)

plot_ly(data = gap_clean %>% filter(year == 2007) %>% top_n(10, gdpPercap), 
        x = ~reorder(country, gdpPercap), 
        y = ~gdpPercap, 
        type = "bar",
        marker = list(color = color_vector)) %>%
  layout(title = paste("Top", 10, "GDP per Capita in", 2007),
         xaxis = list(title = "Country", tickangle = 45),
         yaxis = list(title = "GDP per capita in USD PPP"))

ggplot(gapminder %>% filter(year == input$year) %>% top_n(50, lifeExp) ,
       aes(x = reorder(country, lifeExp), y = lifeExp)) +
  geom_bar(stat = "identity", width = 0.8)  +
  labs(title = paste("Top 50 Countries by Life Expectation in", input$year), 
       x = "Country", 
       y = "Life Expectation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = country_colors)



ggplot(gapminder %>% filter(year == input$year) %>% top_n(50, lifeExp),
       aes(x = gdpPercap, y = lifeExp, size = pop, color = country)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(2, 10)) +
  scale_color_manual(values = country_colors) +
  labs(
    title = paste("GDP per Capita vs. Life Expectancy in", input$year),
    x = "GDP per Capita",
    y = "Life Expectancy",
    size = "Population",
    color = "Country"
  ) +
  theme_minimal()

data <- gap_clean %>%
  filter(year == 2007) %>% top_n(10, lifeExp) 

hchart(data, 
       type = "bar", 
       hcaes(x = reorder(country, lifeExp), y = lifeExp, color = country)) %>%
  hc_title(text = paste("Top", 10, "Countries by Life Expectation in", 2007)) %>%
  hc_xAxis(title = list(text = "Country"), 
           categories = data$country, 
           labels = list(rotation = -45)) %>%
  hc_yAxis(title = list(text = "Life Expectation")) %>%
  hc_colors(country_colors)  # Apply the custom country colors

plot_ly(data, 
        y = ~reorder(country, lifeExp), 
        x = ~lifeExp, 
        type = "bar", 
        color = ~country,  # This automatically assigns a color to each country
        colors = country_colors) %>%  # Use custom colors from 'country_colors'
  layout(title = paste("Top", 10, "Countries by Life Expectation in", 2007),
         xaxis = list(title = "Country", tickangle = -45),
         yaxis = list(title = "Life Expectation"),
         showlegend = FALSE) 



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