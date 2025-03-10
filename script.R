#install.packages("gapminder")
#install.packages("tidyverse")
library(gapminder)
library(tidyverse)
data(gapminder)

str(gapminder)
summary(gapminder)
colSums(is.na(gapminder))


ggplot(gapminder %>% filter(year == input$year) %>% top_n(50, gdpPercap),
       aes(x = reorder(country, gdpPercap), y = gdpPercap)) +
  geom_bar(stat = "identity", width = 0.8)  +
  labs(title = paste("Top 50 Countries by GDP per Capita in", input$year), 
       x = "Country", 
       y = "GDP per capita in USD PPP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = country_colors)

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