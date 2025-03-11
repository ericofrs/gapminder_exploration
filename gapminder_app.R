library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(gapminder) #database
library(rio)

number_top <- 10

gap_clean <- import("data/gap_clean.rds", trust = TRUE) %>% as_tibble()

#Define ui
ui <- page_navbar(
  title = "gapminder", #page title
  theme = bs_theme(
    version = 5,          # Bootstrap 5
    bootswatch = "flatly" # Apply Flatly theme
  ),
  nav_panel(title = "Navbar 1",
           sidebarPanel(
             sliderInput(inputId = "year",
                         label = "Year:", 
                         min = min(gap_clean$year),
                         max = max(gap_clean$year),
                         value = max(gap_clean$year),
                         step = 5,
                         sep = "") # no thousand separator
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Tab 1",
                        plotOutput("gdpBarPlot"),
                        plotOutput("gdpLifePlot"),
                        plotOutput("lifeBarPlot")
               ),
               tabPanel("Tab 2", 
                        h3("Map and population graph will be here"))
             )
           )
    ),
  nav_panel(title = "Navbar 2",
           h3("Line chart will be here")),
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "dark"))
)

# Define server
server <- function(input, output) {
  
  ## gdpPercap Barplot
  output$gdpBarPlot <- renderPlot({
    ggplot(gap_clean %>% filter(year == input$year) %>% top_n(number_top, gdpPercap) ,
           aes(x = reorder(country, gdpPercap), y = gdpPercap, fill = color)) +
      geom_bar(stat = "identity", width = 0.8, aes(fill = country)) +  # Define fill here
      scale_fill_manual(values = country_colors) + 
      labs(title = paste("Top", number_top,"GDP per Capita in", input$year), 
           x = "Country", 
           y = "GDP per capita in USD PPP") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  ## gdp x lifeExp Scatterplot
  output$gdpLifePlot <- renderPlot({
    ggplot(gap_clean %>% filter(year == input$year),
           aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(2, 10)) +
      scale_color_manual(values = continent_colors) +
      labs(
        title = paste("GDP per Capita vs. Life Expectancy in", input$year),
        x = "GDP per Capita",
        y = "Life Expectancy",
        size = "Population",
        color = "Continent"
      ) +
      theme_minimal()
  })
  ## lifeExp Barplot
  output$lifeBarPlot <- renderPlot({
    ggplot(gap_clean %>% filter(year == input$year) %>% top_n(number_top, lifeExp) ,
           aes(x = reorder(country, lifeExp), y = lifeExp)) +
      geom_bar(stat = "identity", width = 0.8, aes(fill = country)) +  # Define fill here
      scale_fill_manual(values = country_colors) +  # Map country colors directly
      labs(title = paste("Top", number_top,"Countries by Life Expectation in", input$year), 
           x = "Country", 
           y = "Life Expectation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()
  })
}

  
shinyApp(ui, server, options = list(display.mode = "showcase"))
