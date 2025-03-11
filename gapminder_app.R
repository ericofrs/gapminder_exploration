library(shiny)
library(shinythemes)
#library(bslib)
library(dplyr)
library(gapminder) #database
number_top <- 50


#Define ui
ui <- navbarPage(
    theme = shinytheme("flatly"), #definition of theme
    "gapminder", #page title
    tabPanel("Navbar 1",
             sidebarPanel(
               sliderInput(inputId = "year",
                           label = "Year:", 
                           min = min(gapminder$year),
                           max = max(gapminder$year),
                           value = max(gapminder$year),
                           step = 5)
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
    tabPanel("Navbar 2", 
             h3("Line chart will be here"))
)

# Define server
server <- function(input, output) {
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$date), sep = ", ")
  })
  output$table <- renderTable({
    head(cars, 4)
  })
  output$gdpBarPlot <- renderPlot({
    ggplot(gapminder %>% filter(year == input$year) %>% top_n(number_top, gdpPercap) ,
           aes(x = reorder(country, gdpPercap), y = gdpPercap)) +
      geom_bar(stat = "identity", width = 0.8)  +
      #scale_fill_manual(values = country_colors) +
      labs(title = paste("Top", number_top,"GDP per Capita in", input$year), 
           x = "Country", 
           y = "GDP per capita in USD PPP") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })
  output$gdpLifePlot <- renderPlot({
    ggplot(gapminder %>% filter(year == input$year),
           aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
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
  })
  
  output$lifeBarPlot <- renderPlot({
    ggplot(gapminder %>% filter(year == input$year) %>% top_n(number_top, lifeExp) ,
           aes(x = reorder(country, lifeExp), y = lifeExp)) +
      geom_bar(stat = "identity", width = 0.8)  +
    #scale_fill_manual(values = country_colors) +
      labs(title = paste("Top", number_top,"Countries by Life Expectation in", input$year), 
           x = "Country", 
           y = "Life Expectation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()
  })
  
  
}

  
shinyApp(ui, server, options = list(display.mode = "showcase"))
