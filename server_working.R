# Server working

# Define server
server <- function(input, output) {
  
  ## Page 1
  ### Filter the data according to the user`s choice
  gap_year <- reactive({
    subset(gap_clean,
           year == input$year)
  })
  ### gdpPercap Barplot
  output$gdpBarPlotly <- renderPlotly({
    plot_ly(data = gap_year() %>% top_n(number_top, gdpPercap), 
            x = ~reorder(country, gdpPercap), 
            y = ~gdpPercap, 
            type = "bar",
            color = ~country,  # This automatically assigns a color to each country
            colors = country_colors) %>%  # Use custom colors from 'country_colors'
      layout(title = paste("Top", number_top, "GDP per Capita in", input$year),
             xaxis = list(title = "Country", tickangle = 45),
             yaxis = list(title = "GDP per capita in USD PPP"),
             showlegend = FALSE)
  })
  ### gdp x lifeExp Scatterplot
  output$gdpLifePlot <- renderPlot({
    ggplot(gap_year(),
           aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(2, 10)) +
      scale_color_manual(values = continent_colors) +
      labs(
        title = paste("GDP per Capita vs. Life Expectancy in", input$year),
        x = "GDP per Capita",
        y = "Life Expectancy",
        size = "Population",
        color = "Continent") #+
    #theme_minimal()
  })
  ### lifeExp Barplot
  output$lifeBarPlotly <- renderPlotly({
    plot_ly(gap_year() %>% top_n(number_top, lifeExp) , 
            x = ~lifeExp, 
            y = ~reorder(country, lifeExp),
            type = "bar", 
            color = ~country,  # This automatically assigns a color to each country
            colors = country_colors) %>%  # Use custom colors from 'country_colors'
      layout(title = paste("Top", number_top, "Countries by Life Expectation in", input$year),
             xaxis = list(title = "Life Expectation in years"),
             yaxis = list(title = "Country"),
             showlegend = FALSE) 
  })
}


shinyApp(ui, server, options = list(display.mode = "showcase"))
