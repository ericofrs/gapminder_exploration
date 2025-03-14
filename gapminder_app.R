library(shiny)
library(shinythemes)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(plotly)
library(gapminder) #database
library(rio)


number_top <- 20

gap_clean <- import("data/gap_clean.rds", trust = TRUE) %>% as_tibble()

#Define ui
ui <- bslib::page_navbar(
  title = h1("Gapminder"), #page title
  navbar_options = navbar_options(underline = TRUE, collapsible = TRUE),
  theme = bs_theme(
    version = 5,          # Bootstrap 5
    bootswatch = "flatly" # Apply theme
  ),
  bslib::nav_panel("Page 1",
                    navset_card_tab(title="Page 1",
                      sidebar = sidebar(
                        sliderTextInput(inputId = "year",
                                        label = "Choose a year:", 
                                        choices = unique(gap_clean$year),
                                        #value = max(gap_clean$year) # replaced by selected in shinyWidgets
                                        selected = max(gap_clean$year),
                                        grid = TRUE
                                        )
                        ),
                      tabsetPanel(
                         tabPanel("Tab 1",
                                  plotlyOutput("gdpBarPlotly"),
                                  plotlyOutput("gdpLifePlot"),
                                  plotlyOutput("lifeBarPlotly")
                                  ),
                         tabPanel("Tab 2", 
                                  h3("Map and population graph will be here")
                                  )
                         )
                      )
                    ),
  bslib::nav_panel("Page 2",
                    navset_card_tab(title = "Page 2",
                                    h3("Line chart will be here"))
                    ),
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

# Define server
server <- function(input, output) {
  
  ## Page 1
  ### Filter the data according to the user`s choice
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
  ### gdpPercap Barplot
  output$gdpBarPlotly <- renderPlotly({
    plot_ly(data = gap_year()$gap_year_gdp, 
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
  output$gdpLifePlot <- renderPlotly({
    plot_ly(
      data = gap_year()$gap_year_combined, 
      x = ~gdpPercap, 
      y = ~lifeExp, 
      size = ~pop, 
      color = ~continent, 
      colors = continent_colors,  # Apply custom continent colors
      type = "scatter", 
      mode = "markers",
      text = ~country,   # Show country name on hover
      hoverinfo = "text", # Display only the country name on hover
      marker = list(
        opacity = 0.7,   # Adjust transparency
        line = list(width = 0.5, color = "black") # Fix the `line.width` warning
      )
    ) %>%
      layout(
        title = paste("GDP per Capita vs. Life Expectancy in", input$year),
        xaxis = list(title = "GDP per Capita", spikemode = 'toaxis'),
        yaxis = list(title = "Life Expectancy", spikemode = 'toaxis'),
        legend = list(title = list(text = "Continent"))
      )
  })
  ### lifeExp Barplot
  output$lifeBarPlotly <- renderPlotly({
    plot_ly(data = gap_year()$gap_year_life , 
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
