library(shiny)
library(shinythemes)
library(bslib)

#Define ui
ui <- navbarPage(
    theme = shinytheme("flatly"), #definition of theme
    "gapminder", #page title
    tabPanel("Navbar 1",
             sidebarPanel(
               sliderInput(inputId = "year",
                           label = "Year:", 
                           min = 1957,
                           max = 2007,
                           value = 2007,
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
}

  
shinyApp(ui, server)
