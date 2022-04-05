#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
library(readr)
library(gganimate)
library(vembedr)
library(htmltools)
library(tidyverse)
library(dplyr)

data <- read_rds("x3.rds")
model <- read_rds("fixed.rds")
ui <- fluidPage(theme = shinytheme("united"),
    
    br(),
    
    navbarPage("Postgraduate Income Calculator",
               
     # note that later when coding the server with the model, "tier" should correspond 
     # with the model input and work (+ make the drop-down work)
               
        selectInput("tier", "College Attended:", levels(data$tier_name))),
        numericInput("par_mean", "Parent Income:", 0),
        numericInput("cohort", "Year Born:", 0),
        textOutput("predincome")
    
    # insert a field here for year graudated college for the purpose of inflation adjustment
    # once you've figured out how to incorporate that into the model
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, server) {
 
# old but failed attempts at coding this. keeping in case this is useful in the future 
    
    #x1 <- reactive({input$cohort})
    #x2 <- reactive({input$tier})
    #x3 <- reactive({input$par_mean})
    
    #df <- tibble(reactive(input$cohort), reactive(input$tier), reactive(input$par_mean))
    
    df <- reactive({
        
        cohort = input$cohort
        tier = input$tier
        par_mean = input$par_mean
    })
    
    output$predincome <- reactive(predict(model, df))
}

# Run the application 
shinyApp(ui = ui, server = server)
