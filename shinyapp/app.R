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
    
   # sidebarPanel(
        
         selectInput("tier", "College Attended:", levels(data$tier))),
         numericInput("par_mean", "Parent Income:", 0),
         numericInput("cohort", "Year Born:", 0),
         #actionButton("calc", label = "Calculate"),
         textOutput("predincome")#)
    
    # insert a field here for year graudated college for the purpose of inflation adjustment
    # once you've figured out how to incorporate that into the model
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
# old but failed attempts at coding this. keeping in case this is useful in the future 
    
    #x1 <- reactive({input$cohort})
    #x2 <- reactive({input$tier})
    #x3 <- reactive({input$par_mean})
    
    df <- reactive(data.frame("cohort" = input$cohort, "tier" = input$tier, "par_mean" = input$par_mean))
    
    #df <- reactive({
        
        #cohort = input$cohort
        #tier = input$tier
        #par_mean = input$par_mean
        
    #})
    
    hi <- reactive({predict(model, newdata = df())})
    output$predincome <- renderText({hi()})
    #output$predincome <- eventReactive(input$calc, {predict(model, newdata = df())} )
    #output$predincome <- eventReactive(input$calc, renderText({hi()}))
    
    # the main problem here is trying to convert these reactive shiny values into
    # values the function predict() and the model can understand and compute with
    # you cannot reocncile reactive values and data frames, so I either need to find a way to
    # work around this or I may need to abandon the calculator idea
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
