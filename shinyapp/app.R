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
library(htmltools)
library(tidyverse)
library(dplyr)

data <- read_rds("x3.rds")
model <- read_rds("fixed.rds")
ui <- fluidPage(theme = shinytheme("united"),
    
    br(),
    
    navbarPage("Title",
               
     # note that later when coding the server with the model, "tier" should correspond 
     # with the model input and work (+ make the drop-down work)
    
    tabsetPanel(
        
        tabPanel("Postgraduate Income Calculator",
       
       sidebarPanel(
        
         selectInput("tier", "College Attended:", choices = data$tier)),
         numericInput("par_mean", "Parent Income:", 0),
         numericInput("cohort", "Year Born:", 0),
         #actionButton("calc", label = "Calculate"),
         textOutput("predincome")
    
    # insert a field here for year graudated college for the purpose of inflation adjustment
    # once you've figured out how to incorporate that into the model
        ))),
   
   #mainPanel(
       
       textOutput("predincome")
       
   #)#,
   
   #tabsetPanel(
       
       #tabPanel(
           
           #"Change in Post-Graduate Income Over Time",
           
           #sidebarPanel(
               
               #selectInput("tiername", "College Attended", choices = data$tier_name),
               #selectInput("parventile", "Parent Income", choices = data$par_ventile),
               
          
           #)
       #)
   #),
    
    #mainPanel(
        
        #plotlyOutput("PostPlotly")
    #)
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    df <- reactive(data.frame("cohort" = input$cohort, "tier" = input$tier, "par_mean" = input$par_mean))
    
    
    x <- reactive({predict(model, newdata = df())})
    output$predincome <- renderText({x()})
    
    #output$predincome <- eventReactive(input$calc, {predict(model, newdata = df())} )
    #output$predincome <- eventReactive(input$calc, {renderText({x()})})
    
    # the main problem here is trying to convert these reactive shiny values into
    # values the function predict() and the model can understand and compute with
    # you cannot reocncile reactive values and data frames, so I either need to find a way to
    # work around this or I may need to abandon the calculator idea
    
    # the fix was to make sure everything is rendered as a reactive value
    
    output$PostPlotly <- renderPlotly({
        
       ggplotly(data %>%
                    
            group_by(cohort) %>%
            filter(tier_name == input$tiername) %>%
            filter(par_ventile == input$parventile) %>%
            ggplot(aes(x = cohort, y = k_mean)) + 
            geom_col()
            
       )
    })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
