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
fulldata <- read_rds("full.rds")
noparent <- read_rds("noparent.rds")
ui <- fluidPage(theme = shinytheme("united"),
    
    br(),
    
    navbarPage("Post-Graduate Earning Potential",
               
     # note that later when coding the server with the model, "tier" should correspond 
     # with the model input and work (+ make the drop-down work)
    
    #tabsetPanel(
        
        #tabPanel("Postgraduate Income Calculator",
       
       #sidebarPanel(
        
         #selectInput("tier", "College Attended:", choices = data$tier)),
         #numericInput("par_mean", "Parent Income:", 0),
         #numericInput("cohort", "Year Born:", 0),
         #actionButton("calc", label = "Calculate"),
        
    
    # insert a field here for year graudated college for the purpose of inflation adjustment
    # once you've figured out how to incorporate that into the model
        #)),
   
   #mainPanel(
       
       #textOutput("predincome")
       
   #),
   
   tabsetPanel(
       
       tabPanel(
           
           "Interactive Visualizations",
           
           h3("Comparing Post-Graduate Earning Potential"),
           
           h5("*All predicted values for those born between 1980 & 1991 ie. graduated between 2002 & 2014"),
           
           h6("**Each ventile is a 5 percentile range, and 99 represents the 99th percentile"),
           
           sidebarPanel(
               
               selectInput("tiername", "College Attended", choices = fulldata$tier_name),
               selectInput("parventile", "Parent Income", choices = fulldata$par_ventile),
               
          
           ),
           
           sidebarPanel(
               
               selectInput("tiername2", "College Attended", choices = fulldata$tier_name),
               selectInput("parventile2", "Parent Income", choices = fulldata$par_ventile)
           )
       )
   ),
    
    mainPanel(
        
        plotlyOutput("PostPlotly"),
        
        plotlyOutput("PostPlotly2")
    ),
   
   tabsetPanel(
       
        tabPanel(
           
            "Parental Impact",
            
            h3("Visualizing the Impact of Parental Income on Post-graduate Earning Potential"),
            h5("*All predicted values for those born between 1980 & 1991 ie. graduated between 2002 & 2014"),
            h6("**Each ventile is a 5 percentile range, and 99 represents the 99th percentile"),
            
            sidebarPanel(
                
                selectInput("college1", "College Attended", choices = fulldata$tier_name),
                selectInput("pincome", "Parent Income", choices = fulldata$par_ventile)
            
            ),
            
            sidebarPanel(
                
                selectInput("college2", "College Attended", choices = noparent$tier_name)
            )
       )
   ),
   
   mainPanel(
       
       plotlyOutput("ParentPlotly1"),
       
       plotlyOutput("ParentPlotly2")
   )
    
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    #df <- reactive(data.frame("cohort" = input$cohort, "tier" = input$tier, "par_mean" = input$par_mean))
    
    
    #x <- reactive({predict(model, newdata = df())})
    #output$predincome <- renderText({x()})
    
    #output$predincome <- eventReactive(input$calc, {predict(model, newdata = df())} )
    #output$predincome <- eventReactive(input$calc, {renderText({x()})})
    
    # the main problem here is trying to convert these reactive shiny values into
    # values the function predict() and the model can understand and compute with
    # you cannot reocncile reactive values and data frames, so I either need to find a way to
    # work around this or I may need to abandon the calculator idea
    
    # the fix was to make sure everything is rendered as a reactive value
    
    output$PostPlotly <- renderPlotly({
        
       ggplotly(fulldata %>%
                    
            group_by(cohort) %>%
            filter(tier_name == input$tiername) %>%
            filter(par_ventile == input$parventile) %>%
            ggplot(aes(x = cohort, y = preds)) + 
            geom_line() +
            scale_x_reverse() +
            labs(x = "Child Birth Year", y = "Predicted Income")
            
            )
    })
    
    output$PostPlotly2 <- renderPlotly({
        
        ggplotly(fulldata %>%
                     
                     group_by(cohort) %>%
                     filter(tier_name == input$tiername2) %>%
                     filter(par_ventile == input$parventile2) %>%
                     ggplot(aes(x = cohort, y = preds)) + 
                     geom_line() +
                     scale_x_reverse() +
                     labs(x = "Child Birth Year", y = "Predicted Income")
                 
        )
    })
    
    output$ParentPlotly1 <- renderPlotly({
        
        ggplotly(fulldata %>%
                     
                     group_by(cohort) %>%
                     filter(tier_name == input$college1) %>%
                     filter(par_ventile == input$pincome) %>%
                     ggplot(aes(x = cohort, y = preds)) +
                     geom_line() +
                     scale_x_reverse()
                 
                 )
    })
    
    output$ParentPlotly2 <- renderPlotly({
        
        ggplotly(noparent %>%
                     
                     group_by(cohort) %>%
                     filter(tier_name == input$college2) %>%
                     ggplot(aes(x = cohort, y = preds)) +
                     geom_line() +
                     scale_x_reverse()
                 
        )
    })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
