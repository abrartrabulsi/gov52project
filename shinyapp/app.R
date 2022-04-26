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
ui <- fluidPage(theme = shinytheme("flatly"),
    
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
   
   tabPanel(
       
       "Interactive Visualizations",
       
       tabsetPanel(
           
           tabPanel(
                
               "Post Graduate Earning Potential",
                          
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
           ),
    
    mainPanel(
        
        plotlyOutput("PostPlotly"),
        
        plotlyOutput("PostPlotly2")
    )),
       
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
       ),
   
   mainPanel(
       
       plotlyOutput("ParentPlotly1"),
       
       plotlyOutput("ParentPlotly2")
   ))),
   
   tabPanel(
       
       "Model Information",
   
   tabsetPanel(
       
       tabPanel(
           
           "Model",
           
           h3("Model Output"),
           
           imageOutput("summary")),
           
       
       tabPanel(
           
           "Model Fit",
           
           h3("Actual vs. Predicted Values & Linear Fit"),
           
           imageOutput("avsp")
           
          
       ),
       
       tabPanel(
           
           "Analysis",
           
           h3("Commentary About the Model"),
           
           textOutput("texta")
       )
       
       )),
   
   tabPanel(
       
       "About",
           
    tabsetPanel(
        
        tabPanel(
            
            "About",
            
            h3("Motivations"),
            
            textOutput("motivations"),
            
            br(),
            
            h3("The Data"),
            
            textOutput("thedata"),
            
            br(),
            
            h3("Purpose"),
            
            textOutput("purpose")
            
        )
    )
    
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
    
    output$summary <- renderImage({
        
        
        list(src = "summary.png",
             contentType = "image",
             width = 600,
             height = 800)
        
        
    }, deleteFile = FALSE)
    
    output$texta <- renderText({
        
        "A linear model was used to make predictions about the data. The model was weighted, featured two fixed
        effects, and no interaction variables. With an R-squared value of .86,
        the model is effective at making accurate predictions."
    })
    
    output$motivations <- renderText({
        
        "There is plenty of data and information about the impact of college on personal financial outcomes.
        However, the impact of parental income on personal financial outcomes has not been studied nearly as much.
        This model is able to visually illustrate just how impactful parental income is on an individual's
        earning potential."
    })
    
    output$thedata <- renderText({
        
        "The data used in this model came from Opportunity Insights, and was used in the study, 'Income Segregation and Intergenerational Mobility Across Colleges in the United States'. 
        In these data, the participants were born between 1980 and 1991, and all interviewed in 2014. The reason why this model can be used to
        acertain earning potential, is because the individuals' incomes were recorded when they were various ages. However,
        also due to the nature of these data, the model is unable to effectively extrapolate outside of the included years."
    })
    
    output$purpose <- renderText({
        
        "This Shiny App was originally created as the final project deliverable for ths class Gov52: Models taught by Andrew Therriault."
    })
    
    output$avsp <- renderImage({
        
        
        list(src = "actualvspredicted.png",
             contentType = "image",
             width = 800,
             height = 600)
        
        
    }, deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
