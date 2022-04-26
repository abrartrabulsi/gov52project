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
           
           h3("About the Model"),
           
           textOutput("texta")
       )
       
       )),
   
   tabPanel(
       
       "About",
           
    tabsetPanel(
        
        tabPanel(
            
            "About",
            
            h3("This Project"),
            
            textOutput("motivations"),
            
            br(),
            
            h3("The Data"),
            
            textOutput("thedata"),
            
            h3("About Me"),
            
            textOutput("questions")
            
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
                     scale_x_reverse() +
                     labs(x = "Child Birth Year", y = "Predicted Income")
                 
                 )
    })
    
    output$ParentPlotly2 <- renderPlotly({
        
        ggplotly(noparent %>%
                     
                     group_by(cohort) %>%
                     filter(tier_name == input$college2) %>%
                     ggplot(aes(x = cohort, y = preds)) +
                     geom_line() +
                     scale_x_reverse() +
                     labs(x = "Child Birth Year", y = "Predicted Income")
                 
        )
    })
    
    output$summary <- renderImage({
        
        
        list(src = "summary.png",
             contentType = "image",
             width = 600,
             height = 800)
        
        
    }, deleteFile = FALSE)
    
    output$texta <- renderText({
        
        "I decided to use a linear model. Initially, I made this decision 
        because OLS regression is a simple, painless place to start for model building, however, it turned out
        that a linear model was quite good at making predictions based off of these data. The final model is a linear fixed effects model
        accounting for both the birth year of inidviduals as well as their college tier. As the model stands now,
        every independent variable and factor is statistically significant, and the model is highly accurate accounting for 86% of the 
        varience in the dependent variable. While parental income certaintly has an impact on post-graduate earning potential, it's not
        nearly as high as the impact of college tier and birth cohort. While every dollar increase in parental income results in an average .0165 increase
        in the child's post-graduate income, the impact of the college attended ranges from -25320 for tier 2 schools, to -5439 for tier 8 schools. The
        impact of birth cohort is relatively higher than that of parental income as well, but idiosyncratically and unexpectedly varied compared to
        what you might expect from these data. The data were collected in 2014, thus, the data is a reflection of post-graduate income of those who had recently graduated,
        and those who had been graduated for a little over a decade. The expected impact of birth cohort would then be incresingly negative for later birth cohorts as they had less time between
        graduating college and the onset of data collection to grow in their careers, and earning potential. This is not the effect
        seen in these data. The most negative impact of bith cohort on earning potential is found in 1981-1983, or for those who graduated betwene 2003 and 2005.
        This is unusual because there was no financial crisis or other explanatory fiscal issues in the United States that could expalin this. If anything, one would expect
        those born around 1985 and 1986 to suffer the highest negative impact on their earning potential given they graduated at the start fo the recession of 2007-2008.
        Therefore, this oddity could be attributed to the data themselves. The data is aggregate, so there is the possibility of discrepencies between birth cohort sample sizes.
        The actual data do show an average decreasing income through birth cohorts as expected. This is displayed in both the interactive visualizations, and the plot
        in the previous tab."
    })
    
    output$motivations <- renderText({
        
        "When I set out to complete this project, my primary goal was to learn and understand machine learning.
        That isn't to say I don't have particular academic or research interests, but they were vastly over shadowed by
        my intent and desire to do something that would be centered around predictions, and would help me learn the basics of machine learning.
        In the process of this project I've been able to do just that from learning about the general process behind machine learning,
        learning about different types of models, deciding which would be best to use for these data, and coding and iterating on the model
        I settled on. This project was an exercise in practical skill learning and application, and I'm happy that I accomplished exactly what
        I originally, and genuinely, wanted. In the process I also learned the value of careful discernment, and dilligent understanding
        in project execution. Originally, I wanted to build a predictive calculator, which I did, but unfortunately did not make it into the
        final product due to my own lack of keen understanding about the data. Due to the nature of the data and my own model, I could only make predictions
        within the original subset of years. The model was poor at extrapolation beyond that set, due to when the data was collected, and the true values income values
        it reflected. I learned the real value of taking your time with the data, and meticulously understanding the data prior to doing a project because in the 
        real world, data will never be as neat and easily understanable as that which you find in a classroom on problem sets, and taking your time with the data
        and being precise and aligned with your deliverables from the start is more worth that sucumbing to imagined pressure at the start of a project,
        and rushing. Ultimately it worked out since the model turned out to be quite good at displaying predicted earning potential due to the nature of the data,
        and comparing the impact of factors like parental income on earning potential, given that this aspect is often left out of similar calculators and scholarship on
        the cost of colleges, and their respective post-graduate earnings."
    })
    
    output$thedata <- renderText({
        
        "The data used in this model came from Opportunity Insights, and was used in the study, 'Income Segregation and Intergenerational Mobility Across Colleges in the United States'. 
        In these data, the participants were born between 1980 and 1991, and all interviewed in 2014."
    })
    
    output$avsp <- renderImage({
        
        
        list(src = "actualvspredicted.png",
             contentType = "image",
             width = 800,
             height = 800)
        
        
    }, deleteFile = FALSE)
    
    output$questions <- renderText({
        
        "My name is Abrar Trabulsi, and I am a junior at Harvard College studying Government and Data Science. If you have
        any questions about this project, you can email me at: abrartrabulsi@college.harvard.edu"
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
