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

data <- read_rds("x3.rds")
ui <- fluidPage(theme = shinytheme("united"),
    
    br(),
    
    navbarPage("Postgraduate Income Calculator",
               
               selectInput("tier_name", "College Attended:", levels(data$tier_name)))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    set.seed(100) # for reproducing results
    
    index <- sample(1:nrow(x3), .6*nrow(x3)) 
    training <- x3[index, ]
    test <- x3[-index, ]
    
    # creating test/train data split
    
    model <- lm(k_mean ~ par_mean + factor(tier) + factor(cohort) + par_mean*factor(tier), data = training, weights = count)
}

# Run the application 
shinyApp(ui = ui, server = server)
