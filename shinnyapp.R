
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    pageWithSidebar(
        headerPanel("statistical distribution"),
        
        sidebarPanel(
            selectInput("Distribution", "Please Select Distribution Type",
                        choices = c("Normal", "Exponential","Binomial")),
            sliderInput("sampleSize", "Please Select Sample Size",
                        min = 100, max = 5000, value = 1000, step = 100),
            sliderInput("p", "Probability:",
                        min = 0, max =1, value = .5, step = 0.05),
            sliderInput("x", "Number of observations:",
                        value = 500,
                        min = 1,
                        max = 1000),
            checkboxInput("normal", "Overlay Normal distrubtion", FALSE),

            conditionalPanel(condition = "input.Distribution == 'Normal'",
                             textInput("Mean", "Select the value of mean", 10),
                             textInput("SD", "Please Select Standard Deviation", 3)),
            conditionalPanel(condition = "input.Distribution == 'Exponential'",
                             textInput("Lambda", "Please Select Lambda:",1)),
            conditionalPanel(condition = "input.Distribution == 'Bionomial'",
                             textInput("mean", "Select value of mean:",1)),
            
            
            
        ),
        
        mainPanel(
            
            plotOutput("myPlot"),
          
        )
    )
    
)

server <- function(input, output, session) {
    
    output$myPlot <- renderPlot({
        distType <- input$Distribution
        size <- input$sampleSize
        
        if(distType == "Normal"){
            randomVec <- rnorm(size, mean = as.numeric(input$Mean), sd = as.numeric(input$SD))
        }
        if(distType == "bionomial") {
            randomVec <- rnorm(size, mean = as.numeric(input$Mean), sd = as.numeric(input$SD))
        
            
        }
        else{
            randomVec <- rexp(size, rate = 1/as.numeric(input$Lambda))
        }
        
        hist(randomVec, col = "light blue")
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
