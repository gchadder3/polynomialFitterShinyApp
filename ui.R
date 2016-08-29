## Shiny App Code for UI for Polynomial Fitter (ui.R)
##
## This is provided just to show that I can also implemenent the 
## Shiny app the old way: using ui.R and server.R.
##
## Last update: 8/29/16 (George Chadderdon)


## shiny package load.
library(shiny)

shinyUI(pageWithSidebar(
    headerPanel('Polynomial Fitter'),
    
    sidebarPanel(
        fileInput(inputId='file1', 
                  label='Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        
        textInput(inputId='ycol', 
                  label='Outcome Variable', 
                  value='mpg'), 
        
        textInput(inputId='xcol', 
                  label='Predictor Variable', 
                  value='wt'), 
        
        verbatimTextOutput(outputId='availcols'),
        
        numericInput(inputId='polyDeg', 
                     label='Polynomial Degree', 
                     value=2, 
                     min=0)
    ),
    
    mainPanel(
        plotOutput(outputId='plot1'), 
        
        verbatimTextOutput(outputId='oid1')
    )
))