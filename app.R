## Shiny App Code for Polynomial Fitter (app.R)
##
## This is the single-file version, so it is independent of ui.R and 
## server.R files, which are used to write Shiny apps the old way.
##
## Last update: 8/29/16 (George Chadderdon)


## shiny package load.
library(shiny)


## Functions to be used for curve-fitting.

## Get a polynomial curve in a data frame.
getPolyCurve <- function(xmin=-5.0, xmax=5.0, xstep=0.001, polyCoeffs=0.0) {
    ## Create the sequence of xs.
    xs <- seq(xmin, xmax, xstep)
    
    ## Start the ys at the intercept value.
    ys <- rep(polyCoeffs[1], length(xs))
    
    ## If we have more than degree 0...
    if (length(polyCoeffs) > 1) {
        ## For each coefficient, add the appropriate polynomial term.
        for (ind in 2:length(polyCoeffs)) {
            ## Only add if the coefficient is not NA.
            if (!is.na(polyCoeffs[ind])) {
                ys <- ys + polyCoeffs[ind] * xs ^ (ind - 1)
            }
        }
    }
    
    ## Write the curve into an x, y data frame.
    data.frame(x=xs, y=ys)
}

## Build a formula string for polynomial regression for particular
getPolyFormulaStr <- function(outcomeVar, predictorVar, polyDegree=0) {
    theFormula <- sprintf("%s ~", outcomeVar)
    if (polyDegree == 0) {
        theFormula <- paste(theFormula, "1")
    } else {
        for (ind in 1:polyDegree) {
            if (ind == 1) 
            {
                theFormula <- paste(theFormula, sprintf("%s", predictorVar))
            } else {
                theFormula <- paste(theFormula, 
                    sprintf("+ I(%s^%d)", predictorVar, ind))
            }
        }
    }
    theFormula
}


## Shiny UI definition.
ui <- pageWithSidebar(
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
)

## Shiny server definition.
server <- function(input, output) {
    selectedDF <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.  
        inFile <- input$file1
        
        ## If we don't have a file loaded in yet, set the data set to 
        ## mtcars.
        if (is.null(inFile))
            return(mtcars)
        
        ## Otherwise
        else 
            read.csv(inFile$datapath)
    })
    
    ## Pull the selected variables into a new data frame
    selectedData <- reactive({
        ## Return a NULL if we aren't pointing to a good column.
        if (!(input$xcol %in% colnames(selectedDF()) && 
              input$ycol %in% colnames(selectedDF()))) 
            return(NULL)

        ## Get columns set by the column user inputs.
        selCols <- selectedDF()[, c(input$xcol, input$ycol)]

        ## Drop any rows that have NA in them.
        good <- complete.cases(selCols)
        selCols <- selCols[good, ]
        
        selCols
    })
    
    ## Do the polynomial curve-fit and extract the betas and MSE.
    fitResults <- reactive({
        ## Pull xcol and ycol from the selected data.
        xcol <- colnames(selectedData())[1]
        ycol <- colnames(selectedData())[2]
        
        ## Get the formula we want to use.
        theFormula <- as.formula(getPolyFormulaStr(ycol, xcol, 
            input$polyDeg))
        
        ## Perform the polynomial fit.
        modelFit <- lm(theFormula, data=selectedData())
        
        ## Pull out the mean-squared error.
        theResids <- residuals(modelFit)
        mse <- sum(theResids ^ 2) / nrow(selectedDF())
        
        ## Pull out the coefficients.
        betas <- modelFit$coefficients
        
        ## Stuff the data into a list.
        list(betas=betas, mse=mse)
    })
    
    ## Prepare the curve-fit plot.
    output$plot1 <- renderPlot({
        ## Drop out if we don't have valid data.
        if (is.null(selectedData()))
            return(NULL)
        
        ## Get the range limits for the selected data.
        xmin <- min(selectedData()[[1]])
        xmax <- max(selectedData()[[1]])
        ymin <- min(selectedData()[[2]])
        ymax <- max(selectedData()[[2]]) 
        
        ## Get the fit curve into a data frame.
        fitCurve <- getPolyCurve(xmin=xmin, xmax=xmax, 
            polyCoeffs=fitResults()$betas)
        
        ## Plot the selected data and the fit.
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(), col="blue", pch=16)
        lines(fitCurve)        
    })
    
    ## Print the available columns.
    output$availcols <- renderText({
        theText <- "Available Variables:\n"
        colCount <- 0
        cutoffWidth <- 20
        for (ind in 1:ncol(selectedDF())) {
            newCol <- colnames(selectedDF())[ind]
            colCount <- colCount + nchar(newCol) + 1
            if (colCount > cutoffWidth) {
                theText <- paste(theText, "\n")
                colCount <- 0
            }
            theText <- paste(theText, newCol)
        }
        theText
    })
    
    ## Print the MSE and betas.
    output$oid1 <- renderText({
        ## Drop out if we don't have valid data.
        if (is.null(selectedData()))
            return(NULL)
        
        theText <- "Fit Results:\n"
        theText <- paste(theText, sprintf("MSE = %e\n", fitResults()$mse))
        theText <- paste(theText, "Polynomial Coeffs:\n")
        for (ind in 1:length(fitResults()$betas)) {
            if (ind == 1) {
                theText <- paste(theText, sprintf(" Intercept\t%e\n", 
                    fitResults()$betas[1]))                
            } else if (ind == 2) {
                theText <- paste(theText, sprintf(" x\t\t%e\n", 
                    fitResults()$betas[2]))                   
            } else {
                theText <- paste(theText, sprintf(" x^%d\t\t%e\n", 
                    ind - 1, fitResults()$betas[ind]))                  
            }
        }
        theText
    })
}

## Run the server using the UI and server function.
shinyApp(ui=ui, server=server)