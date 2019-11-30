library(shiny)
source("PolynomialRegression.R")
#source("QuadraticSpline.R")

if(interactive()){
  
  # Define UI for application that draws a histogram
  ui <- navbarPage("Work In Progress",
      tabPanel("Polynomial Regression",
        titlePanel( h1("Polynomial Regression", align = "center") ),
          fluidRow( 
            column(5, fileInput("file",h2("File Input"), accept =c("test/csv","text/comma-seperated-values,text/plain",".csv"))) 
            ), 
          hr(),
        sidebarLayout(
          sidebarPanel(
            titlePanel("Data Frame"),
            dataTableOutput("contents")),
            
        mainPanel(
          sidebarPanel(
            numericInput(("value"), 
                 h3("Degree of Function"), 
                 value = NULL),
              titlePanel("Function"),
              textOutput("func"),
            numericInput(("degree"), 
               h3("Value to Determine"), 
               value = NULL),
            titlePanel("Value"),
            textOutput("updated"),
            submitButton("Update Values")
          ),
          sidebarPanel(
            titlePanel("Plotted Graph"),
            plotOutput("plot")
          )
        )
      ) 
    ),
       tabPanel("Quadratic Spline", 
                titlePanel( h1("Quadratic Spline Interpolation", align = "center") ),
                fluidRow( 
                  column(5, fileInput("quadratic",h2("File Input"), accept = c("test/csv","text/comma-seperated-values,text/plain",".csv"))) 
                ),
                hr(),
                sidebarLayout(
                  sidebarPanel(
                    titlePanel("Data Frame"),
                    dataTableOutput("quad")
                  ),
                  mainPanel(
                    sidebarPanel(
                      titlePanel("Estimated Value"),
                      numericInput(("degree"), 
                                   h3("Value to Determine"), 
                                   value = NULL),
                      actionButton("update", "Update Data")
                    )
                  ) 
                )
                ),
       tabPanel("Simplex Method"),
        tabPanel("User's Manual") ,fluid = TRUE)

  shinyServer <- (function(input, output) {
    output$contents <- renderDataTable({
      inFile <- input$file
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = FALSE) # For loading of .csv file to the program
    }, options = list(searching = FALSE))
    
    output$func <- renderText({
      inFile <- input$file
      
      if(is.null(inFile)) return(NULL)
      if(is.null(input$value)) return(NULL)
      if(input$value <= 0) return("[!] Cannot Print! Degree is less than 0")
      mat = as.matrix(read.csv(inFile$datapath, header = FALSE))
      x = mat[,1]
      y = mat[,2]
      if(input$value >= length(x)) return("[!] Cannot Print! Degree is above the length of x")
      poly = PolynomialRegression(x, y, input$value)
      poly$function_string
    })
    
    output$updated <- renderText({
      inFile <- input$file
      
      if(is.null(inFile)) return(NULL)
      if(is.null(input$value)) return(NULL)
      if(is.null(input$degree)) return(NULL)
      if(input$value <= 0) return("[!] Cannot Print! Degree is less than 0")
      mat = as.matrix(read.csv(inFile$datapath, header = FALSE))
      x = mat[,1]
      y = mat[,2]
      poly = PolynomialRegression(x, y, input$value)
      poly$function_function(input$degree)
    })
    
    output$plot <- renderPlot({
      inFile <- input$file
      
      if(is.null(inFile)) return(NULL)
      if(is.null(input$value)) return(NULL)
      if(is.null(input$degree)) return(NULL)
      if(input$value <= 0) return("[!] Cannot Print! Degree is less than 0")
      mat = as.matrix(read.csv(inFile$datapath, header = FALSE))
      x = mat[,1]
      y = mat[,2]
      poly = PolynomialRegression(x, y, input$value)
      polyplot <- poly$function_function
      plot(x, y, col="red")
      lines(x, polyplot(x), pch=4, col="blue")
      poly$function_function(input$degree)
    })
  })
  
  

# Run the application 
shinyApp(ui = ui, server = shinyServer)

}