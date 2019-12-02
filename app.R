library(shiny)
library(shinydashboard)
library(rhandsontable)
source("PolynomialRegression.R")
source("QuadraticSpline.R")

if(interactive()){
  
  sideBar <- dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "About"),
      menuItem("Polynomial Regression", tabName = "PolynomialRegression"),
      menuItem("Quadratic Spline", tabName = "QuadraticSpline"),
      menuItem("Simplex Method", tabName = "Simplex")
    )
  )
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "About",
              h2("About the Program")
              ),
      tabItem(tabName = "PolynomialRegression",
                box(fileInput("file",h3("File Input"), accept =c("test/csv","text/comma-seperated-values,text/plain",".csv")),h3("Data Frame"),
                    dataTableOutput("contents")),
              box(
                numericInput(("value"), 
                             h3("Degree of Function"), 
                             value = NULL),
                h3("Function"),
                textOutput("func"),
                numericInput(("degree"), 
                             h3("Value to Determine"), 
                             value = NULL),
                h3("Value"),
                textOutput("updated"),
                submitButton("Update Values")
              ),
              box(
                h3("Plotted Graph"),
                plotOutput("plot")
              )
              ),
      tabItem(tabName = "QuadraticSpline",
              box(fileInput("quadratic",h2("File Input"), accept = c("test/csv","text/comma-seperated-values,text/plain",".csv")),
                  h3("Data frame"),
                  dataTableOutput("quad")
                  ),
              box(h3("Estimated Value"),
                  numericInput(("degree"), 
                               h3("Value to Determine"), 
                               value = NULL),
                  actionButton("update", "Update")
              )
      ),
      tabItem(tabName = "Simplex",
              box(
                helpText("Fairways Wood Company Shipping Analysis"),
                rHandsontableOutput("table1"),
                actionButton("saveBtn", "Save"))
              )
    )
  )
  
  ui <- dashboardPage(
          dashboardHeader(title = "Work In Progress"),
          sideBar,
          body)
                    
  
  shinyServer <- (function(input, output) {
    
    Plants <- c('Denver', 'Phoenix', 'Dallas',NA)
    Supply <- c('310','260','280','Demands')
    SAC <- c('10', '6', '3', '180')
    SLC <- c('8', '5', '4', '80')
    ALB <- c('6', '4', '5', '200')
    NM <- c('5', '3', '5', '160')
    NYC <- c('4', '6', '9', '220')
    df1 = data.frame(Plants = Plants, Supply = Supply, Sacramento = SAC, Albaquerque = ALB, "New Mexico" = NM, "New York City" = NYC)
    
    
    datavalues <- reactiveValues(data=df1)
    # For Polynomial Regression Page
    output$contents <- renderDataTable({
      inFile <- input$file
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = FALSE) # For loading of .csv file to the program
    }, options = list(searching = FALSE))
    
    output$func <- renderText({
      inFile = input$file
      req(input$value)
      if(is.null(input$file)) return(NULL)
      mat = as.matrix(read.csv(inFile$datapath, header = FALSE))
      x = mat[,1]
      y = mat[,2]
      if(input$value <= 0) return("[!] Cannot Print! Degree is less than 0")
      if(input$value >= length(x)) return("[!] Cannot Print! Degree is above the length of x")
      poly = PolynomialRegression(x, y, input$value)
      poly$function_string
    })
    
    output$updated <- renderText({
      inFile = input$file
      req(input$value)
      req(input$degree)
      if(is.null(input$file)) return(NULL)
      mat = as.matrix(read.csv(inFile$datapath, header = FALSE))
      x = mat[,1]
      y = mat[,2]
      if(is.null(input$value)) return(NULL)
      if(is.null(input$degree)) return(NULL)
      if(input$value <= 0) return("[!] Cannot Print! Degree is less than 0")
      poly = PolynomialRegression(x, y, input$value)
      poly$function_function(input$degree)
    })
    
    output$plot <- renderPlot({
      inFile <- input$file
      req(input$value)
      req(input$degree)
      if(is.null(inFile)) return(NULL)
      mat = as.matrix(read.csv(inFile$datapath, header = FALSE))
      x = mat[,1]
      y = mat[,2]
      if(is.null(input$value)) return(NULL)
      if(is.null(input$degree)) return(NULL)
      if(input$value <= 0) return("[!] Cannot Print! Degree is less than 0")
      poly = PolynomialRegression(x, y, input$value)
      polyplot <- poly$function_function
      plot(x, y, col="red")
      lines(x, polyplot(x), pch=4, col="blue")
      poly$function_function(input$degree)
    })
    
    # For Quadratic Spline Page
    output$quad <- renderDataTable({
      inFile <- input$quadratic
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = FALSE) # For loading of .csv file to the program
    }, options = list(searching = FALSE))
    
    # For Simplex Method
    output$table1 <- renderRHandsontable({
      rhandsontable(datavalues$data, width = 700, height = 300, selectCallback = TRUE) %>% 
        hot_col("Plants", readOnly = TRUE) %>%
        hot_col(1:5, type = "autocomplete", strict = FALSE) %>%
        hot_cols(renderer = "
                 function (instance,td,row,col,prop,value,cellProperties){
                  Handsontable.renderers.TextRenderer.apply(this, arguments);
                  if(row === 3 && col === 1){
                      cellProperties.readOnly = true;
                  }
                 return td,  cellProperties;
                 }")
    })
    
    observeEvent(
      input$df1$changes$changes,{
        x = input$table1$changes$changes[[1]][[1]]
        y = input$table1$changes$changes[[1]][[2]]
        old = input$table1$changes$changes[[1]][[3]]
        new = input$table1$changes$changes[[1]][[4]]
        
        output$changeinfo <- renderPrint({
          list(paste("Row index to be changed", x),
               paste("Column index to be changed", y),
               paste("old value to be changed", old),
               paste("new value to be changed", new))
        })
      }
    )
    
    observeEvent(input$saveBtn, 
                 write.csv(hot_to_r(input$table), file = "MyData.csv", row.names = FALSE))
  })
  
  # Run the application 
  shinyApp(ui = ui, server = shinyServer)
  
}