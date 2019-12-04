library(shiny)
library(shinydashboard)
library(rhandsontable)
source("PolynomialRegression.R")
source("Simplex.R")
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
                textOutput("func")
              ),
                #tags$br(),
              box(
                numericInput(("degree"), 
                             h3("Value to Determine"), 
                             value = NULL),
                h3("Value"),
                textOutput("updated")),
                actionButton("update", "Update"),
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
              box(h3("Function Per Interval"),
                  textOutput("out1"), 
                  br(),
                  textOutput("out2"), 
                  br(),
                  textOutput("out3")
                  ),
              box(
                  numericInput(("qDegree"), 
                               h3("Value to Determine"), 
                               value = NULL)
                )
              ),
      tabItem(tabName = "Simplex",
              fluidRow(
                h1("Simplex Method", align = "center"),
                br(),
              box(
                title = "Input Table",
                status = "primary",
                helpText("Fairways Wood Company Shipping Analysis"),
                rHandsontableOutput("table"),
                checkboxInput("solution", "Show Solution", FALSE),
                actionButton("potatoMiner", "Run"),
                collapsible = TRUE, solidHeader = TRUE
                ,width = 10
              ),
              box(
                title = "Minimized Cost",
                h5(textOutput("textext"))
              ),
              box(
                title = "Solution",
                verbatimTextOutput("sol"), collapsible = TRUE,
                solidHeader = TRUE
              )
                
              )
      )
    )
  ) 
  
  ui <- dashboardPage( skin = "black",
          dashboardHeader(title = "Work In Progress"),
          sideBar,
          body
  )
  
  shinyServer <- (function(session, input, output) {
    Plants <- c('Denver', 'Phoenix', 'Dallas', NA)
    Supply <- c('310','260','280','Demands')
    SAC <- c('10', '6', '3', '180')
    SLC <- c('8', '5', '4', '80')
    ALB <- c('6', '4', '5', '200')
    NM <- c('5', '3', '5', '160')
    NYC <- c('4', '6', '9', '220')
    df = data.frame(Plants = Plants, Supply = Supply, Sacramento = SAC, "Salt Lake City" = SLC, Albaquerque = ALB, "New Mexico" = NM, "New York City" = NYC, stringsAsFactors = FALSE)
    datavalues <- reactiveValues(data=df)
    
    observeEvent(input$table$changes$changes, {
      datavalues$data <- hot_to_r(input$table)
    })
    
    
    observeEvent(input$potatoMiner, {
      functions <- list(obj = stringFunction(paste("function(X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35) ", datavalues$data[[3]][1], " * X11 + ", datavalues$data[[3]][2], " * X21 + ", datavalues$data[[3]][3], " * X31 + ",
                                                   datavalues$data[[4]][1], " * X12 + ", datavalues$data[[4]][2], " * X22 + ", datavalues$data[[4]][3], " * X32 + ",
                                                   datavalues$data[[5]][1], " * X13 + ", datavalues$data[[5]][2], " * X23 + ", datavalues$data[[5]][3], " * X33 + ",
                                                   datavalues$data[[6]][1], " * X14 + ", datavalues$data[[6]][2], " * X24 + ", datavalues$data[[6]][3], " * X34 + ",
                                                   datavalues$data[[7]][1], " * X15 + ", datavalues$data[[7]][2], " * X25 + ", datavalues$data[[7]][3], " * X35 + Z", sep = ""
      )),
      const1 = stringFunction(paste("function (X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35, Z) X11 + X12 + X13 + X14 + X15 = ", 
                                    datavalues$data[[2]][1], sep = "")),
      const2 = stringFunction(paste("function (X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35, Z) X21 + X22 + X23 + X24 + X25 = ",
                                    datavalues$data[[2]][2], sep = "")),
      const3 = stringFunction(paste("function (X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35, Z) X31 + X32 + X33 + X34 + X35 = ",
                                    datavalues$data[[2]][3], sep = "")),
      const4 = stringFunction(paste("function (X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35, Z) X11 + X21 + X31 = ",
                                    datavalues$data[[3]][4], sep = "")),
      const5 = stringFunction(paste("function (X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35, Z) X12 + X22 + X32 = ",
                                    datavalues$data[[4]][4], sep = "")),
      const6 = stringFunction(paste("function (X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35, Z) X13 + X23 + X33 = ",
                                    datavalues$data[[5]][4], sep = "")),
      const7 = stringFunction(paste("function (X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35, Z) X14 + X24 + X34 = ",
                                    datavalues$data[[6]][4], sep = "")),
      const8 = stringFunction(paste("function (X11, X21, X31, X12, X22, X32, X13, X23, X33, X14, X24, X34, X15, X25, X35, Z) X15 + X25 + X35 = ",
                                    datavalues$data[[7]][4], sep = ""))
      )
      #write.table(datavalues$data, file = "simplex.csv", row.names = FALSE, col.names = FALSE, sep=",")
      matrix = matrix(0, length(functions), length(c(checkFunctions(functions), "RHS")))
      rownames(matrix) = c(1:length(functions))
      colnames(matrix) = c(checkFunctions(functions), "RHS")
      matrix[1,] = c(-1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,(as.numeric(datavalues$data[[2]][1])) * (-1))
      matrix[2,] = c(0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,(as.numeric(datavalues$data[[2]][2])) * (-1))
      matrix[3,] = c(0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,(as.numeric(datavalues$data[[2]][3])) * (-1))
      matrix[4,] = c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,as.numeric(datavalues$data[[3]][4]))
      matrix[5,] = c(0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,as.numeric(datavalues$data[[4]][4]))
      matrix[6,] = c(0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,as.numeric(datavalues$data[[5]][4]))
      matrix[7,] = c(0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,as.numeric(datavalues$data[[6]][4]))
      matrix[8,] = c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,as.numeric(datavalues$data[[7]][4]))
      matrix[9,] = c(as.numeric(datavalues$data[[3]][1]), as.numeric(datavalues$data[[3]][2]), as.numeric(datavalues$data[[3]][3]), 
                   as.numeric(datavalues$data[[4]][1]), as.numeric(datavalues$data[[4]][2]), as.numeric(datavalues$data[[4]][3]), 
                   as.numeric(datavalues$data[[5]][1]), as.numeric(datavalues$data[[5]][2]), as.numeric(datavalues$data[[5]][3]),
                   as.numeric(datavalues$data[[6]][1]), as.numeric(datavalues$data[[6]][2]), as.numeric(datavalues$data[[6]][3]),
                   as.numeric(datavalues$data[[7]][1]), as.numeric(datavalues$data[[7]][2]), as.numeric(datavalues$data[[7]][3]), 1)
      pen = matrix

      #mat = Simplex(matrix)
      if(input$solution){
        output$sol <- renderPrint({
          Simplex(pen)
        })
      }
    
      matrix = Simplex(matrix)
        output$textext <- renderText({
          if(is.null(matrix)) {
            "No Feasible Solution"
          }
          paste("Minimized cost is: ", matrix[nrow(matrix), ncol(matrix)])
      })
    })
    
    # For Polynomial Regression Page
    output$contents <- renderDataTable({
      inFile <- input$file
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = FALSE) # For loading of .csv file to the program
    }, options = list(searching = FALSE))
    
    observeEvent(input$update, {
        inFile = input$file
        req(input$value)
        if(is.null(input$file)) return(NULL)
        mat = as.matrix(read.csv(inFile$datapath, header = FALSE))
        x = mat[,1]
        y = mat[,2]
        if(input$value <= 0) return("[!] Cannot Print! Degree is less than 0")
        if(input$value >= length(x)) return("[!] Cannot Print! Degree is above the length of x")
        poly = PolynomialRegression(x, y, input$value)
      output$func <- renderText({
        poly$function_string
      })
      
      output$updated <- renderText({
        poly$function_function(input$degree)
      })
      
      output$plot <- renderPlot({
        polyplot <- poly$function_function
        plot(x, y, col="red")
        lines(x, polyplot(x), pch=4, col="blue")
        poly$function_function(input$degree)
      })
    })
    
    # For Quadratic Spline Page
    output$quad <- renderDataTable({
      inFile <- input$quadratic
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = FALSE) # For loading of .csv file to the program
    }, options = list(searching = FALSE))
    
    output$out1 <- renderText({
      if(is.null(input$quadratic)) return(NULL)
      meow = read.csv(input$quadratic$datapath, header = FALSE)
      y = QuadraticSpline(meow)
      paste("Function 1 = ",y[1],sep="")
    })
    
    output$out2 <- renderText({
      if(is.null(input$quadratic)) return(NULL)
      meow = read.csv(input$quadratic$datapath, header = FALSE)
      y = QuadraticSpline(meow)
      paste("Function 2 = ",y[2],sep="")
    })
    
    output$out3 <- renderText({
      if(is.null(input$quadratic)) return(NULL)
      meow = read.csv(input$quadratic$datapath, header = FALSE)
      y = QuadraticSpline(meow)
      paste("Function 3 = ",y[3],sep="")
    })
    
    # For Simplex Method
    output$table <- renderRHandsontable({
      rhandsontable(datavalues$data, width = 1000, height = 200, stretchH = "all",selectCallback = TRUE) %>% 
        hot_col("Plants", readOnly = TRUE) %>%
        hot_col(1:5, type = "autocomplete", strict = FALSE) %>%
        hot_cols(renderer = "
                 function (instance,td,row,col,prop,value,cellProperties){
                  Handsontable.renderers.TextRenderer.apply(this, arguments);
                  if(row === 3 && col === 1){
                      cellProperties.readOnly = true;
                  }
                  return td, cellProperties;
                 }")
    })
  })
    
  # Run the application 
  shinyApp(ui = ui, server = shinyServer)
  
}

