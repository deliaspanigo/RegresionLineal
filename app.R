## app.R ##
library(shinydashboard)
library(DT)
library(plotly)
library(MASS)

source("lib.R")
source("lib2.R")

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    fileInput(
      inputId = "filedata",
      label = "Upload data. csv",
      multiple = FALSE,
      accept = c(".csv"),
      buttonLabel = "Choosing ...",
      placeholder = "No files selected yet"
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    h2("Linear Regresion - Rscience"),
    fluidRow(
      column(4,# Seleccion de las variables X y Y
              uiOutput("dependent")
             ),
      column(4,
            uiOutput("independents")
            )
    ),
    fluidRow(
      column(4,# Seleccion de las variables X y Y
             uiOutput("settings.decimals")
      ),
      column(4,
             uiOutput("settings.alpha")
      )
    ),



    fluidRow(

      # Esta es X
      box(plotOutput("plot1", height = 250)),
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    ),
    verbatimTextOutput("texto01")
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  # Nuestro conjunto de datos
  data <- reactive({
    req(input$filedata)
    inData <- input$filedata
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
  })

  # Seleccion de Variables X e Y
  output$independents <- renderUI({

    # Control solo de Data
    if(is.null(data())) return(NULL)

    # Opciones para X
    selectInput('variable_x',
                'Variable Regresora/Independiente (X)',
                choices = colnames(data()),
                selected = colnames(data())[2],
                multiple = TRUE)
  })



  output$dependent <- renderUI({

    # Control solo de Data
    if(is.null(data())) return(NULL)

    # Opciones de Y
    selectInput("variable_y",
                "Variable Dependiente (Y):",
                choices = colnames(data()),
                selected = colnames(data())[1],
                multiple = FALSE)
  })


  output$"settings.decimals" <- renderUI({

    # Control solo de Data
    if(is.null(data())) return(NULL)


      numericInput(inputId = "decimals",
                   label = "Decimals",
                   value = 2,
                   min = 0,
                   max = 8,
                   step = 1
      )

  })

  output$"settings.alpha" <- renderUI({

    # Control solo de Data
    if(is.null(data())) return(NULL)


             numericInput(inputId = "decimals",
                          label = "Decimals",
                          value = 2,
                          min = 0,
                          max = 8,
                          step = 1
             )

  })


  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  output$texto01 <- renderText ({

    database <- mtcars
    x_var <- c("cyl", "hp", "wt")
    y_var <- "mpg"

    # Global Options
    decimals <- 4
    alpha <- 0.05
    confidence <- Rscience.Confidence(alpha = alpha)

    performance <- "all.in"

    todo <- MegaDAVID(database = database,
                      x_var = x_var,
                      y_var = y_var,
                      decimals = decimals,
                      alpha = alpha,
                      confidence = confidence)
    todo
  #  paste0("LALAL", "ASDA", collapse = "\n")
    c(
      # paste0(code[[1]], collapse = "\n"),
      # paste0(code[[2]], collapse = "\n"),
      # paste0(code[[3]], collapse = "\n"),
      # paste0(code[[4]], collapse = "\n")
      todo
    )
  })

}

shinyApp(ui, server)
