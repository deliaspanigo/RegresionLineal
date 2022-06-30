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
    br(), br(),
    uiOutput("universo")



    # fluidRow(
    #
    #   # Esta es X
    #   box(plotOutput("plot1", height = 250)),
    #   box(
    #     title = "Controls",
    #     sliderInput("slider", "Number of observations:", 1, 100, 50)
    #   )
    # ),


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

    mydata <- mtcars
  })


  output$universo <- renderUI({

    # Control solo de Data
    if(is.null(data())) return(NULL)


    tabsetPanel(
      tabPanel("Tablas de Referencia",
               h2("Referencia Normalidad"),
               tableOutput("table01"), br(),
               h2("Referencia Correlación Pearson"),
               tableOutput("table02"), br(),
               h2("Referencia Correlación Spearman"),
               tableOutput("table03"), br(),
               h2("Referencia Regresión Lineal"),
               tableOutput("table04")
               ),
      tabPanel("Codigo",
                        verbatimTextOutput("texto01"),
                        tags$head(tags$style(HTML("
                            #texto01 {
                              font-size: 15px;
                            }
                            ")))
               ),
      tabPanel("ROutputs",
                        verbatimTextOutput("texto02"),
                        tags$head(tags$style(HTML("
                            #texto02 {
                              font-size: 15px;
                            }
                            "))))
      )




  })

  # Seleccion de Variables X e Y
  output$independents <- renderUI({

    # Control solo de Data
    if(is.null(data())) return(NULL)

    # Opciones para X
    selectInput('variable_x',
                'Variable Regresora/Independiente (X)',
                choices = colnames(data()),
                selected = colnames(data())[c(2,3)],
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


             numericInput(inputId = "alpha",
                          label = "Alfa",
                          value = 0.05,
                          min = 0,
                          max = 1,
                          step = 0.01
             )

  })


  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  all_in_one <- reactive({

    # Control solo de Data
    if(is.null(data())) return(NULL)
    if(is.null(input$variable_y)) return(NULL)
    if(is.null(input$variable_x)) return(NULL)
    if(is.null(input$variable_y)) return(NULL)
    if(is.null(input$decimals)) return(NULL)
    if(is.null(input$alpha)) return(NULL)

    database <- data()
    # x_var <- colnames(database)[c(2,4)]
    # y_var <- colnames(database)[c(1)]

    y_var <- input$variable_y
    x_var <- input$variable_x
    decimals <- input$decimals
    alpha <- input$alpha
    confidence <- Rscience.Confidence(alpha = alpha)

    # x_var <- input$variable_x
    # y_var <- input$variable_y

    # database <- mtcars
    # x_var <- c("cyl", "hp", "wt")
    # y_var <- "mpg"

    # Global Options
    # decimals <- input$decimals
    # alpha <- 0.05
    # confidence <- Rscience.Confidence(alpha = alpha)


    todo <- MegaDAVID(database = database,
                      x_var = x_var,
                      y_var = y_var,
                      decimals = decimals,
                      alpha = alpha,
                      confidence = confidence)


      todo
  })

  observe(output$texto01 <- renderText ({

   all_in_one()$the_code



  }))



  observe(output$texto02 <- renderPrint ({

  #  all_in_one()$ROutput[["Normal"]][[1]]
    all_in_one()$ROutput


  }))

  observe(output$table01 <- renderTable ({

    #  all_in_one()$ROutput[["Normal"]][[1]]
    all_in_one()$All_Reference[["Normal"]]


  }))


  observe(output$table02 <- renderTable ({

    #  all_in_one()$ROutput[["Normal"]][[1]]
    all_in_one()$All_Reference[["CorPearson"]]


  }))

  observe(output$table03 <- renderTable ({

    #  all_in_one()$ROutput[["Normal"]][[1]]
    all_in_one()$All_Reference[["CorSpearman"]]


  }))

  observe(output$table04 <- renderTable ({

    #  all_in_one()$ROutput[["Normal"]][[1]]
    all_in_one()$All_Reference[["LinearRegresion"]]


  }))

}

shinyApp(ui, server)
