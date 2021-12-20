
#Librerias necesarias

library(shiny)
library(shinythemes)

library(UBL)
library(caret)
library(tm)
library(kernlab)

#Interfaz del usuario

ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage("Predictor de enfermedad:",
                           
                           tabPanel("Usuario",
                                    # Valores de entrada
                                    sidebarPanel(
                                        HTML("<h3>Parámetros de entrada</h3>"),
                                        sliderInput(inputId = "edad", label = h3("Edad"), min = 1, 
                                                    max =  120, value = 20),
                                    
                                        numericInput(inputId = "TBIL", "Bilirrubina total (mg/dL)", value = 2),
                                        numericInput(inputId = "DBIL", "Bilirrubina directa (mg/dL)", value = 2),
                                        numericInput(inputId = "ALKPHOS", "Fosfatasa alcalina (U/L)", value = 2),
                                        numericInput(inputId = "SGPT", "Aminotransferasa alanina (U/L)", value = 2),
                                        numericInput(inputId = "SGOT", "Aminotransferasa aspartato (U/L)", value = 2),
                                        numericInput(inputId = "TP", "Proteínas totales", value = 2),
                                        numericInput(inputId = "ALBUMINA", "Albúmina (g/dL)", value = 2),
                                        numericInput(inputId = "RATIO", "Ratio albumina/globulina", value = 0.5),
                                        selectInput(inputId = "GENERO", "GÉNERO", 
                                                    choices = list("Hombre" = "Male", "Mujer" = "Female"), 
                                                    selected = "Male"),
                                        
                                        actionButton("submitbutton", 
                                                     "Confirmar", 
                                                     class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                        tags$label(h3('Parámetros elegidos y predicción')), # Los parametros
                                        verbatimTextOutput('contents'),
                                        tableOutput('tabledata'), # Tabla de resultados
                                        verbatimTextOutput('prediccion'), #prediccion
                                        verbatimTextOutput('prediccion2') #explicacion
                                        
                                    ) # Panel principal
                                    
                           ),
                           
                           tabPanel("Base de datos", 
                                    titlePanel("database"), 
                                    headerPanel(title= "Predicción de base de datos"),
                                    sidebarPanel(
                                        fileInput("file","Seleccione el archivo"),
                                        
                                        radioButtons("sep","Separador", choices= c(Coma= ",",Punto= ".", Guión= "-" )),
                                        checkboxInput("header", "¿Encabezado?"),
                                        
                                        actionButton("submitbutton2", 
                                                     "Confirmar", 
                                                     class = "btn btn-primary")
                                    ),
                                    mainPanel(
                                        h2("Condiciones para poder vislumbrar la base de datos:"),
                                           h5("- Debe ser un archivo '.csv'"),
                                           h5("- Debe estar separado por comas, puntos o guiones"),
                                           h5("- Debe componerse de 10 columnas, las cuales tienen que estar ordenadas"),
                                           h6("- El orden requerido: Edad, Género, Bilirrubina total, Bilirrubina directa, Fosfatasa alcalina,
                              Alanina, Aspartato, Proteínas totales, Albúmina, Ratio"),
                                        h3('Base de datos introducida:'),
                                        tableOutput("input_file"),
                                        h3('Predicción de los valores introducidos:'),
                                        verbatimTextOutput('prediccion3'), #prediccion
                                        tableOutput('prediccion4')
                                        
                                    )
                                    
                                    
                           )
                           
                           
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Servidor                           #
####################################

server <- function(input, output, session) {
    
    # Datos de entrada
    datasetInput <- reactive({  
        
        
        datainput <- data.frame(input$edad, input$GENERO, input$TBIL,input$DBIL,input$ALKPHOS,
                                input$SGPT, input$SGOT, input$TP, input$ALBUMINA, input$RATIO)
        names(datainput) <- c("Edad", "Genero", "TBil", "DBil", "Alkphos",
                              "Sgpt", "Sgot", "TP", "Albumina", "Ratio")
        print(datainput)
        
    })
    
    
    #########################################
    
    SVM_D <- readRDS("modelo.rds")
    
    # Status/Output Text Box
    output$contents <- renderPrint({
        if (input$submitbutton>0) { 
            isolate("Calculo realizado.") 
        } else {
            return("El servidor se encuentra preparado para trabajar.")
        }
    })
    
    # Tabla con los parametros elegidos
    output$tabledata <- renderTable({
        if (input$submitbutton>0) { 
            isolate(datasetInput()) 
        } 
    })
    
    output$prediccion <- renderText({
        if (input$submitbutton>0){

            pred <- predict(SVM_D, newdata = isolate(datasetInput()))

            print(pred)
        }
    })
    
    output$prediccion2 <- renderText({
        if (input$submitbutton>0){
            
            pred <- predict(SVM_D, newdata = isolate(datasetInput()))
            
            if (pred == 1) { 
                print("El paciente posee problemas hepáticos")
            } else{
                print("El paciente no tiene problemas hepáticos")
            }
        }
    })
    
    output$input_file <- renderTable({
        
        file_to_read = input$file
        if(is.null(file_to_read)){
            return()
        } else{
            read.table(file_to_read$datapath, sep= input$sep, header= input$header)
        }
        
    })
    
    output$prediccion3 <- renderText({
        if (input$submitbutton2>0){
            file_to_read = input$file
            if(is.null(file_to_read)){
                return()
            } else{
                DATA <- read.table(file_to_read$datapath, sep= input$sep, header= input$header)
            }
            names(DATA) <- c("Edad", "Genero", "TBil", "DBil", "Alkphos",
                                  "Sgpt", "Sgot", "TP", "Albumina", "Ratio")
            
            pred <- predict(SVM_D, newdata = DATA)
            
            print(pred)
        }
    })
    
    output$prediccion4 <- renderTable({
        if (input$submitbutton2>0){
            file_to_read = input$file
            if(is.null(file_to_read)){
                return()
            } else{
                DATA <- read.table(file_to_read$datapath, sep= input$sep, header= input$header)
            }
            names(DATA) <- c("Edad", "Genero", "TBil", "DBil", "Alkphos",
                             "Sgpt", "Sgot", "TP", "Albumina", "Ratio")
            
            pred <- predict(SVM_D, newdata = DATA)
            
            explicar <- function(x){
                agasaja <- c()
                
                for(i in 1:length(x)){
                    if (x[i] == 1) { 
                        agasaja[i] <- "El paciente posee problemas hepáticos"
                    } else{
                        agasaja[i] <- "El paciente no tiene problemas hepáticos"
                    }
                }
                return(agasaja)
            }
            
            length(pred)
            expl <- explicar(pred)
            
            outputable <- data.frame(pred, expl)
            names(outputable) <- c("Predicción", "Explicación")
            
            print(outputable)
            
        }
    })
    
}



####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)