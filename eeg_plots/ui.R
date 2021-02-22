library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage( theme = shinytheme("cerulean"),
               
               # Application title
               titlePanel("Entrenamiento en emoción Pre-Post"),
               
               tabsetPanel(
                   # * tabPanel PREs -----------------------------
                   tabPanel("PREs",
                            sidebarLayout(
                                sidebarPanel(
                                    pickerInput(
                                        inputId  = "cond",
                                        label    = "Condición: ",
                                        choices  = c("Identidad", "Sexo",
                                                     "Alegría","Tristeza",
                                                     "Enojo"),
                                        selected = "Identidad",
                                        options  = list(
                                            `actions-box` = TRUE,
                                            size = 10,
                                            `selected-text-format` = "count > 3"
                                            
                                        ),
                                        multiple = TRUE
                                    ),
                                    pickerInput(
                                        inputId  = "elec",
                                        label    = "Electrodo: ",
                                        choices  = c("Fz","Cz","Pz","T5","T6"),
                                        selected = "Pz",
                                        options  = list(
                                            `actions-box` = TRUE,
                                            size = 10,
                                            `selected-text-format` = "count > 3"
                                            
                                        ),
                                        multiple = TRUE
                                    ),
                                    
                                    materialSwitch(
                                        inputId = "grupos",
                                        label   = "Separar por grupos",
                                        status  = "primary",
                                        value   = TRUE 
                                    ),
                                    radioButtons(
                                        inputId = "radio",
                                        label = "Selección de participantes:",
                                        choices = list(
                                            "Todos",
                                            "Selección manual"),
                                        selected = "Todos"
                                    ),
                                    conditionalPanel(
                                        condition = "input.radio != 'Todos'",
                                        
                                        selectInput(inputId = "partic",
                                                    "Participante: ",
                                                    participantes$Sujeto),
                                        checkboxInput(inputId = "vis_picos",
                                                      "Mostrar picos en la gráfica")
                                    )
                                    
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                    
                                    fluidRow(
                                        column(12,
                                               plotlyOutput("eeg")
                                        )
                                    ),
                                    
                                    conditionalPanel(
                                        condition = "input.radio != 'Todos'",
                                        fluidRow(
                                            column(6,h3("Evaluación PRE")),
                                            column(6, h3("Evaluación POST"))
                                        ),
                                        fluidRow(
                                            column(6, tableOutput("picos_pre")),
                                            column(6, tableOutput("picos_post"))
                                        )
                                    )
                                )
                            )
                   ),
                   # * tabPanel AUC ------------------------------
                   tabPanel("Area bajo la curva Pz",
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput(inputId = "cond_auc",
                                                "Condición: ",
                                                c("Identidad", "Sexo",
                                                  "Alegría","Tristeza",
                                                  "Enojo")),
                                    selectInput(inputId = "partic_auc",
                                                "Participante: ",
                                                participantes$Sujeto)
                                ),
                                mainPanel(
                                    splitLayout(cellWidths = c("70%", "30%"),
                                                plotlyOutput("auc_plot"),
                                                tableOutput("auc"),
                                                tags$head(tags$style("#auc{color: black;
                                    font-size: 20px;
                                    font-style: bold;
                                    text-align: center;
                                    }"
                                                )
                                                )
                                    
                                    )
                                )
                            )
                   ),
                   # * tabPanel Amp. y lat. PREs ------------------
                   tabPanel("Amplitudes y latencias PREs",
                            sidebarLayout(
                                
                                sidebarPanel(
                                    checkboxInput(inputId = "escala_man",
                                                  label = "Escoger manualmente rango de eje y"),
                                    conditionalPanel(
                                        condition = "input.escala_man == 1",
                                        numericInput(inputId = "ymin",
                                                     label = "Rango mín. eje y",
                                                     value = -15),
                                        numericInput(inputId = "ymax",
                                                     label = "Rango máx. eje y",
                                                     value = 15)),
                                    # sliderInput(inputId = "rango_y",
                                    #             label = "Rango para eje y",
                                    #             min = -50,
                                    #             max = 800,
                                    #             value = c(-15,15))),
                                    checkboxGroupInput(inputId = "elec_latev",
                                                       label = "Electrodos",
                                                       choices = c("T5","T6","P3"),
                                                       selected = "T5"),
                                    checkboxInput(inputId = "rev_y",
                                                  label = "Invertir eje y"),
                                    radioButtons(inputId = "amp_late",
                                                 label = "Graficar amplitud o latencia",
                                                 choices = c("µV","ms"),
                                                 selected = "µV")
                                ),
                                mainPanel(
                                    
                                    plotOutput("latevolts")
                                )
                            ) 
                   ),
                   # * tabPanel Pre-post sin EEG -------------------
                   tabPanel(
                       "Pre-Post sin EEG",
                       navlistPanel(
                           tabPanel(
                               "TR y RC",
                               dropdownButton(
                                   tags$h4("Configuración de la gráfica"),
                                   
                                   circle  = TRUE,
                                   status  = "danger",
                                   icon    = icon("gear"),
                                   width   = "300px",
                                   tooltip = tooltipOptions(title = "Da clic para configurar la gráfica")
                                   
                               )
                           ),
                           tabPanel(
                               "Errores",
                           )
                       )
                   )
               )
               
               
    )
)

# para cargar actualizaciones ####
# rsconnect::deployApp("./eeg_plots")
