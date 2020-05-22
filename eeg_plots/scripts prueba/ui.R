source("data.R")

# Define UI for application ####
ui <- fluidPage(
  
  # Application title
  titlePanel("PREs Proyecto YBL"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      # Selección de tiempo
      sliderInput(inputId = "tiempo",
                  "Horizonte de tiempo (ms)",
                  min = -100, max = 1022,
                  value = c(-100,1022)),
      
      # Selección de condición
      selectInput(inputId = "cond",
                  "Condición: ",
                  c("Alegría","Tristeza","Enojo",
                    "Identidad","Sexo")),
      
      # Selección de electrodo
      selectInput(inputId = "elec",
                  "Electrodo: ",
                  c("Fz","Cz","Pz","T5","T6")),
      
      # Análisis global o por participante
      radioButtons(
        inputId="radio",
        label="Selección de participantes:",
        choices=list(
          "Todos",
          "Selección manual"),
        selected = "Todos"
        
      ),
      
      # Selección de participante
      conditionalPanel(
        condition = "input.radio != 'Todos'",
        selectInput(inputId = "partic",
                    "Participante(s): ",
                    participantes$Sujeto)
      )
      
    ),
    
    # Panel principal de gráficos y tablas
    mainPanel(
      plotlyOutput("eeg"),
      
      tableOutput("picos")
    )
  )
)
