
# Define server logic ####
server <- function(input, output) {
  
  # Definir si el análisis se realiza global o por sujeto
  # sujetos_input <- reactive({
  #   if(input$radio == "Todos"){
  #     participantes$Sujeto    
  #   } else {
  #     filter(participantes,Sujeto %in% input$partic) %>%
  #       select(Sujeto) %>% 
  #       c()
  #     
  #   }
  #   
  # })
  
  # Actualiza si hay cambios en tipo de análisis (global o 
  # por participante) y calcula tabla
  
    
  
  sujetos_input <- reactive({
    if(input$radio == "Todos"){
      participantes$Sujeto    
    } else {
      filter(participantes,
                        Sujeto %in% input$partic) %>%
        select(Sujeto) %>% 
        c()
      
    }
    df_norm %>%
      filter(Sujeto %in% sujetos_input()) %>% 
      group_by(t, Grupo,Condición,
               Evaluación, Electrodo) %>% 
      summarise(Amplitud = mean(value))
  })
  
  
  #gg_df
  # df_input <- reactive({
  #   df_norm %>%
  #     filter(Sujeto %in% sujetos_input()) %>% 
  #     group_by(t, Grupo,Condición,
  #              Evaluación, Electrodo) %>% 
  #     summarise(Amplitud = mean(value)) 
  #     })
    
  # Filtra tabla de acuerdo a condición, electrodo y t
    df_final_input <- reactive({
      sujetos_input() %>% 
        filter(t>=input$tiempo[1] && t <=input$tiempo[2],
               Condición == input$cond, 
               Electrodo == input$elec)
    })
      
  
  
  # Gráfica de PREs ####
  output$eeg <- renderPlotly({
    
    # Filtrado de datos reactivo
    # gg_df <-
    #   
    #   df_norm %>%
    #   filter(Sujeto %in% sujetos_input()) %>% 
    #   group_by(t, Grupo,Condición,
    #            Evaluación, Electrodo) %>% 
    #   summarise(Amplitud = mean(value)) %>%
    #   filter(t>=input$tiempo[1] && t <=input$tiempo[2],
    #          Condición == input$cond, 
    #          Electrodo == input$elec)
    
    # Gráfica
    # gg <- gg_df() 
    
    # gg <- gg %>% 
      gg <- 
      ggplot(df_final_input(),aes(x = t, y = Amplitud, 
                 color = Evaluación)) +
      geom_line() +
      scale_x_continuous(breaks = seq(-100,1000,
                                      by = 100))+
      theme_classic() + 
      theme(text = element_text(family = "serif"))
    
    # Condición para segmentar por facets o no   
    if(n_distinct(sujetos_input$Grupo) > 1){
      ggplotly(gg + facet_wrap(~ Grupo, nrow = 1)+
                 theme(strip.background = element_blank(),
                       strip.placement = "outside"))    
    } else {
      ggplotly(gg)
    }
    
  })
  
  # Tabla para ver picos
  output$picos <- renderTable({
    df_final_input
  })
}

