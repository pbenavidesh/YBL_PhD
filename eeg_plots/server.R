#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    Data <- reactive({
        if(input$radio == "Todos"){
            participantes$Sujeto    
        } else {
            filter(participantes,Sujeto %in% input$partic) %>%
                select(Sujeto) %>% 
                c()
            
        }
        
    })
    
    gg_df <- reactive({
        
        df <- df_norm %>%
            filter(Sujeto %in% Data()) 
        
        if (length(input$cond) != 1 | length(input$elec) != 1){
            df <- df %>% 
                filter(Condición %in% input$cond,
                       Electrodo %in% input$elec) %>% 
                {if(input$grupos){
                    group_by(., t, Grupo, Evaluación)
                } else {
                    group_by(., t, Evaluación)
                }} %>% 
                summarise(Amplitud = mean(value)) %>% 
                mutate(t_eval = str_c(t, Evaluación, 
                                      sep = "_"))
            
        } else {
            df <- df %>% 
                {if (input$grupos){
                    group_by(., t, Grupo, Condición,
                             Evaluación, Electrodo)
                } else{
                    group_by(., t, Condición,
                             Evaluación, Electrodo)
                }} %>% 
                summarise(Amplitud = mean(value)) %>% 
                filter(Condición %in% input$cond,
                       Electrodo %in% input$elec) %>% 
                mutate(t_eval = str_c(t, Evaluación, 
                                      sep = "_"))
            
        }
        df
        
        # df %>% 
        #     summarise(Amplitud = mean(value)) %>%
        #     filter(Condición %in% input$cond, 
        #            Electrodo %in% input$elec) %>% 
        #     filter(Condición == input$cond,
        #            Electrodo == input$elec) %>%
        #     mutate(t_eval = str_c(t, Evaluación, 
        #                           sep = "_"))
    })
    
    gg_df_auc <- reactive({
        
        df_norm %>%
            filter(Sujeto == input$partic_auc,
                   Electrodo == "Pz") %>% 
            group_by(t, Grupo, Condición,
                     Evaluación) %>% 
            summarise(Amplitud = mean(value)) %>%
            filter(Condición == input$cond_auc) %>% 
            mutate(t_eval = str_c(t, Evaluación, 
                                  sep = "_"))
    })
    
    auc_data <- reactive({
        df_auc %>% 
            filter(código == input$partic_auc,
                   Condición == input$cond_auc)
    })
    
    auc_data_pre <- reactive({
        auc_data() %>% 
            filter(Evaluación == "PRE") %>% 
            pull(li)
    }) 
    
    auc_data_post <- reactive({
        auc_data() %>% 
            filter(Evaluación == "POST") %>% 
            pull(li) 
    }) 
    
    gg_area_auc_pre <- reactive({
        gg_df_auc() %>% 
            filter(t %in% seq(auc_data_pre(),
                              auc_data_pre() + 200,
                              by = 2)&
                       Evaluación == "PRE")
    })
    
    gg_area_auc_post <- reactive({
        gg_df_auc() %>% 
            filter(t %in% seq(auc_data_post(),
                              auc_data_post() + 200,
                              by = 2)&
                       Evaluación == "POST")
    })
    
    picos_df <- reactive({
        gg_df() %>%
            select(-c(t_eval)) %>% 
            ungroup() %>%
            filter(t > 0) %>% 
            pivot_wider(names_from = "Evaluación",
                        values_from = "Amplitud") %>%
            select(-c(Condición, Electrodo))
    })
    
    picos_maxmin <- reactive({
        
        picos_global <- lapply(picos_df() %>% 
                                   select("PRE","POST"),findpeaks)
        
        if(input$elec %in% c("Fz","Cz","Pz")){
            tibble(t = c(picos_global$PRE[[1]] * 2,
                         picos_global$POST[[1]] * 2),
                   Amplitud = 
                       c(picos_global$PRE[[2]],
                         Amplitud_max = picos_global$POST[[2]]),
                   evaluación = c(rep("PRE",length(picos_global$PRE[[1]])),
                                  rep("POST",length(picos_global$POST[[1]]))),
                   t_eval = str_c(t,evaluación,sep = "_")
            ) %>% 
                filter(t %in% 300:800)
            
            
        } else {
            tibble(t = c(picos_global$PRE[[3]] * 2,
                         picos_global$POST[[3]] * 2),
                   Amplitud = 
                       c(picos_global$PRE[[4]],
                         picos_global$POST[[4]]),
                   evaluación = c(rep("PRE",length(picos_global$PRE[[3]])),
                                  rep("POST",length(picos_global$POST[[3]]))),
                   t_eval = str_c(t,evaluación,sep = "_")
            ) %>% 
                filter(t %in% 100:400)
            
        }
    })
    
    latevolts_reac <- reactive({
        latevolts %>%
            filter(electrodo %in% input$elec_latev,
                   var_late == input$amp_late)
    })
    
    
    output$eeg <- renderPlotly({
        
        gg <- gg_df() %>%
            
            ggplot(aes(x = t, y = Amplitud, 
                       color = Evaluación)) +
            
            scale_x_continuous(breaks = seq(-100,1000,
                                            by = 100))+
            theme_classic() + 
            theme(text = element_text(family = "serif", size = 14))+
            labs(x = "ms", y = "µV") +#plotly::TeX('$\\mu V$')
            scale_color_manual(values=c("PRE"= "turquoise3",
                                        "POST"="orchid2"),
                               name="")
        gplotly <- 
            
            if(n_distinct(gg_df()$Grupo) > 1){
                
                if(input$grupos == TRUE){
                    ggplotly(gg + geom_line(size = 1) +
                                 facet_wrap(~ Grupo, nrow = 1)+
                                 theme(strip.background = element_blank(),
                                       strip.placement = "outside")) %>% 
                        config(mathjax = 'cdn')
                } else {
                    ggplotly(gg + geom_line(size = 1)) %>% 
                        config(mathjax = 'cdn')
                }
                
            } else {
                ggplotly( if(input$vis_picos == FALSE){ gg +
                        geom_line(size = 1)} 
                        else { gg + geom_line(size = 1) + 
                                geom_text(data = picos_maxmin(),
                                          aes(x = t, y = Amplitud,
                                              size = 10,
                                              color = evaluación,
                                              label = str_c(t,sprintf("%0.2f", 
                                                                      round(Amplitud, digits = 2)),
                                                            sep = ", ")))
                        }
                ) %>% 
                    config(mathjax = 'cdn')
            }
        
        widgetframe::frameableWidget(gplotly)                        
        
        
    })
    
    output$auc_plot <- renderPlotly({
        gg <- gg_df_auc() %>%
            
            ggplot(aes(x = t, y = Amplitud, 
                       color = Evaluación)) +
            
            scale_x_continuous(breaks = seq(-100,1000,
                                            by = 100)) +
            theme_classic() + 
            theme(text = element_text(family = "serif", size = 14)) +
            labs(x = "ms", y = "µV") + #plotly::TeX('$\\mu V$')
            scale_color_manual(values=c("PRE"= "turquoise3",
                                        "POST"="orchid2"),
                               name="")
        ggplotly( gg + geom_line(size = 1) + 
                      geom_area(data = gg_area_auc_pre(), 
                                aes(x = t, y = Amplitud),
                                fill = "turquoise3", alpha = 0.4) +
                      geom_area(data = gg_area_auc_post(), 
                                aes(x = t, y = Amplitud),
                                fill = "orchid2", alpha = 0.4)
        )
    })
    
    output$auc <- renderTable({
        auc_pre <- DescTools::AUC(
            x = gg_area_auc_pre()$t, y =gg_area_auc_pre()$Amplitud,
            absolutearea = TRUE,
            method = "spline")
        auc_post <- DescTools::AUC(
            x = gg_area_auc_post()$t, y =gg_area_auc_post()$Amplitud,
            absolutearea = TRUE,
            method = "spline")
        tibble(Evaluación = c("PRE","POST"),
               Área = c(round(auc_pre,2),round(auc_post,2))
        )
        # paste("El área bajo la curva es: ","<br>", round(auc,2))
    })
    
    output$picos_pre <- renderTable({
        
        picos_maxmin() %>% 
            filter(evaluación =="PRE") %>% 
            select(-c(evaluación))
        
    })
    
    output$picos_post <- renderTable({
        
        picos_maxmin() %>% 
            filter(evaluación == "POST") %>% 
            select(-c(evaluación))
    })
    
    
    
    output$latevolts <- renderPlot({
        gg_late <- latevolts_reac() %>% 
            ggplot(aes(x = condición, y = value, fill = evaluación)) + 
            geom_bar(stat = "summary", fun = "mean", position = "dodge",
                     color = "black")+ xlab("") +
            ylab(input$amp_late) +
            facet_grid(electrodo ~ grupo, scales = "free", switch = "x") +
            scale_fill_manual(values= colores_prepost, name="") +
            theme_classic() + 
            theme(strip.background = element_blank(),
                  strip.placement = "outside") +
            stat_summary(geom = "errorbar",
                         fun.data = mean_se, 
                         position = position_dodge(width = 1), width = 0.5) +
            theme(text = element_text(family = "serif",
                                      size = 18))
        
        if(input$rev_y == TRUE){
            gg_late <- gg_late + scale_y_reverse()
        }
        
        if(input$escala_man == TRUE){
            gg_late + coord_cartesian(ylim = c(input$ymin,input$ymax))
        } else{gg_late}
    })

})
