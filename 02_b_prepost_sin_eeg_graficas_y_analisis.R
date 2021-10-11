# Gráficas y análisis prepost sin eeg


# sourcing scripts --------------------------------------------------------

source("02_a_prepost_sin_eeg_limpieza.R",encoding = "UTF-8")

# ERRORES ---------------------------------------------------------------
# * Errores funs ----------------------------------------------------------

eeg_errores_plot <- function(tipo_error = c("Comisión", "Omisión"),
                             grafico = c("violin", "boxplot"),
                             tipo_tarea){
  
  plot_type <- c(violin = geom_split_violin(size = 0.6), 
                 boxplot = geom_boxplot())
  
  p <- eeg_errores_medias %>% 
    filter(Tipo_error %in% tipo_error,
           Tipo_tarea == tipo_tarea) %>%
    ggplot(aes(x = Emoción, y = pct, fill = pre.post)) +
    facet_grid(Tipo_error ~ Grupo, switch = "both") +
    xlab("") + theme_classic() + ylab("%") +
    plot_type[grafico] + g +
    scale_fill_manual(values= colores_prepost, name="") +
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    point(width = 0.5) + tema + mediana + prom(width = 0.5) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)
  
  if(length(tipo_error) == 1){
    p + theme(strip.text.y = element_blank())
  } else{
    p
  }
}

eeg_errores_emo_plot <- function(tipo_error = c("Comisión", "Omisión"),
                                 tarea){
  p <- eeg_errores_medias %>% 
    filter(Tipo_error %in% tipo_error,
           Tarea == tarea) %>%
    ggplot(aes(x = Emoción, y = pct, fill = pre.post)) +
    facet_grid(Tipo_error ~ Grupo, switch = "both") +
    xlab("") + theme_classic() + ylab("%") +
    geom_split_violin(size = 0.6) + g +
    scale_fill_manual(values= colores_prepost, name="") +
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    point(width = 0.5) + tema + mediana + prom(width = 0.5) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)
  
  if(length(tipo_error) == 1){
    p + theme(strip.text.y = element_blank())
  } else{
    p
  }
}

eeg_errores_id_plot <- function(tipo_error = c("Comisión", "Omisión"),
                                tarea){
  p <- eeg_errores_medias %>% 
    filter(Tipo_error %in% tipo_error,
           Tarea == tarea) %>%
    ggplot(aes(x = Grupo, y = pct, fill = pre.post)) +
    facet_grid(vars(Tipo_error), switch = "both") +
    xlab("") + theme_classic() + ylab("%") +
    geom_split_violin(size = 0.6) + g +
    scale_fill_manual(values= colores_prepost, name="") +
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    point(width = 0.5) + tema + mediana + prom(width = 0.5) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)
  
  if(length(tipo_error) == 1){
    p + theme(strip.text.y = element_blank())
  } else{
    p
  }
}
#  * Errores ---------------------------------------------------------

eeg_errores_plot(tipo_tarea = "emp", grafico = "violin")
eeg_errores_plot(tipo_tarea = "emp", grafico = "boxplot")
# guardar("./pre post sin eeg/emp_errores.jpeg")

eeg_errores_plot(tipo_tarea = "mem", grafico = "violin")
eeg_errores_plot(tipo_tarea = "mem", grafico = "boxplot")
# guardar("./pre post sin eeg/mem_errores.jpeg")

eeg_errores_plot(tipo_tarea = "emp", tipo_error = "Comisión", 
                 grafico = "violin")
# guardar("./pre post sin eeg/emp_comision.jpeg")

eeg_errores_plot(tipo_tarea = "emp", tipo_error = "Omisión", 
                 grafico = "violin")
# guardar("./pre post sin eeg/emp_omision.jpeg")

eeg_errores_plot(tipo_tarea = "mem", tipo_error = "Comisión",
                 grafico = "violin")
# guardar("./pre post sin eeg/mem_comision.jpeg")

eeg_errores_plot(tipo_tarea = "mem", tipo_error = "Omisión",
                 grafico = "violin")
# guardar("./pre post sin eeg/mem_omision.jpeg")

# boxplot

eeg_errores_plot(tipo_tarea = "emp", grafico = "boxplot")
# guardar("./pre post sin eeg/emp_errores_box.jpeg")

eeg_errores_plot(tipo_tarea = "mem", grafico = "boxplot")
# guardar("./pre post sin eeg/mem_errores_box.jpeg")

eeg_errores_plot(tipo_tarea = "emp", tipo_error = "Comisión", 
                 grafico = "boxplot")
# guardar("./pre post sin eeg/emp_comision_box.jpeg")

eeg_errores_plot(tipo_tarea = "emp", tipo_error = "Omisión", 
                 grafico = "boxplot")
# guardar("./pre post sin eeg/emp_omision_box.jpeg")

eeg_errores_plot(tipo_tarea = "mem", tipo_error = "Comisión",
                 grafico = "boxplot")
# guardar("./pre post sin eeg/mem_comision_box.jpeg")

eeg_errores_plot(tipo_tarea = "mem", tipo_error = "Omisión",
                 grafico = "boxplot")
# guardar("./pre post sin eeg/mem_omision_box.jpeg")



#  * Errores x condición ---------------------------------------------

# Emoción
eeg_errores_emo_plot(tarea = "emp emoción")
# guardar("./pre post sin eeg/emp_emocion_errores.jpeg")

eeg_errores_emo_plot(tarea = "mem emoción")
# guardar("./pre post sin eeg/mem_emocion_errores.jpeg")

eeg_errores_emo_plot(tarea = "emp emoción", tipo_error = "Comisión")
# guardar("./pre post sin eeg/emp_emocion_comision.jpeg")

eeg_errores_emo_plot(tarea = "emp emoción", tipo_error = "Omisión")
# guardar("./pre post sin eeg/emp_emocion_omision.jpeg")

eeg_errores_emo_plot(tarea = "mem emoción", tipo_error = "Comisión")
# guardar("./pre post sin eeg/mem_emocion_comision.jpeg")

eeg_errores_emo_plot(tarea = "mem emoción", tipo_error = "Omisión")
# guardar("./pre post sin eeg/mem_emocion_omision.jpeg")


# Identidad
eeg_errores_id_plot(tarea = "emp identidad")
# guardar("./pre post sin eeg/emp_identidad_errores.jpeg")

eeg_errores_id_plot(tarea = "mem identidad")
# guardar("./pre post sin eeg/mem_identidad_errores.jpeg")

eeg_errores_id_plot(tarea = "emp identidad", tipo_error = "Comisión")
# guardar("./pre post sin eeg/emp_identidad_comision.jpeg")

eeg_errores_id_plot(tarea = "emp identidad", tipo_error = "Omisión")
# guardar("./pre post sin eeg/emp_identidad_omision.jpeg")

eeg_errores_id_plot(tarea = "mem identidad", tipo_error = "Comisión")
# guardar("./pre post sin eeg/mem_identidad_comision.jpeg")

eeg_errores_id_plot(tarea = "mem identidad", tipo_error = "Omisión")
# guardar("./pre post sin eeg/mem_identidad_omision.jpeg")


# RESULTADOS PREPOST SIN EEG ----------------------------------------------

# * Resultados funs -------------------------------------------------------

eeg_plots <- function(tarea = c("Emparejamiento", "Memoria"), 
                      variable = c("TR","RC"),
                      grafico = c("violin", "boxplot"),
                      condición = c("emocion", "identidad")){
  plot_type <- c(violin = geom_split_violin(), 
                 boxplot = geom_boxplot())
  
  p <- eeg_medias %>% 
    filter(
      Tarea == tarea,
      Variable == variable,
      Condición %in% condición
    ) %>% 
    ggplot(aes(x = Emoción, y = value, fill = pre.post)) +
    facet_wrap(~ Grupo, strip.position = "bottom") +
    xlab("") + theme_classic() + 
    scale_fill_manual(values=colores_prepost,
                      name="") +
    scale_color_manual(values=colores_prepost,
                       name="") +
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    g + plot_type[grafico] +
    point() + tema + mediana + prom()
  
  
  if(variable == "TR"){
    p + ylab("s") + scale_y_continuous(breaks = seq(1,5, by = 1))
  } else{
    p + ylab("%")
  }
}
#  * Emparejamiento emoción ------------------------------------------------
#   - Tiempos de reacción

eeg_plots(tarea = "Emparejamiento", variable = "TR", grafico = "violin")
# guardar("./pre post sin eeg/emp_TR.jpeg")

eeg_plots(tarea = "Emparejamiento", variable = "TR", grafico = "boxplot")
# guardar("./pre post sin eeg/emp_TR_box.jpeg")

#   - Respuestas correctas

eeg_plots(tarea = "Emparejamiento", variable = "RC", grafico = "violin")
# guardar("./pre post sin eeg/emp_RC.jpeg")

eeg_plots(tarea = "Emparejamiento", variable = "RC", grafico = "boxplot")
# guardar("./pre post sin eeg/emp_RC_box.jpeg")


#  * Memoria emoción -------------------------------------------------------

#   - Tiempos de reacción

eeg_plots(tarea = "Memoria", variable = "TR", grafico = "violin")
# guardar("./pre post sin eeg/mem_TR.jpeg")

eeg_plots(tarea = "Memoria", variable = "TR", grafico = "boxplot")
# guardar("./pre post sin eeg/mem_TR_box.jpeg")

#   - Respuestas correctas

eeg_plots(tarea = "Memoria", variable = "RC", grafico = "violin")
# guardar("./pre post sin eeg/mem_RC.jpeg")

eeg_plots(tarea = "Memoria", variable = "RC", grafico = "boxplot")
# guardar("./pre post sin eeg/mem_RC_box.jpeg")





# REGRESIÓN ---------------------------------------------------------------

# * Reg pkgs --------------------------------------------------------------

library(tidymodels)

# * Reg data --------------------------------------------------------------

eeg_medias_pre <- eeg_medias %>% 
  filter(
    Variable == "RC", 
    Tarea    == "Emparejamiento",
    pre.post == "Pre",
    Emoción  != "Identidad"
  ) %>% 
  rename(RC_pre = value)

eeg_medias_reg <- eeg_medias %>% 
  filter(
    Variable == "RC", 
    Tarea    == "Emparejamiento",
    pre.post == "Post",
    Emoción  != "Identidad"
  ) %>% 
  left_join(eeg_medias_pre,
            by = c("Grupo", "Tarea", "Condición", 
                   "Emoción", "Variable", "Niño"))

# Separar el desempeño en "Alto, Medio, Bajo"
# eeg_medias_reg %>% 
#   mutate(
#     result_pre = case_when(RC_pre < 50 ~ "Bajo",
#                            RC_pre < 70 ~ "Medio",
#                            TRUE        ~ "Alto")
#   ) %>% 
#   group_by(Grupo, Emoción) %>% 
#   summarise(
#     Pre = mean(RC_pre),
#     Post = mean(value),
#     n = n()
#   ) %>% view()

# df <- eeg_medias_reg %>% 
#   mutate(cambio_pct = value / RC_pre -1)






# Reg gráficas ------------------------------------------------------------
# Gráfica de acuerdo a niveles
p1 <- eeg_medias_reg %>%
  ggplot(aes(x = RC_pre, y = value,
             #shape = Grupo,
             color = Emoción,
             label = Niño)) +
  #geom_point(alpha = 0.6)
  geom_jitter() +
  # facet_wrap(~ Grupo) +
  geom_smooth(method = "lm",
              se = FALSE) +
  lims(x = c(0,100), y = c(0,100)) +
  labs(x = "RC - Pre", y = "RC - Post")

plotly::ggplotly(p1)


# * Reg config ------------------------------------------------------------

lm_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

lm_fit <- lm_model %>% 
  fit(value ~ RC_pre + Emoción + Grupo, data = eeg_medias_reg)

summary(lm_fit$fit)

# función de regresión

eeg_regresion <- function(
  variable = "RC",
  tarea    = "Emparejamiento",
  emocion  = c("Alegría", "Tristeza", "Enojo"),
  grupo    = c("Identidad", "Emoción")
){
  
  eeg_medias_pre <- eeg_medias %>% 
    filter(
      Variable == variable, 
      Tarea    == tarea,
      pre.post == "Pre",
      Emoción %in% emocion,
      Grupo   %in% grupo
    ) %>% 
    rename(RC_pre = value)
  
  eeg_medias_reg <- eeg_medias %>% 
    filter(
      Variable == variable, 
      Tarea    == tarea,
      pre.post == "Post",
      Emoción %in% emocion,
      Grupo   %in% grupo
    ) %>% 
    left_join(eeg_medias_pre,
              by = c("Grupo", "Tarea", "Condición", 
                     "Emoción", "Variable", "Niño"))
  
  lm_model <- linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  if (length(emocion)>1){
    lm_fit <- lm_model %>% 
      fit(value ~ Emoción + RC_pre, data = eeg_medias_reg)
  } else {
    lm_fit <- lm_model %>% 
      fit(value ~ RC_pre, data = eeg_medias_reg)
  }
  
  
  return(summary(lm_fit$fit))
  
}

# * Regresiones -----------------------------------------------------------


# Regresiones con grupos indistintos

eeg_regresion(emocion = "Alegría")
eeg_regresion(emocion = "Tristeza")
eeg_regresion(emocion = "Enojo")

# Regresiones separando por grupos

eeg_regresion(emocion = "Alegría", grupo = "Identidad")
eeg_regresion(emocion = "Alegría", grupo = "Emoción")
eeg_regresion(emocion = "Tristeza", grupo = "Identidad")
eeg_regresion(emocion = "Tristeza", grupo = "Emoción")
eeg_regresion(emocion = "Enojo", grupo = "Identidad")
eeg_regresion(emocion = "Enojo", grupo = "Emoción")

# otra regresión

eeg_medias_reg2 <- eeg_medias %>% 
  pivot_wider(names_from = c(pre.post, Variable), values_from = value) %>% 
  filter(
    Emoción != "Identidad",
    Tarea   == "Emparejamiento"
  )

lm_fit2 <- lm_model %>% 
  fit(Post_RC ~ Grupo + Emoción + Pre_RC + Post_TR, data = eeg_medias_reg2)

summary(lm_fit2$fit)

