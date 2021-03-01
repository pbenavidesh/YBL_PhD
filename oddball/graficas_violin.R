# Gráficas y análisis estadístico Doctorado YBL

# pkgs --------------------------------------------------------------------

library(easypackages)
libraries("tidyverse","ggpubr","latex2exp","scales", "gghighlight", "patchwork")

source("./oddball/geom_split_violin.R")

# Función para seleccionar variables de un df "varlist" -------------------

varlist <- function (df=NULL,type=c("numeric","factor",
                                    "character"),
                     pattern="", exclude=NULL) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}

# 0. Formato básico para todas las gráficas -------------------------------

# Tipografía y tamaño de letra
g <- theme(text = element_text(family = "serif",
                                        size = 16,
                               color = "black"))
# colores para evaluación pre y post
colores_prepost <- c("Pre"= "turquoise1",
                              "Post"="orchid1")
# para agregar puntos a la gráfica de oddball
point <- function(width = 0.5){
  geom_point(position = position_dodge(width = width),
             aes(fill = pre.post, group = pre.post), 
             shape = 21, color = "black")
}
# para agregar el tamaño de la leyenda
tema <- theme(legend.key.size = unit(0.2, "cm"), 
        legend.text = element_text(size = 12))  
# para agregar mediana a la gráfica
mediana <- stat_summary(fun = median, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..),
               position = position_dodge(width = 0.5),
               width = 0.5, size = 0.6, color ="black") 
# para agregar la media a la gráfica
prom <- function(width = 0.5) {
  stat_summary(fun = mean, aes(group = pre.post),
               geom = "point", 
               position = position_dodge(width = width),
               shape = 23, fill = "red", size = 2)
}
# para exportar la gráfica
guardar <- function(nombre, width = 200, height = 100,
                    units ="mm"){
  ggsave(nombre,
         width = width,height = height,units = units)
}

# 1. a) Limpieza de datos oddball -----------------------------------------

df <- read_csv("./oddball/oddball.csv", 
               locale = locale(encoding = "ISO-8859-1")) 
# Cambiar el texto
df <- df %>% 
  rename(frecuente.infrecuente = `frecuente/infrecuente`,
         pre.post = `pre/post`) %>%
  mutate(Grupo = 
           str_replace_all(Grupo,
                           c("Grupo emoción"   = "Emoción",
                             "Grupo identidad" = "Identidad")),
         Grupo = factor(Grupo, levels = c("Identidad",
                                          "Emoción")),
         pre.post = factor(pre.post, levels = c("Pre",
                                                "Post")),
         Actividad = factor(Actividad, 
                            levels = c("Identidad",
                                       "Sexo",
                                       "Alegría",
                                       "Tristeza",
                                       "Enojo")),
         grupo_eval = factor(str_c(as.character(Grupo),
                                   as.character(pre.post),
                                   sep = "-"),
                             levels = c("Identidad-Pre",
                                        "Emoción-Pre",
                                        "Identidad-Post",
                                        "Emoción-Post")),
         Tipo_error = case_when(
           Error == 0                         ~ "Acierto",
           Error == 100 & is.na(Respuesta.rt) ~ "Omisión",
           TRUE                               ~ "Comisión"
         )
  )


df_medias <- df %>%
  filter(frecuente.infrecuente == "Infrecuente") %>% 
  group_by(pre.post,Grupo,Actividad,Participante) %>%
  summarise(RC = mean(`Resp correcta`), 
            Tiempo_reaccion = 
              mean(`Tiempo respuesta`, na.rm = TRUE)) %>% 
  mutate(T_reaccion_ms = Tiempo_reaccion * 1000,
         grupo_eval = factor(str_c(as.character(Grupo),
                                   as.character(pre.post),
                                   sep = "-"),
                             levels = c("Identidad-Pre",
                                        "Emoción-Pre",
                                        "Identidad-Post",
                                        "Emoción-Post"))) %>% 
  unite(col = "Act_part", Actividad:Participante, remove = FALSE) %>% 
  mutate(Act_part = as_factor(Act_part))

# df_medias %>% 
#   unite(col = "Act_part", Actividad:Participante, remove = FALSE)

df_medias_errores <- df %>% 
  group_by(pre.post, Grupo, Actividad, 
           Participante, frecuente.infrecuente, 
           Tipo_error) %>%
  summarise(RC  = mean(`Resp correcta`),
            n   = n(),
            .groups = "drop_last") %>% 
  mutate(grupo_eval = factor(str_c(as.character(Grupo),
                                   as.character(pre.post),
                                   sep = "-"),
                             levels = c("Identidad-Pre",
                                        "Emoción-Pre",
                                        "Identidad-Post",
                                        "Emoción-Post")),
         pct = n / sum(n) * 100
  )


# df_medias_globales <- df_medias %>% 
#   group_by(pre.post,Grupo,Actividad) %>% 
#   summarise(RC = mean(RC),
#             Tiempo_reaccion = mean(Tiempo_reaccion))

# 1. b) Gráficas de violin oddball ####
gg <- ggplot(data = df_medias) +
  aes(x = Actividad, fill = pre.post)+ 
  facet_wrap(~Grupo, strip.position = "bottom") +
  xlab("") + theme_classic()+ 
  scale_fill_manual(values= colores_prepost, name="")+
  scale_color_manual(values= colores_prepost,name="")+
  theme(strip.background = element_blank(),
        strip.placement = "outside")


#   1. b.i) Tiempos de reacción ####
gg + g+ geom_split_violin(size = 0.6) +  aes(y = T_reaccion_ms) +
  ylab("ms") +
  point(width = 0.5) + tema + mediana + prom(width = 0.5)

guardar("./oddball/oddball_TR.jpeg")

# boxplot

gg + g + geom_boxplot() +  aes(y = T_reaccion_ms) +
  ylab("ms")+
  point(width = 0.75) + tema + prom(width = 0.75) +
  geom_line(aes(group = Act_part))

guardar("./oddball/oddball_box_TR.jpeg")

#   1. b.ii) Respuestas correctas ####
gg + g + geom_split_violin(size = 0.6) + aes(y = RC) + 
  ylab("%") + point() + tema + mediana + prom()
  
guardar("./oddball/oddball_RC.jpeg")

# boxplot
gg + g + geom_boxplot() + aes(y = RC) + 
  ylab("%") + point(width = 0.75) + tema + prom(width = 0.75)
  
guardar("./oddball/oddball_box_RC.jpeg")
    
#   1. b.iii) Errores ----

df_medias_errores %>% 
  select(-c(RC, n)) %>%
  pivot_wider(
    names_from  = Tipo_error,
    values_from = pct
  ) %>% 
  mutate_at(.vars = c("Acierto", "Comisión", "Omisión"),
            .funs = str_replace_na, replacement = 0)

df_medias_errores %>% 
  ggplot(aes(x = Actividad, y = RC, 
             fill = Tipo_error, group = pre.post)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~ Grupo)

# % de aciertos y errores por grupo, actividad, frec/infrec
df_medias_errores %>% 
  group_by(pre.post, Grupo, Actividad, 
           frecuente.infrecuente, Tipo_error) %>% 
  summarise(pct = mean(pct)) %>% 
  ggplot(aes(x = pre.post, y = pct,
             fill = Tipo_error)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(Grupo ~ Actividad + frecuente.infrecuente)

df_medias_errores %>% 
  filter(Tipo_error != "Acierto") %>%
  ggplot(aes(x = Actividad, y = pct, fill = pre.post)) +
  facet_grid(Tipo_error ~ Grupo, switch = "both") +
  xlab("") + theme_classic() + ylab("%") +
  geom_split_violin(size = 0.6) + g +
  scale_fill_manual(values= colores_prepost, name="") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  point(width = 0.5) + tema + mediana + prom(width = 0.5) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)

guardar("./oddball/oddball_errores.jpeg")

# 2. a) Limpieza de datos prepost sin eeg ####

eeg <- read_csv("./pre post sin eeg/prepost_sin_eeg.csv",
                locale = locale(encoding = "ISO-8859-1")) 
eeg <- eeg %>% 
  rename(pre.post = `pre/post`,
         emocion.emp = `emocion emp`,
         emocion.mem = `emocion mem`) %>% 
  mutate(Grupo = 
           str_replace_all(Grupo,
                           c("Grupo emoción"="Emoción",
                             "Grupo identidad"="Identidad")),
         Grupo = factor(Grupo, levels = c("Identidad",
                                          "Emoción")),
         pre.post = factor(pre.post, levels = c("Pre",
                                                "Post")),
         emocion.emp = factor(emocion.emp,
                              levels=c('Alegría',
                                       'Tristeza',
                                       'Enojo')),
         emocion.mem = factor(emocion.mem,
                              levels=c('Alegría',
                                       'Tristeza',
                                       'Enojo'))
         )
###

eeg_medias <- eeg %>% 
  select(Niño:Edad, pre.post, contains("TR"), contains("RC"), 
         emocion.emp, emocion.mem) %>%
  pivot_longer(
    cols = c(ends_with("RC"), ends_with("TR"))
  ) %>% 
  separate(name, into = c("Tarea", "Condición", "Variable")) %>% 
  mutate(
    Emoción = case_when(
      Condición == "identidad"  ~ "Identidad",
      Tarea == "Emparejamiento" ~ as.character(emocion.emp),
      TRUE                      ~ as.character(emocion.mem)
    ) %>% factor(levels = c("Identidad", "Alegría", "Tristeza", "Enojo"))
  ) %>% 
  select(-emocion.emp,-emocion.mem) %>% 
  group_by(pre.post, Grupo, Tarea, Condición, 
           Emoción, Variable, Niño) %>% 
  summarise(
    value   = mean(value, na.rm = TRUE),
    .groups = "drop_last"
  ) %>% 
  mutate(value = if_else(Variable == "RC", value * 100, value))



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

# Errores

eeg_errores <- eeg %>% 
  pivot_longer(
    cols      = starts_with("Tipo"),
    names_to  = "Tarea",
    values_to = "Tipo_error"
  ) %>% 
  select(-c(matches("[Comisión] | [Omisión]"))) %>% 
  mutate(
    Tarea   = str_replace(Tarea, "Tipo ", ""),
    Emoción = case_when(
      str_detect(Tarea, "identidad") ~ "Identidad",
      str_starts(Tarea, "emp")       ~ as.character(emocion.emp),
      TRUE                           ~ as.character(emocion.mem)
      ) %>% 
      factor(levels = c("Identidad", "Alegría", "Tristeza", "Enojo"))
  )

eeg_errores_medias <-  eeg_errores %>% 
  group_by(pre.post, Grupo, Tarea, Emoción,
           Niño, Tipo_error) %>% 
  summarise(
    n = n()
  ) %>% 
  mutate(
    pct = n / sum(n) * 100
  ) %>% 
  separate(Tarea, into = c("Tipo_tarea", "Condición"), remove = FALSE,
           sep = " ")

# Gráficas con ambas condiciones en una sola

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

eeg_errores_plot(tipo_tarea = "emp", grafico = "violin")
guardar("./pre post sin eeg/emp_errores.jpeg")

eeg_errores_plot(tipo_tarea = "mem", grafico = "violin")
guardar("./pre post sin eeg/mem_errores.jpeg")

eeg_errores_plot(tipo_tarea = "emp", tipo_error = "Comisión", 
                 grafico = "violin")
guardar("./pre post sin eeg/emp_comision.jpeg")

eeg_errores_plot(tipo_tarea = "emp", tipo_error = "Omisión", 
                 grafico = "violin")
guardar("./pre post sin eeg/emp_omision.jpeg")

eeg_errores_plot(tipo_tarea = "mem", tipo_error = "Comisión",
                 grafico = "violin")
guardar("./pre post sin eeg/mem_comision.jpeg")

eeg_errores_plot(tipo_tarea = "mem", tipo_error = "Omisión",
                 grafico = "violin")
guardar("./pre post sin eeg/mem_omision.jpeg")

# boxplot

eeg_errores_plot(tipo_tarea = "emp", grafico = "boxplot")
guardar("./pre post sin eeg/emp_errores_box.jpeg")

eeg_errores_plot(tipo_tarea = "mem", grafico = "boxplot")
guardar("./pre post sin eeg/mem_errores_box.jpeg")

eeg_errores_plot(tipo_tarea = "emp", tipo_error = "Comisión", 
                 grafico = "boxplot")
guardar("./pre post sin eeg/emp_comision_box.jpeg")

eeg_errores_plot(tipo_tarea = "emp", tipo_error = "Omisión", 
                 grafico = "boxplot")
guardar("./pre post sin eeg/emp_omision_box.jpeg")

eeg_errores_plot(tipo_tarea = "mem", tipo_error = "Comisión",
                 grafico = "boxplot")
guardar("./pre post sin eeg/mem_comision_box.jpeg")

eeg_errores_plot(tipo_tarea = "mem", tipo_error = "Omisión",
                 grafico = "boxplot")
guardar("./pre post sin eeg/mem_omision_box.jpeg")



# Gráficas separadas por condición
 
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
# Emoción
eeg_errores_emo_plot(tarea = "emp emoción")
guardar("./pre post sin eeg/emp_emocion_errores.jpeg")

eeg_errores_emo_plot(tarea = "mem emoción")
guardar("./pre post sin eeg/mem_emocion_errores.jpeg")

eeg_errores_emo_plot(tarea = "emp emoción", tipo_error = "Comisión")
guardar("./pre post sin eeg/emp_emocion_comision.jpeg")

eeg_errores_emo_plot(tarea = "emp emoción", tipo_error = "Omisión")
guardar("./pre post sin eeg/emp_emocion_omision.jpeg")

eeg_errores_emo_plot(tarea = "mem emoción", tipo_error = "Comisión")
guardar("./pre post sin eeg/mem_emocion_comision.jpeg")

eeg_errores_emo_plot(tarea = "mem emoción", tipo_error = "Omisión")
guardar("./pre post sin eeg/mem_emocion_omision.jpeg")


# Identidad
eeg_errores_id_plot(tarea = "emp identidad")
guardar("./pre post sin eeg/emp_identidad_errores.jpeg")

eeg_errores_id_plot(tarea = "mem identidad")
guardar("./pre post sin eeg/mem_identidad_errores.jpeg")

eeg_errores_id_plot(tarea = "emp identidad", tipo_error = "Comisión")
guardar("./pre post sin eeg/emp_identidad_comision.jpeg")

eeg_errores_id_plot(tarea = "emp identidad", tipo_error = "Omisión")
guardar("./pre post sin eeg/emp_identidad_omision.jpeg")

eeg_errores_id_plot(tarea = "mem identidad", tipo_error = "Comisión")
guardar("./pre post sin eeg/mem_identidad_comision.jpeg")

eeg_errores_id_plot(tarea = "mem identidad", tipo_error = "Omisión")
guardar("./pre post sin eeg/mem_identidad_omision.jpeg")


# 2. b) Gráficas de resultados prepost sin EEG ####
#   2. b.i) Emparejamiento emocion ####
#     2. b.i-a) Tiempos de reacción ####

eeg_plots(tarea = "Emparejamiento", variable = "TR", grafico = "violin")
guardar("./pre post sin eeg/emp_TR.jpeg")

eeg_plots(tarea = "Emparejamiento", variable = "TR", grafico = "boxplot")
guardar("./pre post sin eeg/emp_TR_box.jpeg")

#     2. b.i-b) Respuestas correctas ####

eeg_plots(tarea = "Emparejamiento", variable = "RC", grafico = "violin")
guardar("./pre post sin eeg/emp_RC.jpeg")

eeg_plots(tarea = "Emparejamiento", variable = "RC", grafico = "boxplot")
guardar("./pre post sin eeg/emp_RC_box.jpeg")


#   2. b.ii) Memoria emoción ####
#     2. b.ii-a) Tiempos de reacción ####

eeg_plots(tarea = "Memoria", variable = "TR", grafico = "violin")
guardar("./pre post sin eeg/mem_TR.jpeg")

eeg_plots(tarea = "Memoria", variable = "TR", grafico = "boxplot")
guardar("./pre post sin eeg/mem_TR_box.jpeg")

#     2. b.ii-b) Respuestas correctas ####

eeg_plots(tarea = "Memoria", variable = "RC", grafico = "violin")
guardar("./pre post sin eeg/mem_RC.jpeg")

eeg_plots(tarea = "Memoria", variable = "RC", grafico = "boxplot")
guardar("./pre post sin eeg/mem_RC_box.jpeg")


# 2. c) REGRESIÓN ---------------------------------------------------------

library(tidymodels)

# Los datos 

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

# ARTÍCULO ----------------------------------------------------------------

eeg_errores_medias_wide <- eeg_errores_medias %>% 
  select(-n) %>%
  drop_na() %>%
  pivot_wider(
    names_from = Tipo_error, 
    values_from = pct
  ) %>% 
  mutate(
    Tarea = str_replace_all(
      Tipo_tarea,
      c("emp" = "Emparejamiento", 
        "mem" = "Memoria")
    )
  ) %>% 
  select(-Tipo_tarea)

eeg_medias_wide <- eeg_medias %>% 
  mutate(
    Condición = str_replace(Condición, "emocion", "emoción")
  ) %>% 
  pivot_wider(
    names_from = Variable,
    values_from = value
  )

art_eeg_tbl <- eeg_medias_wide %>% 
  left_join(eeg_errores_medias_wide) %>% 
  mutate(
    Grupo = fct_recode(
      Grupo,
      `Identity Group` = "Identidad",
      `Emotion Group`  = "Emoción"
    ),
    Emoción = fct_recode(
      Emoción,
      Identity  = "Identidad",
      Happiness = "Alegría",
      Sadness   = "Tristeza",
      Anger     = "Enojo"
    ),
    pre.post = fct_recode(
      pre.post,
      `Pre-`  = "Pre",
      `Post-` = "Post"
    )
  ) %>% 
  select(-Acierto) %>% 
  pivot_longer(
    cols      = RC:Comisión,
    names_to  = "Variable",
    values_to = "valor"
  ) %>% 
  mutate(
    Variable = str_replace_all(
      Variable, c("RC" = "Correct Responses (%)", "Comisión" = "Errors (%)",
                  "TR" = "Response Times (s)")
    ),
    Variable = factor(Variable, levels = c("Correct Responses (%)", 
                                           "Errors (%)", "Omisión", 
                                           "Response Times (s)"))
  )
  


art_colores_prepost <- c("Pre-"= "turquoise1",
                         "Post-"="orchid1")

art_eeg_plot <- function(variable, 
                         grafico = c("violin", "boxplot"),
                         tarea){
  
  plot_type <- c(violin = geom_split_violin(size = 0.6), 
                 boxplot = geom_boxplot(
                   outlier.color = "black",
                   outlier.shape = 1,
                   outlier.size  = 0.7
                 ))
  
  p <- art_eeg_tbl %>% 
    filter(Variable %in% variable,
           Tarea == tarea) %>%
    ggplot(aes(x = Emoción, y = valor, fill = pre.post)) +
    facet_grid(Variable ~ Grupo, switch = "both", scales = "free_y") +
    xlab("") + theme_classic() + ylab("") +
    plot_type[grafico] + g +
    scale_fill_manual(values= art_colores_prepost, name="") +
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    mediana +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
    theme(text = element_text(family = "sans",
                              size = 12, color = "black"),
          legend.justification = "top",
          plot.title = element_text(hjust = 0.5),
          axis.ticks.x = element_blank()) +
    stat_compare_means(method = "wilcox.test",
                       paired = TRUE,
                       label = "p.signif",
                       hide.ns = TRUE)
  
  if(length(variable) == 1){
    p + theme(strip.text.y = element_blank())
  } else{
    p
  }
  
  # if(tarea == "Emparejamiento"){
  #   p + ggtitle("Matching Tasks")
  # } else{
  #   p + ggtitle("Memory Tasks")
  # }
}


art_eeg_plot(
  variable = c("Correct Responses (%)",
               "Errors (%)",
               "Response Times (s)"), 
  tarea    = "Emparejamiento", 
  grafico  = "boxplot"
)
# guardar("./Artículo/Emp_RC_Comision_TR.jpeg",
#         height = 542, width = 598, 
#         units = "mm")

art_eeg_plot(
  variable = c("Correct Responses (%)",
               "Errors (%)",
               "Response Times (s)"), 
  tarea    = "Memoria", 
  grafico  = "boxplot"
)

# COMPARE MEANS

art_eeg_tbl %>% 
  compare_means(
    formula =  valor ~ pre.post, 
    paired  = TRUE,
    group.by = c("Variable", "Grupo", "Tarea", 
                 "Condición", "Emoción")
  ) %>% 
  filter(p.signif != "ns") %>% view()

# Acomodo rectangular de gráficas
art_eeg_plot2 <- function(variable, 
                          grafico = c("violin", "boxplot"),
                          tarea){
  
  plot_type <- c(violin = geom_split_violin(size = 0.6), 
                 boxplot = geom_boxplot(
                   outlier.color = "black",
                   outlier.shape = 1,
                   outlier.size  = 0.7
                 ))
  
  p <- art_eeg_tbl %>% 
    filter(Variable %in% variable,
           Tarea == tarea) %>%
    ggplot(aes(x = Emoción, y = valor, fill = pre.post)) +
    facet_grid(Variable ~ Grupo, switch = "both", scales = "free_y") +
    xlab("") + theme_classic() + ylab("") +
    plot_type[grafico] + g +
    scale_fill_manual(values= art_colores_prepost, name="") +
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    mediana +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
    theme(text = element_text(family = "sans",
                              size = 12, color = "black"),
          legend.justification = "top",
          plot.title = element_text(hjust = 0.5),
          axis.ticks.x = element_blank()) +
    stat_compare_means(method = "wilcox.test",
                       paired = TRUE,
                       label = "p.signif",
                       hide.ns = TRUE)
  
  if(length(variable) == 1){
    p + theme(strip.text.y = element_blank())
  } else{
    p
  }
  
  # if(tarea == "Emparejamiento"){
  #   p + ggtitle("Matching Tasks")
  # } else{
  #   p + ggtitle("Memory Tasks")
  # }
}

p1 <- art_eeg_plot2(
  variable = c("Correct Responses (%)",
               "Errors (%)"), 
  tarea    = "Emparejamiento", 
  grafico  = "boxplot"
) + theme(legend.position = "none")

p2 <- art_eeg_plot2(
  variable = "Response Times (s)", 
  tarea    = "Emparejamiento", 
  grafico  = "boxplot"
) + 
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        strip.text.x = element_blank())

p1 | (p2/plot_spacer())


p1 <- art_eeg_plot2(
  variable = c("Correct Responses (%)",
               "Errors (%)"), 
  tarea    = "Memoria", 
  grafico  = "boxplot"
) + theme(legend.position = "none")

p2 <- art_eeg_plot2(
  variable = "Response Times (s)", 
  tarea    = "Memoria", 
  grafico  = "boxplot"
) + 
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        strip.text.x = element_blank())

p1 | (p2/plot_spacer())

# 3. a) Limpieza de datos películas y SSIS ####
# Base de datos de entrenamiento emoción
bd_train <- read_csv("./películas/bd_train_emocion.csv",
                  locale = locale(encoding = "ISO-8859-1"))

bd_train <- bd_train %>% 
  rename(Grupo = `Gpo. Etto.`,
         Código = `Codigo participante`) %>% 
  mutate(Grupo = str_replace_all(Grupo,
                                 c("Grupo emoción"="Emoción",
                                   "Grupo identidad"="Identidad")),
         Grupo = factor(Grupo, levels = c("Identidad",
                                          "Emoción"))
         ) %>% 
  rename_all(~sub(" 1","_Pre",.x)) %>% 
  rename_all(~sub(" 2","_Post",.x)) %>% 
  select(Código, Grupo, contains("Pre") ,contains("Post"))

# Solo los datos de películas y SSIS
pelis <- bd_train %>% 
  select(Código, Grupo, contains("Peliculas"),SSIS_Pre,SSIS_Post,
         `Emo peli_Pre`,`Emo peli_Post`)

#Sin considerar niño outlier EDC10M
# pelis <- pelis %>% 
#   filter(Código != "EDC10M")

pelis_long <- pelis %>% 
  mutate(Peliculas_Pre = Peliculas_Pre * 100,
         Peliculas_Post = Peliculas_Post * 100) %>% 
  pivot_longer(cols = c(Peliculas_Pre:`Emo peli_Post`), 
               names_to = "pre.post", values_to = "valor") %>% 
  separate(pre.post, into = c("Tipo","pre.post"), sep = "_") %>% 
  mutate(pre.post = factor(pre.post, levels = c("Pre","Post"))) %>% 
  group_by(pre.post,Grupo,Tipo,Código)
  
# 3. b) Gráficas películas y SSIS ####
gg_peli <- function(graf = "Peliculas"){
  # pelis_long %>% 
  #   filter(Tipo == Tipo) %>% 
    ggplot(pelis_long %>% filter(Tipo == graf),
           mapping = aes(x = Grupo, y = valor, 
                         fill = pre.post)) +
    # facet_wrap(~Tipo, strip.position = "bottom") +
    # scale_fill_discrete(name = "") +
    xlab("") + theme_classic() + 
    scale_fill_manual(values = colores_prepost, name ="") +
    scale_color_manual(values = colores_prepost, name ="")#+
  #theme(strip.background = element_blank(),
  #      strip.placement = "outside")
}
  
#   3. b.i) Películas ####
gg_peli() + g + tema + 
  geom_split_violin(size = 0.6) +
  point() + mediana + prom() + ylab("%")
  
guardar("./películas/Peliculas.jpeg")

#   3. b.ii) Gráficas de SSIS ####

gg_peli(graf = "SSIS") + g + 
  geom_split_violin(size = 0.6) +  
  point() + mediana + prom() + ylab("Puntuación")

guardar("./películas/SSIS.jpeg")

#   3. b.iii) Gráficas de Emo pelis ####

gg_peli(graf = "Emo peli") + g + 
  geom_split_violin(size = 0.6) +  
  point() + mediana + prom() + ylab("%") +
  tema

guardar("./películas/Emo pelis.jpeg")

# 4. a) Limpieza Subescalas de SSIS ####
SSIS <- bd_train %>%
  select(Código, Grupo, contains("SSIS"),-c(SSIS_Pre,SSIS_Post)) %>% 
  filter(Código != "EDC10M")
  
SSIS_long <- SSIS %>% 
  pivot_longer(cols = -c(Código,Grupo)) %>% 
  separate(name, into = c("SSIS","clave","pre.post")) %>% 
  mutate(pre.post = factor(pre.post, levels = c("Pre","Post")),
         clave = as.factor(clave))

subescalas <- tibble(clave=levels(SSIS_long$clave)) %>% 
  mutate(Subescala = c("Autocontrol","Asertividad",
                          "Bullying","Compromiso",
                          "Comunicación","Cooperación",
                          "Empatía","Externalización",
                          "Hiperactividad","Internalización",
                          "PC",
                          "Responsabilidad"),
         escala = c(rep("Habilidades sociales",2),
                    "Problemas de conducta",
                    rep("Habilidades sociales", 4),
                    rep("Problemas de conducta", 4),
                    "Habilidades sociales"))

SSIS_long <- SSIS_long %>% 
  left_join(subescalas, by = "clave") %>% 
  mutate(Subescala = factor(Subescala))

rm(subescalas)

# 4. b) SSIS por escalas y subescalas ####
gg_ssis <- function(escalas, angulo = 45){
  ggplot(SSIS_long %>% filter(escala == escalas),
         mapping = aes(x = Subescala, y = value, 
                       fill = pre.post)) +
    facet_wrap(~Grupo, strip.position = "bottom") +
    xlab("") + theme_classic() + 
    scale_fill_manual(values = colores_prepost, name ="") +
    scale_color_manual(values = colores_prepost, name ="") +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x =  element_text(size = 8, angle = angulo,
                                      hjust = 1)) +
    ylab("Puntuación")
}

gg_ssis("Habilidades sociales") + g + 
  geom_split_violin(size = 0.6) +
  point() + mediana + prom()

guardar("./películas/SSIS_hs.jpeg")

gg_ssis("Problemas de conducta") + g + 
  geom_split_violin(size = 0.6) +
  point() + mediana + prom() +
  scale_y_continuous( breaks = seq(0,60, by = 20))

guardar("./películas/SSIS_pc.jpeg")

# 5. a) Limpieza de datos pico máximo P3 ####

path_picos <- "./PCA/toolkit/"
pico_p3 <- read_tsv(paste(path_picos,
                          "amplitud p3/pico maximo/",
                          "pico maximo pz.txt",
                          sep = ""), col_names = F,
                    skip = 7)

pico_p3_vars <- read_tsv(paste(path_picos,
                               "amplitud p3/pico maximo/",
                               "pico maximo pz.txt",
                               sep = ""), col_names = F,
                         skip = 5) %>% slice(1)

colnames(pico_p3_vars) <- NULL

pico_p3_vars <- c(unlist(pico_p3_vars),"participante")

colnames(pico_p3) <- pico_p3_vars

condición_names <- c("IA" = "Alegría",
               "IT" = "Tristeza",
               "IE" = "Enojo",
               "ID" = "Identidad",
               "IS" = "Sexo")

condición_levels <- c("Identidad",
                      "Sexo",
                      "Alegría",
                      "Tristeza",
                      "Enojo")

pico_p3_long <- pico_p3 %>% 
  pivot_longer(-c(grupo, participante),
               names_to = "name", values_to = "Amplitud") %>% 
  separate(name,into = c("pre.post","condición"),sep = 1) %>%
  mutate(pre.post = str_replace_all(pre.post,
                                    c("1"= "Pre",
                                      "2" = "Post")) %>% 
           factor(levels = c("Pre","Post")),
         condición = str_replace_all(condición, condición_names),
         condición = factor(condición,
                            levels = condición_levels),
         Amplitud = replace(Amplitud, Amplitud == -999,NA),
         grupo = str_replace_all(grupo,c("1" = "Emoción",
                                         "2" = "Identidad")) %>% 
           factor(levels = c("Identidad","Emoción")))
   
pico_p3_medias <- pico_p3_long %>% 
  group_by(pre.post,grupo,condición) %>%
  summarise(Amplitud = mean(Amplitud,na.rm = T))
  


# 5. b) Gráficas de pico máximo P3 ####
gg_picop3 <- ggline(pico_mediasp3,x="pre_post",y="Amplitud",
       color="Condición",legend.title="")

gg_picop3 + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
                  strip.placement = "outside",
        text = element_text(family = "serif"))
ggsave("pico_max_p3.jpeg")

# 6. a) Limpieza de datos pico máximo T5 ####

pico_t5 <- read.delim("t5 pico maximo.txt",header = F,
                      sep = "\t",skip = 7,as.is = T)
pico_t5_vars <- read.delim("t5 pico maximo.txt",header = F,
                           sep = "\t",skip = 5,as.is = T,
                           nrows = 1)
colnames(pico_t5_vars) <- NULL
pico_t5_vars <- c(unlist(pico_t5_vars),"participante")
colnames(pico_t5) <- pico_t5_vars
pico_t5_long <- gather(pico_t5,Condición,Amplitud,
                       -c(grupo,participante))
pico_t5_long$pre_post <-as.factor(
  ifelse(substring(pico_t5_long$Condición,
                   1,1)=="1","Pre","Post"))
pico_t5_long$pre_post <- relevel(pico_t5_long$pre_post,"Pre")
pico_t5_long$Condición <- substring(
  pico_t5_long$Condición,2)
pico_t5_long$Condición <- pico_t5_long$Condición %>%
  str_replace_all(c("IA" = "Alegría",
                    "IT" = "Tristeza",
                    "IE" = "Enojo",
                    "ID" = "Identidad",
                    "IS" = "Sexo")) %>% 
  factor(levels = c("Alegría",
                    "Tristeza",
                    "Enojo",
                    "Identidad",
                    "Sexo"))

# Quitar valores omitidos
pico_t5_long$Amplitud <- ifelse(pico_t5_long$Amplitud==-999,
                                NA,pico_t5_long$Amplitud)
pico_mediast5 <- pico_t5_long %>% 
  group_by(pre_post,grupo,Condición) %>%
  summarise(Amplitud = mean(Amplitud,na.rm = T))

pico_mediast5$grupo <- ifelse(pico_mediast5$grupo==1,"Emoción",
                              "Identidad")
pico_mediast5$grupo <- as.factor(pico_mediast5$grupo)

# 6. b) Gráficas de pico máximo T5 ####
gg_picot5 <- ggline(pico_mediast5,x="pre_post",y="Amplitud",
                    color="Condición",legend.title="")

gg_picot5 + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_t5.jpeg")


# 7. a) Limpieza de datos pico máximo T6 ####

pico_t6 <- read.delim("t6 pico maximo.txt",header = F,
                      sep = "\t",skip = 7,as.is = T)
pico_t6_vars <- read.delim("t6 pico maximo.txt",header = F,
                           sep = "\t",skip = 5,as.is = T,
                           nrows = 1)
colnames(pico_t6_vars) <- NULL
pico_t6_vars <- c(unlist(pico_t6_vars),"participante")
colnames(pico_t6) <- pico_t6_vars
pico_t6_long <- gather(pico_t6,Condición,Amplitud,
                       -c(grupo,participante))
pico_t6_long$pre_post <-as.factor(
  ifelse(substring(pico_t6_long$Condición,
                   1,1)=="1","Pre","Post"))
pico_t6_long$pre_post <- relevel(pico_t6_long$pre_post,"Pre")
pico_t6_long$Condición <- substring(
  pico_t6_long$Condición,2)
pico_t6_long$Condición <- pico_t6_long$Condición %>%
  str_replace_all(c("IA" = "Alegría",
                    "IT" = "Tristeza",
                    "IE" = "Enojo",
                    "ID" = "Identidad",
                    "IS" = "Sexo")) %>% 
  factor(levels = c("Alegría",
                    "Tristeza",
                    "Enojo",
                    "Identidad",
                    "Sexo"))

# Quitar valores omitidos
pico_t6_long$Amplitud <- ifelse(pico_t6_long$Amplitud==-999,
                                NA,pico_t6_long$Amplitud)
pico_mediast6 <- pico_t6_long %>% 
  group_by(pre_post,grupo,Condición) %>%
  summarise(Amplitud = mean(Amplitud,na.rm = T))

pico_mediast6$grupo <- ifelse(pico_mediast6$grupo==1,"Emoción",
                              "Identidad")
pico_mediast6$grupo <- as.factor(pico_mediast6$grupo)

# 7. b) Gráficas de pico máximo T6 ####
gg_picot6 <- ggline(pico_mediast6,x="pre_post",y="Amplitud",
                    color="Condición",legend.title="")

gg_picot6 + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_t6.jpeg")



# 8. a) Limpieza de datos pico máximo n2 ####

pico_n2 <- read.delim("N2 pre post.txt",header = F,
                      sep = "\t",skip = 7,as.is = T)
pico_n2_vars <- read.delim("N2 pre post.txt",header = F,
                           sep = "\t",skip = 5,as.is = T,
                           nrows = 1)
colnames(pico_n2_vars) <- NULL
pico_n2_vars <- c(unlist(pico_n2_vars[1:31]),"participante")

pico_n2_elec <- read.delim("N2 pre post.txt",header = F,
                           sep = "\t",skip = 6,as.is = T,
                           nrows = 1)
colnames(pico_n2_elec) <- NULL
pico_n2_elec <- unlist(pico_n2_elec)

pico_n2_vars_elec <- paste(pico_n2_vars,pico_n2_elec,sep = "_")

colnames(pico_n2) <- pico_n2_vars_elec
pico_n2_long <- gather(pico_n2,Condición,Amplitud,
                       -c(grupo_spec,participante_NA))
pico_n2_long$pre_post <-as.factor(
  ifelse(substring(pico_n2_long$Condición,
                   1,1)=="1","Pre","Post"))
pico_n2_long$pre_post <- relevel(pico_n2_long$pre_post,"Pre")
pico_n2_long$electrodo <- substring(pico_n2_long$Condición,5)
pico_n2_long$Condición <- substring(
  pico_n2_long$Condición,2,3)
pico_n2_long$Condición <- pico_n2_long$Condición %>%
  str_replace_all(c("IA" = "Alegría",
                    "IT" = "Tristeza",
                    "IE" = "Enojo",
                    "ID" = "Identidad",
                    "IS" = "Sexo")) %>% 
  factor(levels = c("Alegría",
                    "Tristeza",
                    "Enojo",
                    "Identidad",
                    "Sexo"))

# Quitar valores omitidos
pico_n2_long$Amplitud <- ifelse(pico_n2_long$Amplitud==-999,
                                NA,pico_n2_long$Amplitud)
pico_mediasn2 <- pico_n2_long %>% 
  group_by(pre_post,grupo_spec,Condición,electrodo) %>%
  summarise(Amplitud = mean(Amplitud,na.rm = T))

pico_mediasn2$grupo <- ifelse(pico_mediasn2$grupo_spec==1,
                              "Emoción","Identidad")
pico_mediasn2$grupo <- as.factor(pico_mediasn2$grupo)


# 8. b) Gráficas de pico máximo n2 ####

# gg_picon2 <- facet_wrap(~grupo,strip.position = "bottom")+
#   theme_classic() + xlab("")+ 
#   ylab(TeX('Amplitud  ($\\mu$V)'))+
#   theme(strip.background = element_blank(),
#         strip.placement = "outside",
#         text = element_text(family = "serif"))+
#   coord_cartesian(ylim = c(-13,0))
gg_picon2_Cz <- ggline(filter(pico_mediasn2,electrodo=="Cz"),
                    x="pre_post",y="Amplitud",
                    color="Condición",legend.title="")
gg_picon2_Cz + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_n2_Cz.jpeg")

gg_picon2_Fz <- ggline(filter(pico_mediasn2,electrodo=="Fz"),
                       x="pre_post",y="Amplitud",
                       color="Condición",legend.title="")
gg_picon2_Fz + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_n2_Fz.jpeg")

gg_picon2_Pz <- ggline(filter(pico_mediasn2,electrodo=="Pz"),
                       x="pre_post",y="Amplitud",
                       color="Condición",legend.title="")
gg_picon2_Pz + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_n2_Pz.jpeg")


# # # # # # # # # # # # # # # # # # ----
# P R U E B A
# # # # # # # # # # # # # # # # # #
source("vioplot2.R")
gg + g+ geom_split_violin(draw_quantiles = c(0.5)) +
  aes(y = T_reaccion_ms) +
  ylab("ms")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  stat_summary(fun.y = mean,
               geom = "point",shape=4,size=8)
ggsave("prueba_oddball_TR.jpeg",width = 800,height = 400,
       units = "mm")

# # # # # # # # # # # # # # # # # # 

