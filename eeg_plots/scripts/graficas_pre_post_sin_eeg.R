# Pre post sin EEG
# Archivo para cargar, limpiar y crear funciones de gráficas
# Pablo Benavides-Herrera
# 2020-11-17

# pkgs --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(latex2exp)
library(scales)
library(gghighlight)


# scripts -----------------------------------------------------------------

source("scripts/geom_split_violin.R")

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

# Formato básico para todas las gráficas ----------------------------------

# Tipografía y tamaño de letra
g <- theme(text = element_text(family = "serif",
                               size = 16))
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

# DATA --------------------------------------------------------------------

eeg <- read_csv("data/prepost_sin_eeg.csv",
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


# * eeg - medias ----------------------------------------------------------

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


# Función para crear los gráficos de EEG
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



# * eeg - errores ---------------------------------------------------------

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
                             tipo_tarea){
  p <- eeg_errores_medias %>% 
    filter(Tipo_error %in% tipo_error,
           Tipo_tarea == tipo_tarea) %>%
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



