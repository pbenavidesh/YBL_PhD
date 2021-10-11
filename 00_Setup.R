# Doctorado YBL
# Setup

# pkgs --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(latex2exp)
library(scales)
library(gghighlight)
library(patchwork)

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
