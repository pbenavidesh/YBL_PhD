# 1. pkgs -----------------------------------------------------------------

library("shiny")
library("shinythemes")
library("shinyWidgets")
library("tidyverse")
library("plotly")
library("latex2exp")
library("gghighlight")
library("widgetframe")
library("DescTools")


# 2. SCRIPTS --------------------------------------------------------------

source("scripts/findpeaks.R") # function to find local peaks
source("scripts/geom_split_violin.R")
source("scripts/latevolts_auc_tidy.R",
       encoding = "UTF-8") # data preprocessing for AUC
# source("scripts/graficas_pre_post_sin_eeg.R",
#        encoding = "UTF-8") # plots for pre-post without EEG

# 3. DATA -----------------------------------------------------------------


# Tab PREs
df_norm <- read_csv("data/df_norm.csv")

df_norm <- df_norm %>% 
  mutate(Grupo = str_replace(Grupo, "Grupo ",""),
         Grupo = factor(Grupo %>% str_to_sentence(), 
                        levels = c("Identidad",
                                   "Emoción")))

participantes <- df_norm %>% 
  select(Sujeto, Grupo) %>% 
  distinct(Sujeto, .keep_all = T)

# Tab AUC


# Tab Resultados conductuales

# oddball <- read_csv("data/oddball.csv")
# pre_post <- read_csv("data/pre_post_sin_eeg.csv")
# bd_entrenamiento <- read_csv("data/bd_entrenamiento_emocion.csv")

# Tab Amplitudes y latencias

latevolts <- read_csv("data/latevolts.csv",
                      locale = locale(encoding = "ISO-8859-1"))%>% 
  mutate(grupo = str_replace_all(grupo,c("1" = "Emoción",
                                         "2" = "Identidad")) %>% 
           factor(levels = c("Identidad", "Emoción")),
         evaluación = factor(evaluación, 
                             levels = c("PRE","POST")),
         condición = factor(condición, 
                            levels = c("Identidad","Sexo",
                                       "Alegría", "Tristeza",
                                       "Enojo"))) %>% 
  pivot_longer(cols = c("µV","ms"),names_to = "var_late")
colores_prepost <- c("PRE"= "turquoise1",
                     "POST"="orchid1")
