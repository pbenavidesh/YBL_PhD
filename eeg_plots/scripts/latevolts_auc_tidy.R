# Limpieza de datos de latevolts.xlsx


# pkgs --------------------------------------------------------------------

library("tidyverse")
library("readxl")


# data import -------------------------------------------------------------

df_auc <- read_excel("./data/latevolts_auc.xlsx",
                 sheet = "lat inicio P3b",
                 skip = 1)

nombres <- tibble(name = names(df_auc)[-c(1,2)],
                  cond = rep(c("Alegría-PRE",
                               "Alegría-POST",
                               "Tristeza-PRE",
                               "Tristeza-POST",
                               "Enojo-PRE",
                               "Enojo-POST",
                               "Identidad-PRE",
                               "Identidad-POST",
                               "Sexo-PRE",
                               "Sexo-POST"), 
                             each = 4
                             )
)

df_auc <- df_auc %>% 
  pivot_longer(cols = -c("código", "grupo")) %>% 
  left_join(nombres, by = "name") %>% 
  separate(cond, into = c("Condición",
                          "Evaluación")) %>% 
  mutate(name = str_remove_all(name,
                               "[0123456789]") %>% 
           str_replace(pattern = "LI", 
                       replacement = "li") ) %>% 
  pivot_wider(names_from = c("name"),
              values_from = "value") %>% 
  mutate(código = as_factor(código),
         Evaluación = factor(Evaluación, 
                             levels = c("PRE","POST")),
         Condición = factor(Condición,
                            levels = c("Identidad",
                                   "Sexo",
                                   "Alegría",
                                   "Tristeza",
                                   "Enojo")),
         grupo = as.character(grupo),
         grupo = str_replace_all(grupo, c("1" = "Emoción",
                                          "2" = "Identidad")),
         grupo = factor(grupo, levels = c("Identidad",
                                          "Emoción"))
         )

df_auc
rm(nombres)
