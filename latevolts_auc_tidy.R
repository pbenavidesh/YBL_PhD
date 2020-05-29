# Limpieza de datos de latevolts.xlsx


# pkgs --------------------------------------------------------------------

library(easypackages)
libraries("tidyverse","readxl")


# data import -------------------------------------------------------------

df <- read_excel("latevolts_auc.xlsx",
                 sheet = "lat inicio P3b",
                 skip = 1)

nombres <- tibble(name = names(df)[-c(1,2)],
                  cond = rep(c("Alegría-PRE",
                               "Alegría-POST",
                               "Tristeza-PRE",
                               "Tristeza-POST",
                               "Enojo-PRE",
                               "Enojo-POST",
                               "Identidad-PRE",
                               "Identidad-POST",
                               "Sexo-PRE",
                               "Sexo-Post"), 
                             each = 4
                             )
)

df_tidy <- df %>% 
  pivot_longer(cols = -c("código", "grupo")) %>% 
  left_join(nombres, by = "name") %>% 
  separate(cond, into = c("Condición",
                          "Evaluación")) %>% 
  mutate(name = str_remove_all(name,
                               "[0123456789]") %>% 
           str_replace(pattern = "LI", 
                       replacement = "li") ) %>% 
  pivot_wider(names_from = c("name"),
              values_from = "value")
