# Resultados pre post (sin EEG)
# Pablo Benavides-Herrera
# 2020-09-01
# 


# pkgs --------------------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------

df <- readxl::read_xlsx("./resultados pre-post sep20/Resultados pre-post (sin EEG).xlsx",
                  sheet = "pre-post pbh",
                  trim_ws = TRUE) %>% 
  select(-c(last_col(offset = 2):last_col()))

# tidy data ---------------------------------------------------------------

df_tidy <- df %>% 
  # discard unnecesary variables
  select(Niño:Edad, 
         `emocion emp`:`Tipo emp emoción`, 
         `Tipo mem emoción`, Imagen) %>% 
  rename(emocion_emp = `emocion emp`,
         emocion_mem = `emocion mem`,
         resultado_emp = `Tipo emp emoción`,
         resultado_mem = `Tipo mem emoción`) %>% 
  pivot_longer(cols = c(emocion_emp, emocion_mem, 
                        resultado_emp, resultado_mem), 
               names_to = c(".value", "tipo"),
               # values_to = c("Emoción", "Resultado"),
               names_sep = "_") %>% 
  mutate_if(is.character,as_factor)


# Analysis ----------------------------------------------------------------

df_tidy %>% 
  mutate(correcto = if_else(resultado == "Acierto",1,0)) %>%
  filter(`pre/post`== "Post") %>% 
  group_by(`pre/post`,Imagen,tipo,emocion) %>% 
  summarise(n = mean(correcto)) %>% 
  arrange(n)
