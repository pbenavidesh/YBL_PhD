# Data to use in app

df_norm <- read_csv("data/df_norm.csv")

participantes <- df_norm %>% 
  select(Sujeto, Grupo) %>% 
  distinct(Sujeto, .keep_all = T)