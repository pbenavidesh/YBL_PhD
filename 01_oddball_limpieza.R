# Limpieza de datos oddball


# sourcing scripts --------------------------------------------------------

source("00_Setup.R", encoding = "UTF-8")


# DATA --------------------------------------------------------------------

# * Import ----------------------------------------------------------------

df <- read_csv("./oddball/oddball.csv", 
               locale = locale(encoding = "ISO-8859-1")) 


# * Wrangling -------------------------------------------------------------

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


# * Medias ----------------------------------------------------------------

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


# * Errores ---------------------------------------------------------------

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
