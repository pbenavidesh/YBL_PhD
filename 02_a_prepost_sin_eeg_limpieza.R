# Limpieza de datos prepost sin EEG


# sourcing scripts --------------------------------------------------------

source("00_Setup.R", encoding = "UTF-8")


# DATA --------------------------------------------------------------------

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


# eeg_medias --------------------------------------------------------------

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

# eeg_errores -------------------------------------------------------------

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


# eeg_errores_medias ------------------------------------------------------

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

