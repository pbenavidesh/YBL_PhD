# Limpieza de datos de los registros de EEG
# de participantes

# df_norm <- read_csv("./eeg_plots/data/df_norm.csv")

# 0. Carga de paqueterías ####
library("easypackages")
packages("tidyverse","plotly","gghighlight")

# 1. Carga de archivos ####
var_names <- read_csv("./PCA/nombres_variables.csv",
                      col_types = "c")

file_names <- list.files(path = "./PCA/datos/")

participantes <- readxl::read_excel(
  "./eeg_plots/data/graficas_PRES2.xlsx",
    sheet = "participantes x grupo") %>%
  rename(Sujeto = Participante) %>% 
  mutate(Grupo = factor(Grupo, 
                        levels = c("Grupo emoción",
                                   "Grupo identidad"))) 
  

# 2. Carga de datos de EEG ####

datos <- tibble(filename = file_names) %>% 
  mutate(file_contents = 
           map(filename,
               ~ read_delim(file.path("./PCA/datos",.),
                            delim = "\t",
                            escape_double = FALSE,
                            col_names = pull(var_names,nombres),
                            trim_ws = TRUE,
                            col_types = cols(.default = "d")))
  ) %>% 
  unnest(c(file_contents))

# Quitar datos innecesarios
rm(list = c("var_names","file_names"))

datos <- datos %>% 
  mutate(filename =  str_to_upper(datos$filename)) %>% 
  mutate(filename = str_replace_all(datos$filename,
                                    c(".TXT" = "",
                                      ".txt" = "",
                                      "POST2" = "POST"))) %>% 
  mutate(evaluacion = if_else(
    str_detect(datos$filename,"POST",
                                         negate = T),
                              "PRE","NA")) %>% 
  mutate(evaluacion = na_if(evaluacion,"NA")) %>%  
  unite(name, filename,evaluacion, na.rm = T)  %>% 
  separate(name,into = c("Condición","Sujeto","Evaluación"),
           sep = "_")
  
# 3. Transformaciones ####
# Mantener solo los electrodos necesarios

linea_media <- datos %>% 
  select(-c(HEOG,VEOG)) %>% # Todos los electrodos
  # select(Sujeto, Evaluación, Condición, Fz, Cz, Pz, T5, T6) %>% 
  mutate(t = rep(seq(-100,1022, by = 2),trunc(n()/562,0)),
         segmento = trunc((row_number()-1)/562,0))

electrodos <- linea_media %>% 
  select(-c(Condición:Evaluación,t,segmento)) %>% 
  names()
# calcular las medias por segmento del tiempo negativo
df_medias <- linea_media %>% 
  filter(t < 0) %>%
  pivot_longer(cols = -c(Condición:Evaluación,t,segmento), 
               names_to = "Electrodo") %>% 
  group_by(segmento, Condición, Sujeto, Evaluación, 
           Electrodo) %>% 
  summarise(value = -1 * mean(value))

# Agregar las medias a cada segmento en linea_media
df <- linea_media %>% 
  pivot_longer(cols = -c(Condición:Evaluación,t,segmento), 
               names_to = "Electrodo") %>% 
  left_join(df_medias, by = c("segmento","Electrodo")) %>% 
  mutate(value = value.x + value.y) %>%
  select(-c(Condición.y, Sujeto.y, Evaluación.y,
            value.x, value.y)) %>% 
  pivot_wider(names_from = "Electrodo", 
              values_from = "value") %>% 
  select(-segmento)

# Quitar datos intermedios
rm(list = c("linea_media","df_medias"))

# Calcular media por sujeto, condición y evaluación
# y cambiar t a columnas

df_norm <- df %>% 
  pivot_longer(cols = -c(Condición.x:t), 
               names_to = "Electrodo") %>%
  mutate(Condición = factor(Condición.x,
                            levels = c("IA","IT","IE","IID",
                                       "IS"),
                            labels = c("Alegría","Tristeza",
                                       "Enojo","Identidad",
                                       "Sexo")),
         Evaluación = factor(Evaluación.x,
                             levels = c("PRE","POST")),
         Electrodo = factor(Electrodo, 
                            levels = electrodos)
         ) %>%
  rename(Sujeto = Sujeto.x) %>% 
  select(-c("Condición.x","Evaluación.x")) %>% 
  group_by(t, Condición, Evaluación, Sujeto, Electrodo) %>% 
  summarise(value = mean(value)) %>% 
  left_join(participantes, by = "Sujeto")




# #3.1 Exportar datos a Excel --------------------------------------------------
# Para uso de Yermein en Excel
# df_norm_excel <- df_norm %>%
#   pivot_wider(names_from = "t",
#               values_from = "value")
# 
# write_excel_csv(df_norm_excel, path = "df_norm_r.csv")
# 
# rm(df_norm_excel)
# 
# # Para uso de Shiny app
# write_excel_csv(df_norm, path = "df_norm.csv")



# 4. Gráficas ----------------------------------------------------------------
# Gráfica de todas las condiciones y evaluaciones
df_norm %>%
  group_by(t, Condición, Evaluación, Electrodo) %>% 
  summarise(media = mean(value)) %>% 
  ggplot(aes(x = t, y = media, color = Electrodo)) +
  geom_line() + facet_grid(rows = vars(Evaluación),
                           cols = vars(Condición)) + 
  theme_classic() + theme(text = element_text(family = "serif"),
                          strip.background = element_blank(),
                          strip.placement = "outside")

gg_eeg <- df_norm %>%
  group_by(t, Condición, Evaluación, Electrodo) %>% 
  summarise(media = mean(value)) %>%
  filter(Condición == "Alegría",
         Electrodo == "Pz") %>% 
  ggplot(aes(x = t, y = media)) +
  geom_line(aes(color = Evaluación)) + theme_classic() +
    theme(text = element_text(family = "serif"),
                          strip.background = element_blank(),
                          strip.placement = "outside") 
  ggplotly(gg_eeg)
  
  
readxl::read_excel("./eeg_plots/graficas_PRES.xlsx"
                   ,sheet = "participantes x grupo")

df %>%
  rename(Sujeto = Sujeto.x) %>% 
  left_join(participantes, by = "Sujeto")


# Gráfica estilo eeglab de cada electrodo por grupo y condición

df_plots <- df_norm %>%
  filter(t <= 800) %>% 
  group_by(t, Grupo, Condición, Evaluación, Electrodo) %>% 
  summarise(media = mean(value)) 

df_plots_nest <- df_plots %>% ungroup() %>% 
  group_by(Grupo,Condición,Evaluación) %>% 
  nest()

df_plots %>% 
  filter(Grupo == "Grupo emoción" &
           Condición =="Alegría") %>% 
  ggplot(aes(x = t, y = media, color = Evaluación)) +
  geom_line() + facet_wrap(~ Electrodo) +
  theme_void() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(-10,30))
  

for (i in seq_along(levels(df_plots$Grupo))){
  for (j in seq_along(levels(df_plots$Condición))){
    df_plots %>% 
      filter(Grupo == levels(Grupo)[i] &
               Condición == levels(Condición)[j]) %>% 
      ggplot(aes(x = t, y = media, color = Evaluación)) +
      geom_line() + facet_wrap(~ Electrodo) + 
      theme(text = element_text(family = "serif"),
            strip.background = element_blank(),
            strip.placement = "outside",
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank()
      ) + ggtitle(paste()) 
  }
}




# Plot function

PREs_plot <- function(grupo,condición){
  p <- df_plots %>% 
    filter(Grupo == grupo &
             Condición == condición) %>% 
    ggplot(aes(x = t, y = media, color = Evaluación)) +
    geom_line() + facet_wrap(~ Electrodo) + 
    theme(text = element_text(family = "serif"),
          strip.background = element_blank(),
          strip.placement = "outside",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank()
          ) +
    ggtitle(paste("Distribución de los PREs del", grupo, "en la condición",
                  condición))
  ggplotly(p)
}


plot_variants <- expand.grid(levels(df_plots$Grupo),
                              levels(df_plots$Condición)
 )
names(plot_variants) <- c("Grupo","Condición")

# Generar todas las gráficas

plots_pres <- map2(plot_variants$Grupo, plot_variants$Condición,
                   PREs_plot)

guardar <- function(nombre, width = 200, height = 100,
                    units ="mm"){
  ggsave(nombre,
         width = width,height = height,units = units)
}

paths <- str_c(plot_variants %>% unite("plot") %>% pull(),".jpeg")


pwalk(list(paths, plots_pres), ggsave)


# plots <- mtcars %>% 
#   split(.$cyl) %>% 
#   map(~ggplot(., aes(mpg, wt)) + geom_point())
# paths <- stringr::str_c(names(plots), ".pdf")


plots_pres[[1]]

# Siempre no se necesitó esto ####
PREs_plot2 <- function(grupo,condición,electrodos){
  p <- df_plots %>% 
    filter(Grupo == grupo &
             Condición == condición &
             Electrodo %in% electrodos) %>% 
    ggplot(aes(x = t, y = media, color = Evaluación)) +
    geom_line() + facet_wrap(~ Electrodo, nrow = 1, strip.position = "bottom") +
    theme_void() + 
    theme(legend.position = "none")+
    coord_cartesian(ylim = c(-10,30))
  p
}

p1 <- PREs_plot2("Grupo emoción","Alegría",c("Fpz"))
p2 <- PREs_plot2("Grupo emoción","Alegría",c("F3","Fz","F4"))
p3 <- PREs_plot2("Grupo emoción","Alegría",c("T3","C3","Cz","C4","T4"))

(plot_spacer() | plot_spacer() | p1 | plot_spacer() | plot_spacer() )/ 
  (plot_spacer() | p2 | plot_spacer()) /
  (p3)

