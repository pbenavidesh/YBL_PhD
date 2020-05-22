# Gráficas y análisis estadístico Doctorado YBL

# Carga de paqueterías ####
library(easypackages)
libraries("tidyverse","ggpubr","latex2exp","scales", "gghighlight")

source("./oddball/geom_split_violin.R")

# Función para seleccionar variables de un df "varlist" ####
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
# 0. Formato básico para todas las gráficas ####
# Tipografía y tamaño de letra
g <- theme(text = element_text(family = "serif",
                                        size = 12))
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
        legend.text = element_text(size = 8))  
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
 
# 1. a) Limpieza de datos oddball ####
df <- read_csv("./oddball/oddball.csv", 
               locale = locale(encoding = "ISO-8859-1")) 
# Cambiar el texto
df <- df %>% 
  rename(frecuente.infrecuente = `frecuente/infrecuente`,
         pre.post = `pre/post`) %>%
  mutate(Grupo = 
           str_replace_all(Grupo,
                           c("Grupo emoción"="Emoción",
                             "Grupo identidad"="Identidad")),
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
                                        "Emoción-Post"))) %>% 
  as_tibble()


df_medias <- df %>%
  filter(frecuente.infrecuente == "Infrecuente") %>% 
  group_by(pre.post,Grupo,Actividad,Participante) %>%
  summarise(RC = mean(`Resp correcta`), 
            Tiempo_reaccion = 
              mean(`Tiempo respuesta`, na.rm = T)) %>% 
  mutate(T_reaccion_ms = Tiempo_reaccion * 1000,
         grupo_eval = factor(str_c(as.character(Grupo),
                                   as.character(pre.post),
                                   sep = "-"),
                             levels = c("Identidad-Pre",
                                        "Emoción-Pre",
                                        "Identidad-Post",
                                        "Emoción-Post")))

# df_medias_globales <- df_medias %>% 
#   group_by(pre.post,Grupo,Actividad) %>% 
#   summarise(RC = mean(RC),
#             Tiempo_reaccion = mean(Tiempo_reaccion))

# 1. b) Gráficas de violin oddball ####
gg <- ggplot(data = df_medias) +
  aes(x = Actividad, fill = pre.post)+ 
  facet_wrap(~Grupo, strip.position = "bottom") +
  xlab("") + theme_classic()+ 
  scale_fill_manual(values= colores_prepost, name="")+
  scale_color_manual(values= colores_prepost,name="")+
  theme(strip.background = element_blank(),
        strip.placement = "outside")


#   1. b.i) Tiempos de reacción ####
gg + g+ geom_split_violin(size = 0.6) +  aes(y = T_reaccion_ms) +
  ylab("ms") +
  point(width = 0.5) + tema + mediana + prom(width = 0.5)

guardar("./oddball/oddball_TR.jpeg")

# boxplot

gg + g + geom_boxplot() +  aes(y = T_reaccion_ms) +
  ylab("ms")+
  point(width = 0.75) + tema + prom(width = 0.75)

guardar("./oddball/oddball_box_TR.jpeg")

#   1. b.ii) Respuestas correctas ####
gg + g + geom_split_violin(size = 0.6) + aes(y = RC) + 
  ylab("%") + point() + tema + mediana + prom()
  
guardar("./oddball/oddball_RC.jpeg")

# boxplot
gg + g + geom_boxplot() + aes(y = RC) + 
  ylab("%") + point(width = 0.75) + tema + prom(width = 0.75)
  
guardar("./oddball/oddball_box_RC.jpeg")
    

# 2. a) Limpieza de datos prepost sin eeg ####

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

eeg_medias_emp <- eeg %>% 
  group_by(pre.post,Grupo,emocion.emp,Niño) %>%
  summarise(emp_em_RC = mean(Emparejamiento_emocion_RC)*100, 
        emp_em_TR= mean(Emparejamiento_emocion_TR, na.rm = T),
        mem_em_RC=mean(Memoria_emocion_RC)*100,
        mem_em_TR=mean(Memoria_emocion_TR,na.rm = T),
        emp_id_RC=mean(Emparejamiento_identidad_RC)*100,
        emp_id_TR=mean(Emparejamiento_identidad_TR,na.rm = T),
        mem_id_RC=mean(Memoria_identidad_RC)*100,
        mem_id_TR=mean(Memoria_identidad_TR,na.rm = T)) 

eeg_medias_mem <- eeg %>% 
  group_by(pre.post,Grupo,emocion.mem,Niño) %>%
  summarise(mem_em_RC=mean(Memoria_emocion_RC)*100,
            mem_em_TR=mean(Memoria_emocion_TR,na.rm = T))

eeg_medias_id <- eeg %>% 
  group_by(pre.post,Grupo,Niño) %>%
  summarise(emp_id_RC=mean(Emparejamiento_identidad_RC)*100,
          emp_id_TR=mean(Emparejamiento_identidad_TR,na.rm=T),
          mem_id_RC=mean(Memoria_identidad_RC)*100,
          mem_id_TR=mean(Memoria_identidad_TR,na.rm = T))

# 2. b) Gráficas de resultados prepost sin EEG ####
#   2. b.i) Emparejamiento emocion ####
gg_emp <- ggplot(data = eeg_medias_emp) +
  aes(x = emocion.emp, fill = pre.post)+ 
  facet_wrap(~Grupo, strip.position = "bottom") +
  # scale_fill_discrete(name = "") +
  xlab("") + theme_classic()+ 
  scale_fill_manual(values=colores_prepost,
                    name="")+
  scale_color_manual(values=colores_prepost,
                     name="")+
  theme(strip.background = element_blank(),
        strip.placement = "outside")

#     2. b.i-a) Tiempos de reacción ####
gg_emp + g + geom_split_violin()+  aes(y = emp_em_TR) + 
  ylab("s")+ point() + tema + mediana + prom() +
  scale_y_continuous( breaks = seq(1,5, by = 1))
guardar("./pre post sin eeg/emp_em_TR.jpeg")

# boxplot
gg_emp + g + geom_boxplot()+ aes(y = emp_em_TR) +
  ylab("s")+ point(width = 0.75) + tema + prom(width = 0.75) +
  scale_y_continuous( breaks = seq(1,5, by = 1))
guardar("./pre post sin eeg/box_emp_em_TR.jpeg")

#     2. b.i-b) Respuestas correctas ####
gg_emp + g + geom_split_violin() +  aes(y = emp_em_RC) +
  ylab("%") + point() + tema + mediana + prom()
  
  
guardar("./pre post sin eeg/emp_em_RC.jpeg")

# boxplot
gg_emp + g + geom_boxplot() +  aes(y = emp_em_RC) +
  ylab("%") + point() + tema + prom()
  
guardar("./pre post sin eeg/box_emp_em_RC.jpeg")

#   2. b.ii) Memoria emoción ####
gg_mem <- ggplot(data = eeg_medias_mem) +
  aes(x = emocion.mem, fill = pre.post)+ 
  facet_wrap(~Grupo, strip.position = "bottom") +
  xlab("") + theme_classic()+ 
  scale_fill_manual(values= colores_prepost,
                    name="")+
  scale_color_manual(values= colores_prepost,
                     name="")+
  theme(strip.background = element_blank(),
        strip.placement = "outside")

#     2. b.ii-a) Tiempos de reacción ####
gg_mem + g + geom_split_violin() + aes(y = mem_em_TR) +
  ylab("s")+ point() + tema + mediana + prom() +
  scale_y_continuous( breaks = seq(0,2, by = 0.5))
  
guardar("./pre post sin eeg/mem_em_TR.jpeg")

gg_mem + g + geom_boxplot() + aes(y = mem_em_TR) +
  ylab("s")+ point(width = 0.75) + tema + prom(width = 0.75) +
  scale_y_continuous( breaks = seq(0,2, by = 0.5))

guardar("./pre post sin eeg/box_mem_em_TR.jpeg")

#     2. b.ii-b) Respuestas correctas ####
gg_mem + g + geom_split_violin() + aes(y = mem_em_RC) +
  ylab("%") + point() + tema + mediana + prom()
  
guardar("./pre post sin eeg/mem_em_RC.jpeg")

gg_mem + g + geom_boxplot() + aes(y = mem_em_RC) +
  ylab("%") + point(0.75) + tema + prom(0.75)
  
guardar("./pre post sin eeg/box_mem_em_RC.jpeg")

#   2. b.iii) Emparejamiento identidad ####
gg_id <- ggplot(data = eeg_medias_id) +
  aes(x = Grupo, fill = pre.post)+ 
  xlab("") + theme_classic()+ 
  scale_fill_manual(values= colores_prepost,
                    name="")+
  scale_color_manual(values= colores_prepost,
                     name="")

#     2. b.iii-a) Emparejamiento ####
# Tiempos de reacción
gg_id + g + geom_split_violin() +  aes(y = emp_id_TR) + 
  ylab("s")+ point() + tema + mediana + prom()
  
guardar("./pre post sin eeg/emp_id_TR.jpeg")

# boxplot
gg_id + g + geom_boxplot() +  aes(y = emp_id_TR) + 
  ylab("s")+ point(0.75) + tema + prom(0.75) 
  
guardar("./pre post sin eeg/box_emp_id_TR.jpeg")

# Respuestas correctas
gg_id + g + geom_split_violin() +  aes(y = emp_id_RC) +
  ylab("%") + point() + tema + mediana + prom() +
  scale_y_continuous( breaks = seq(0,100, by = 25))
  # coord_cartesian(ylim = c(10,100))
  
guardar("./pre post sin eeg/emp_id_RC.jpeg")

gg_id + g + geom_boxplot() +  aes(y = emp_id_RC) +
  ylab("%") + point(0.75) + tema + prom(0.75) +
  scale_y_continuous( breaks = seq(0,100, by = 25))
  
guardar("./pre post sin eeg/box_emp_id_RC.jpeg")

#     2. b.iii-b) Memoria ####
# Tiempos de reacción
gg_id + g + geom_split_violin() + aes(y = mem_id_TR) +
  ylab("s")+ point() + tema + mediana + prom() +
  scale_y_continuous( breaks = seq(0,2, by = 0.5))+
  coord_cartesian(ylim = c(0.25,1.5))
guardar("./pre post sin eeg/mem_id_TR.jpeg")

gg_id + g + geom_boxplot() + aes(y = mem_id_TR) +
  ylab("s")+ point(0.75) + tema + prom(0.75) + 
  scale_y_continuous( breaks = seq(0,2, by = 0.5))+
  coord_cartesian(ylim = c(0.25,1.5))
  
guardar("./pre post sin eeg/box_mem_id_TR.jpeg")

# Respuestas correctas
gg_id + g + geom_split_violin() + aes(y = mem_id_RC) +
  ylab("%") + 
  point() + tema + mediana + prom() +
  scale_y_continuous( breaks = seq(0,100, by = 25))

guardar("./pre post sin eeg/mem_id_RC.jpeg")

# boxplot
gg_id + g + geom_boxplot() + aes(y = mem_id_RC) +
  ylab("%") + point(0.75) + tema + prom(0.75) +
  scale_y_continuous( breaks = seq(0,100, by = 25))

guardar("./pre post sin eeg/box_mem_id_RC.jpeg")

# 3. a) Limpieza de datos películas y SSIS ####
# Base de datos de entrenamiento emoción
bd_train <- read_csv("./películas/bd_train_emocion.csv",
                  locale = locale(encoding = "ISO-8859-1"))

bd_train <- bd_train %>% 
  rename(Grupo = `Gpo. Etto.`,
         Código = `Codigo participante`) %>% 
  mutate(Grupo = str_replace_all(Grupo,
                                 c("Grupo emoción"="Emoción",
                                   "Grupo identidad"="Identidad")),
         Grupo = factor(Grupo, levels = c("Identidad",
                                          "Emoción"))
         ) %>% 
  rename_all(~sub(" 1","_Pre",.x)) %>% 
  rename_all(~sub(" 2","_Post",.x)) %>% 
  select(Código, Grupo, contains("Pre") ,contains("Post"))

# Solo los datos de películas y SSIS
pelis <- bd_train %>% 
  select(Código, Grupo, contains("Peliculas"),SSIS_Pre,SSIS_Post)

#Sin considerar niño outlier EDC10M
pelis <- pelis %>% 
  filter(Código != "EDC10M")

pelis_long <- pelis %>% 
  mutate(Peliculas_Pre = Peliculas_Pre * 100,
         Peliculas_Post = Peliculas_Post * 100) %>% 
  pivot_longer(cols = c(Peliculas_Pre, Peliculas_Post,
                        SSIS_Pre, SSIS_Post), 
               names_to = "pre.post", values_to = "valor") %>% 
  separate(pre.post, into = c("Tipo","pre.post"), sep = "_") %>% 
  mutate(pre.post = factor(pre.post, levels = c("Pre","Post"))) %>% 
  group_by(pre.post,Grupo,Tipo,Código)
  
# 3. b) Gráficas películas y SSIS ####
gg_peli <- function(graf = "Peliculas"){
  # pelis_long %>% 
  #   filter(Tipo == Tipo) %>% 
    ggplot(pelis_long %>% filter(Tipo == graf),
           mapping = aes(x = Grupo, y = valor, 
                         fill = pre.post)) +
    # facet_wrap(~Tipo, strip.position = "bottom") +
    # scale_fill_discrete(name = "") +
    xlab("") + theme_classic() + 
    scale_fill_manual(values = colores_prepost, name ="") +
    scale_color_manual(values = colores_prepost, name ="")#+
  #theme(strip.background = element_blank(),
  #      strip.placement = "outside")
}
  
#   3. b.i) Películas ####
gg_peli() + g + tema + 
  geom_split_violin(size = 0.6) +
  point() + mediana + prom() + ylab("%")
  
guardar("./películas/Peliculas.jpeg")

#   3. b.ii) Gráficas de SSIS ####

gg_peli(graf = "SSIS") + g + 
  geom_split_violin(size = 0.6) +  
  point() + mediana + prom() + ylab("Puntuación")

guardar("./películas/SSIS.jpeg")



# 4. a) Limpieza Subescalas de SSIS ####
SSIS <- bd_train %>%
  select(Código, Grupo, contains("SSIS"),-c(SSIS_Pre,SSIS_Post)) %>% 
  filter(Código != "EDC10M")
  
SSIS_long <- SSIS %>% 
  pivot_longer(cols = -c(Código,Grupo)) %>% 
  separate(name, into = c("SSIS","clave","pre.post")) %>% 
  mutate(pre.post = factor(pre.post, levels = c("Pre","Post")),
         clave = as.factor(clave))

subescalas <- tibble(clave=levels(SSIS_long$clave)) %>% 
  mutate(Subescala = c("Autocontrol","Asertividad",
                          "Bullying","Compromiso",
                          "Comunicación","Cooperación",
                          "Empatía","Externalización",
                          "Hiperactividad","Internalización",
                          "PC",
                          "Responsabilidad"),
         escala = c(rep("Habilidades sociales",2),
                    "Problemas de conducta",
                    rep("Habilidades sociales", 4),
                    rep("Problemas de conducta", 4),
                    "Habilidades sociales"))

SSIS_long <- SSIS_long %>% 
  left_join(subescalas, by = "clave") %>% 
  mutate(Subescala = factor(Subescala))

rm(subescalas)

# 4. b) SSIS por escalas y subescalas ####
gg_ssis <- function(escalas, angulo = 45){
  ggplot(SSIS_long %>% filter(escala == escalas),
         mapping = aes(x = Subescala, y = value, 
                       fill = pre.post)) +
    facet_wrap(~Grupo, strip.position = "bottom") +
    xlab("") + theme_classic() + 
    scale_fill_manual(values = colores_prepost, name ="") +
    scale_color_manual(values = colores_prepost, name ="") +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x =  element_text(size = 8, angle = angulo,
                                      hjust = 1)) +
    ylab("Puntuación")
}

gg_ssis("Habilidades sociales") + g + 
  geom_split_violin(size = 0.6) +
  point() + mediana + prom()

guardar("./películas/SSIS_hs.jpeg")

gg_ssis("Problemas de conducta") + g + 
  geom_split_violin(size = 0.6) +
  point() + mediana + prom() +
  scale_y_continuous( breaks = seq(0,60, by = 20))

guardar("./películas/SSIS_pc.jpeg")

# 5. a) Limpieza de datos pico máximo P3 ####

path_picos <- "./PCA/toolkit/"
pico_p3 <- read_tsv(paste(path_picos,
                          "amplitud p3/pico maximo/",
                          "pico maximo pz.txt",
                          sep = ""), col_names = F,
                    skip = 7)

pico_p3_vars <- read_tsv(paste(path_picos,
                               "amplitud p3/pico maximo/",
                               "pico maximo pz.txt",
                               sep = ""), col_names = F,
                         skip = 5) %>% slice(1)

colnames(pico_p3_vars) <- NULL

pico_p3_vars <- c(unlist(pico_p3_vars),"participante")

colnames(pico_p3) <- pico_p3_vars

condición_names <- c("IA" = "Alegría",
               "IT" = "Tristeza",
               "IE" = "Enojo",
               "ID" = "Identidad",
               "IS" = "Sexo")

condición_levels <- c("Identidad",
                      "Sexo",
                      "Alegría",
                      "Tristeza",
                      "Enojo")

pico_p3_long <- pico_p3 %>% 
  pivot_longer(-c(grupo, participante),
               names_to = "name", values_to = "Amplitud") %>% 
  separate(name,into = c("pre.post","condición"),sep = 1) %>%
  mutate(pre.post = str_replace_all(pre.post,
                                    c("1"= "Pre",
                                      "2" = "Post")) %>% 
           factor(levels = c("Pre","Post")),
         condición = str_replace_all(condición, condición_names),
         condición = factor(condición,
                            levels = condición_levels),
         Amplitud = replace(Amplitud, Amplitud == -999,NA),
         grupo = str_replace_all(grupo,c("1" = "Emoción",
                                         "2" = "Identidad")) %>% 
           factor(levels = c("Identidad","Emoción")))
   
pico_p3_medias <- pico_p3_long %>% 
  group_by(pre.post,grupo,condición) %>%
  summarise(Amplitud = mean(Amplitud,na.rm = T))
  


# 5. b) Gráficas de pico máximo P3 ####
gg_picop3 <- ggline(pico_mediasp3,x="pre_post",y="Amplitud",
       color="Condición",legend.title="")

gg_picop3 + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
                  strip.placement = "outside",
        text = element_text(family = "serif"))
ggsave("pico_max_p3.jpeg")

# 6. a) Limpieza de datos pico máximo T5 ####

pico_t5 <- read.delim("t5 pico maximo.txt",header = F,
                      sep = "\t",skip = 7,as.is = T)
pico_t5_vars <- read.delim("t5 pico maximo.txt",header = F,
                           sep = "\t",skip = 5,as.is = T,
                           nrows = 1)
colnames(pico_t5_vars) <- NULL
pico_t5_vars <- c(unlist(pico_t5_vars),"participante")
colnames(pico_t5) <- pico_t5_vars
pico_t5_long <- gather(pico_t5,Condición,Amplitud,
                       -c(grupo,participante))
pico_t5_long$pre_post <-as.factor(
  ifelse(substring(pico_t5_long$Condición,
                   1,1)=="1","Pre","Post"))
pico_t5_long$pre_post <- relevel(pico_t5_long$pre_post,"Pre")
pico_t5_long$Condición <- substring(
  pico_t5_long$Condición,2)
pico_t5_long$Condición <- pico_t5_long$Condición %>%
  str_replace_all(c("IA" = "Alegría",
                    "IT" = "Tristeza",
                    "IE" = "Enojo",
                    "ID" = "Identidad",
                    "IS" = "Sexo")) %>% 
  factor(levels = c("Alegría",
                    "Tristeza",
                    "Enojo",
                    "Identidad",
                    "Sexo"))

# Quitar valores omitidos
pico_t5_long$Amplitud <- ifelse(pico_t5_long$Amplitud==-999,
                                NA,pico_t5_long$Amplitud)
pico_mediast5 <- pico_t5_long %>% 
  group_by(pre_post,grupo,Condición) %>%
  summarise(Amplitud = mean(Amplitud,na.rm = T))

pico_mediast5$grupo <- ifelse(pico_mediast5$grupo==1,"Emoción",
                              "Identidad")
pico_mediast5$grupo <- as.factor(pico_mediast5$grupo)

# 6. b) Gráficas de pico máximo T5 ####
gg_picot5 <- ggline(pico_mediast5,x="pre_post",y="Amplitud",
                    color="Condición",legend.title="")

gg_picot5 + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_t5.jpeg")


# 7. a) Limpieza de datos pico máximo T6 ####

pico_t6 <- read.delim("t6 pico maximo.txt",header = F,
                      sep = "\t",skip = 7,as.is = T)
pico_t6_vars <- read.delim("t6 pico maximo.txt",header = F,
                           sep = "\t",skip = 5,as.is = T,
                           nrows = 1)
colnames(pico_t6_vars) <- NULL
pico_t6_vars <- c(unlist(pico_t6_vars),"participante")
colnames(pico_t6) <- pico_t6_vars
pico_t6_long <- gather(pico_t6,Condición,Amplitud,
                       -c(grupo,participante))
pico_t6_long$pre_post <-as.factor(
  ifelse(substring(pico_t6_long$Condición,
                   1,1)=="1","Pre","Post"))
pico_t6_long$pre_post <- relevel(pico_t6_long$pre_post,"Pre")
pico_t6_long$Condición <- substring(
  pico_t6_long$Condición,2)
pico_t6_long$Condición <- pico_t6_long$Condición %>%
  str_replace_all(c("IA" = "Alegría",
                    "IT" = "Tristeza",
                    "IE" = "Enojo",
                    "ID" = "Identidad",
                    "IS" = "Sexo")) %>% 
  factor(levels = c("Alegría",
                    "Tristeza",
                    "Enojo",
                    "Identidad",
                    "Sexo"))

# Quitar valores omitidos
pico_t6_long$Amplitud <- ifelse(pico_t6_long$Amplitud==-999,
                                NA,pico_t6_long$Amplitud)
pico_mediast6 <- pico_t6_long %>% 
  group_by(pre_post,grupo,Condición) %>%
  summarise(Amplitud = mean(Amplitud,na.rm = T))

pico_mediast6$grupo <- ifelse(pico_mediast6$grupo==1,"Emoción",
                              "Identidad")
pico_mediast6$grupo <- as.factor(pico_mediast6$grupo)

# 7. b) Gráficas de pico máximo T6 ####
gg_picot6 <- ggline(pico_mediast6,x="pre_post",y="Amplitud",
                    color="Condición",legend.title="")

gg_picot6 + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_t6.jpeg")



# 8. a) Limpieza de datos pico máximo n2 ####

pico_n2 <- read.delim("N2 pre post.txt",header = F,
                      sep = "\t",skip = 7,as.is = T)
pico_n2_vars <- read.delim("N2 pre post.txt",header = F,
                           sep = "\t",skip = 5,as.is = T,
                           nrows = 1)
colnames(pico_n2_vars) <- NULL
pico_n2_vars <- c(unlist(pico_n2_vars[1:31]),"participante")

pico_n2_elec <- read.delim("N2 pre post.txt",header = F,
                           sep = "\t",skip = 6,as.is = T,
                           nrows = 1)
colnames(pico_n2_elec) <- NULL
pico_n2_elec <- unlist(pico_n2_elec)

pico_n2_vars_elec <- paste(pico_n2_vars,pico_n2_elec,sep = "_")

colnames(pico_n2) <- pico_n2_vars_elec
pico_n2_long <- gather(pico_n2,Condición,Amplitud,
                       -c(grupo_spec,participante_NA))
pico_n2_long$pre_post <-as.factor(
  ifelse(substring(pico_n2_long$Condición,
                   1,1)=="1","Pre","Post"))
pico_n2_long$pre_post <- relevel(pico_n2_long$pre_post,"Pre")
pico_n2_long$electrodo <- substring(pico_n2_long$Condición,5)
pico_n2_long$Condición <- substring(
  pico_n2_long$Condición,2,3)
pico_n2_long$Condición <- pico_n2_long$Condición %>%
  str_replace_all(c("IA" = "Alegría",
                    "IT" = "Tristeza",
                    "IE" = "Enojo",
                    "ID" = "Identidad",
                    "IS" = "Sexo")) %>% 
  factor(levels = c("Alegría",
                    "Tristeza",
                    "Enojo",
                    "Identidad",
                    "Sexo"))

# Quitar valores omitidos
pico_n2_long$Amplitud <- ifelse(pico_n2_long$Amplitud==-999,
                                NA,pico_n2_long$Amplitud)
pico_mediasn2 <- pico_n2_long %>% 
  group_by(pre_post,grupo_spec,Condición,electrodo) %>%
  summarise(Amplitud = mean(Amplitud,na.rm = T))

pico_mediasn2$grupo <- ifelse(pico_mediasn2$grupo_spec==1,
                              "Emoción","Identidad")
pico_mediasn2$grupo <- as.factor(pico_mediasn2$grupo)


# 8. b) Gráficas de pico máximo n2 ####

# gg_picon2 <- facet_wrap(~grupo,strip.position = "bottom")+
#   theme_classic() + xlab("")+ 
#   ylab(TeX('Amplitud  ($\\mu$V)'))+
#   theme(strip.background = element_blank(),
#         strip.placement = "outside",
#         text = element_text(family = "serif"))+
#   coord_cartesian(ylim = c(-13,0))
gg_picon2_Cz <- ggline(filter(pico_mediasn2,electrodo=="Cz"),
                    x="pre_post",y="Amplitud",
                    color="Condición",legend.title="")
gg_picon2_Cz + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_n2_Cz.jpeg")

gg_picon2_Fz <- ggline(filter(pico_mediasn2,electrodo=="Fz"),
                       x="pre_post",y="Amplitud",
                       color="Condición",legend.title="")
gg_picon2_Fz + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_n2_Fz.jpeg")

gg_picon2_Pz <- ggline(filter(pico_mediasn2,electrodo=="Pz"),
                       x="pre_post",y="Amplitud",
                       color="Condición",legend.title="")
gg_picon2_Pz + facet_wrap(~grupo,strip.position = "bottom")+
  theme_classic() + xlab("")+ 
  ylab(TeX('Amplitud  ($\\mu$V)'))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"))+
  coord_cartesian(ylim = c(-13,0))
ggsave("pico_max_n2_Pz.jpeg")


# # # # # # # # # # # # # # # # # # ----
# P R U E B A
# # # # # # # # # # # # # # # # # #
source("vioplot2.R")
gg + g+ geom_split_violin(draw_quantiles = c(0.5)) +
  aes(y = T_reaccion_ms) +
  ylab("ms")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  stat_summary(fun.y = mean,
               geom = "point",shape=4,size=8)
ggsave("prueba_oddball_TR.jpeg",width = 800,height = 400,
       units = "mm")

# # # # # # # # # # # # # # # # # # 

