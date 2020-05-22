# Latevolts ####

library(easypackages)
libraries("tidyverse","scales")

latevolts <- read_csv("latevolts.csv",
                      locale = locale(encoding = "ISO-8859-1")) %>% 
  mutate(grupo = str_replace_all(grupo,c("1" = "Emoción",
                                   "2" = "Identidad")) %>% 
           factor(levels = c("Identidad", "Emoción")),
         evaluación = factor(evaluación, 
                             levels = c("PRE","POST")),
         condición = factor(condición, 
                            levels = c("Identidad","Sexo",
                                       "Alegría", "Tristeza",
                                       "Enojo")))

# formato básico de las gráficas
g <- theme(text = element_text(family = "serif",
                               size = 12))
# colores para evaluación pre y post
colores_prepost <- c("PRE"= "turquoise1",
                     "POST"="orchid1")

latevolts %>% 
  filter(electrodo == "T5") %>% 
  ggplot(aes(x = condición, y = µV, fill = evaluación)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge")+
  facet_wrap(~ grupo)

# prueba función ####
gg_latevolts <- function(elec = c("T5","T6"), 
                         y_plot = c("Amplitud","ms"),
                         scale_rev = TRUE){
  gg <- latevolts %>%
    filter(electrodo %in% elec) %>% 
    ggplot(aes(x = condición, 
               y = if_else(y_plot == "Amplitud", µV, ms),
               fill = evaluación)) + 
    geom_bar(stat = "summary", fun = "mean", position = "dodge",
             color = "black")+ xlab("") +
    scale_fill_manual(values= colores_prepost, name="") +
    theme_classic() + 
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    stat_summary(geom = "errorbar",
                 fun.data = mean_se, 
                 position = position_dodge(width = 1), width = 0.5)
  
  if(sum(elec == c("T5","T6"))>=1){
    gg <- gg  +
      facet_grid(electrodo ~ grupo, scales = "free", switch = "x")
      
  } else {gg <- gg + facet_wrap(~ grupo, scales = "free", 
                          strip.position = "bottom")}
  if(scale_rev == TRUE){
    gg + scale_y_reverse()
  } else{ gg}
}

gg_latevolts(elec = c("T5","T6"),y_plot = "Amplitud",
             scale_rev = TRUE)

# prueba ####  
gg_late <- latevolts %>%
  filter(electrodo %in% c("T5","T6")) %>% 
  ggplot(aes(x = condición, y = µV, fill = evaluación)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge",
           color = "black")+ xlab("") +
  facet_grid(electrodo ~ grupo, scales = "free", switch = "x") +
  scale_fill_manual(values= colores_prepost, name="") +
  scale_y_reverse() + theme_classic() + 
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  stat_summary(geom = "errorbar",
               fun.data = mean_se, 
               position = position_dodge(width = 1), width = 0.5) 
  ggplotly(gg_late)
  

latevolts_medias <- latevolts %>% 
  group_by(electrodo, grupo, evaluación, condición) %>% 
  summarise(media_ms = mean(ms),
            media_µV = mean(µV))



latevolts_medias %>% 
  filter(electrodo == "T5") %>% 
  ggplot(aes(x = condición, y = media_µV, fill = evaluación)) + 
  geom_bar(stat = "identity",position = "dodge")+
  facet_wrap(~ grupo)


latevolts %>%
  pivot_longer(cols = c("µV","ms"),names_to = "var_late") %>% 
  filter(var_late == "ms") %>% 
  distinct(var_late)
