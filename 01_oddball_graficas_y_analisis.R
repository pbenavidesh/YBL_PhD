# Gráficas y análisis oddball


# sourcing scripts --------------------------------------------------------

source("01_oddball_limpieza.R", encoding = "UTF-8")


# ggplot base -------------------------------------------------------------

gg <- ggplot(data = df_medias) +
  aes(x = Actividad, fill = pre.post)+ 
  facet_wrap(~Grupo, strip.position = "bottom") +
  xlab("") + theme_classic()+ 
  scale_fill_manual(values= colores_prepost, name="")+
  scale_color_manual(values= colores_prepost,name="")+
  theme(strip.background = element_blank(),
        strip.placement = "outside")

# * Tiempos de reacción ---------------------------------------------------

gg + g+ geom_split_violin(size = 0.6) +  aes(y = T_reaccion_ms) +
  ylab("ms") +
  point(width = 0.5) + tema + mediana + prom(width = 0.5)

# guardar("./oddball/oddball_TR.jpeg")

# boxplot

gg + g + geom_boxplot() +  aes(y = T_reaccion_ms) +
  ylab("ms")+
  point(width = 0.75) + tema + prom(width = 0.75) +
  geom_line(aes(group = Act_part))

# guardar("./oddball/oddball_box_TR.jpeg")


# * Respuestas correctas --------------------------------------------------

gg + g + geom_split_violin(size = 0.6) + aes(y = RC) + 
  ylab("%") + point() + tema + mediana + prom()

# guardar("./oddball/oddball_RC.jpeg")

# boxplot
gg + g + geom_boxplot() + aes(y = RC) + 
  ylab("%") + point(width = 0.75) + tema + prom(width = 0.75)

# guardar("./oddball/oddball_box_RC.jpeg")


# * Errores ---------------------------------------------------------------

df_medias_errores %>% 
  select(-c(RC, n)) %>%
  pivot_wider(
    names_from  = Tipo_error,
    values_from = pct
  ) %>% 
  mutate_at(.vars = c("Acierto", "Comisión", "Omisión"),
            .funs = str_replace_na, replacement = 0)

df_medias_errores %>% 
  ggplot(aes(x = Actividad, y = RC, 
             fill = Tipo_error, group = pre.post)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~ Grupo)

# % de aciertos y errores por grupo, actividad, frec/infrec
df_medias_errores %>% 
  group_by(pre.post, Grupo, Actividad, 
           frecuente.infrecuente, Tipo_error) %>% 
  summarise(pct = mean(pct)) %>% 
  ggplot(aes(x = pre.post, y = pct,
             fill = Tipo_error)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(Grupo ~ Actividad + frecuente.infrecuente)

df_medias_errores %>% 
  filter(Tipo_error != "Acierto") %>%
  ggplot(aes(x = Actividad, y = pct, fill = pre.post)) +
  facet_grid(Tipo_error ~ Grupo, switch = "both") +
  xlab("") + theme_classic() + ylab("%") +
  geom_split_violin(size = 0.6) + g +
  scale_fill_manual(values= colores_prepost, name="") +
  theme(strip.background = element_blank(),
        strip.placement = "outside") +
  point(width = 0.5) + tema + mediana + prom(width = 0.5) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)

# guardar("./oddball/oddball_errores.jpeg")


