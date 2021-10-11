# Artículo Pre-Post sin EEG

# sourcing scripts --------------------------------------------------------

source("02_a_prepost_sin_eeg_limpieza.R",encoding = "UTF-8")



# FUNS --------------------------------------------------------------------

art_eeg_plot <- function(variable, 
                          grafico = c("violin", "boxplot"),
                          tarea,
                          .interactive = FALSE){
  
  art_colores_prepost <- c("Pre-"  = "dodgerblue3",
                           "Post-" = "peachpuff")
  
  plot_type <- c(violin = geom_split_violin(size = 0.6), 
                 boxplot = geom_boxplot(
                   outlier.color = "black",
                   outlier.shape = 1,
                   outlier.size  = 0.7
                 ))
  
  p <- art_eeg_tbl %>% 
    filter(Variable %in% variable,
           Tarea == tarea) %>%
    ggplot(aes(x = Emoción, y = valor, fill = pre.post)) +
    facet_grid(Variable ~ Grupo, switch = "both", scales = "free_y") +
    xlab("") + theme_classic() + ylab("") +
    plot_type[grafico] + g +
    scale_fill_manual(values = art_colores_prepost, name = "") +
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    mediana +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
    theme(text = element_text(family = "sans",
                              size = 12, color = "black"),
          legend.justification = "top",
          plot.title = element_text(hjust = 0.5),
          axis.ticks.x = element_blank()) +
    ggpubr::stat_compare_means(method = "wilcox.test",
                               paired = TRUE,
                               label = "p.signif",
                               hide.ns = TRUE)
  
  if(length(variable) == 1){
    p <- p + theme(strip.text.y = element_blank())
    p
  } else{
    p
  }
  
  if(.interactive){
    plotly::ggplotly(p)
  } else {
    p
  }
  
  # if(tarea == "Emparejamiento"){
  #   p + ggtitle("Matching Tasks")
  # } else{
  #   p + ggtitle("Memory Tasks")
  # }
}

# DATA --------------------------------------------------------------------

# Tabla de errores - wide
eeg_errores_medias_wide <- eeg_errores_medias %>% 
  select(-n) %>%
  drop_na() %>%
  pivot_wider(
    names_from = Tipo_error, 
    values_from = pct,
    values_fill = 0
  ) %>% 
  mutate(
    Tarea = str_replace_all(
      Tipo_tarea,
      c("emp" = "Emparejamiento", 
        "mem" = "Memoria")
    )
  ) %>% 
  select(-Tipo_tarea) %>% 
  mutate(ac_com = Acierto - Comisión)

# Tabla de las medias - wide
eeg_medias_wide <- eeg_medias %>% 
  mutate(
    Condición = str_replace(Condición, "emocion", "emoción")
  ) %>% 
  pivot_wider(
    names_from = Variable,
    values_from = value
  )

# Tabla final
art_eeg_tbl <- eeg_medias_wide %>% 
  left_join(eeg_errores_medias_wide) %>% 
  mutate(
    Grupo = fct_recode(
      Grupo,
      `Identity Group` = "Identidad",
      `Emotion Group`  = "Emoción"
    ),
    Emoción = fct_recode(
      Emoción,
      Identity  = "Identidad",
      Happiness = "Alegría",
      Sadness   = "Tristeza",
      Anger     = "Enojo"
    ),
    pre.post = fct_recode(
      pre.post,
      `Pre-`  = "Pre",
      `Post-` = "Post"
    )
  ) %>% 
  select(-Acierto) %>% 
  pivot_longer(
    cols      = RC:ac_com,
    names_to  = "Variable",
    values_to = "valor"
  ) %>% 
  mutate(
    Variable = str_replace_all(
      Variable, c("RC" = "Correct Responses (%)", "Comisión" = "Errors (%)",
                  "TR" = "Response Times (s)", "ac_com" = "RCE")
    ),
    Variable = factor(Variable, levels = c("Correct Responses (%)", 
                                           "Errors (%)", "Omisión", 
                                           "Response Times (s)",
                                           "RCE"))
  )




# STATS -------------------------------------------------------------------

# * Tabla mediana y cuartiles ---------------------------------------------
art_eeg_tbl %>% 
  filter(Variable %in% c("Correct Responses (%)",
                         "Errors (%)",
                         "Response Times (s)")) %>%
  group_by(Tarea, pre.post, Variable, Grupo, Emoción) %>% 
  summarise(
    Min      = min(valor, na.rm = TRUE),
    Cuartil1 = quantile(valor, probs = 0.25, na.rm = TRUE),
    Mediana  = median(valor, na.rm = TRUE),
    Cuartil3 = quantile(valor, probs = 0.75, na.rm = TRUE),
    Max      = max(valor, na.rm = TRUE)
  ) %>% 
  view()
# write_csv("medianas.csv")


# * Wilcoxon --------------------------------------------------------------

art_eeg_tbl %>% 
  ggpubr::compare_means(
    formula =  valor ~ pre.post, 
    paired  = TRUE,
    group.by = c("Variable", "Grupo", "Tarea", 
                 "Condición", "Emoción")
  ) %>% 
  # filter(p.signif != "ns") %>% 
  view()

art_eeg_tbl %>% 
  group_by(Variable, Grupo, Tarea, Condición, Emoción) %>% 
  rstatix::wilcox_test(valor ~ pre.post, paired = TRUE) %>% 
  # view()
  write_csv("wilcoxon.csv")

# U de Mann Whitney

art_eeg_tbl %>% 
  ggpubr::compare_means(
    formula =  valor ~ Grupo, 
    paired  = FALSE,
    group.by = c("Variable", "pre.post", "Tarea", 
                 "Condición", "Emoción")
  ) %>% 
  filter(p.signif != "ns") %>%
  view()

art_eeg_tbl %>% 
  group_by(Variable, pre.post, Tarea, Condición, Emoción) %>% 
  rstatix::wilcox_test(valor ~ Grupo, paired = FALSE) %>% 
  # view()
  write_csv("u_mann_whitney.csv")


# PLOTS -------------------------------------------------------------------

# * CR, Errors, RT --------------------------------------------------------

art_eeg_plot(
  variable = c("Correct Responses (%)",
               "Errors (%)",
               "Response Times (s)"), 
  tarea    = "Emparejamiento", 
  grafico  = "boxplot",
  .interactive = FALSE
)
# guardar("./Artículo/Emp_RC_Comision_TR_jul.jpeg",
#         height = 180, width = 220,
#         units = "mm")

art_eeg_plot(
  variable = c("Correct Responses (%)",
               "Errors (%)",
               "Response Times (s)"), 
  tarea    = "Memoria", 
  grafico  = "boxplot"
)

# guardar("./Artículo/Mem_RC_Comision_TR_jul.jpeg",
#         height = 180, width = 220,
#         units = "mm")


# * Acomodo rectangular ---------------------------------------------------

p1 <- art_eeg_plot(
  variable = c("Correct Responses (%)",
               "Errors (%)"), 
  tarea    = "Emparejamiento", 
  grafico  = "boxplot"
) +  theme(legend.position = "none")

p2 <- art_eeg_plot(
  variable = "Response Times (s)", 
  tarea    = "Emparejamiento", 
  grafico  = "boxplot"
) #+ 
# theme(axis.title.x = element_blank(),
#       axis.text.x  = element_blank(),
#       strip.text.x = element_blank())

p1 | (p2/plot_spacer()) 

# guardar("./Artículo/matchingtasks_jul.jpeg",
#         height = 150, width = 330,
#         units = "mm")

p1 <- art_eeg_plot(
  variable = c("Correct Responses (%)",
               "Errors (%)"), 
  tarea    = "Memoria", 
  grafico  = "boxplot"
) + theme(legend.position = "none")

p2 <- art_eeg_plot(
  variable = "Response Times (s)", 
  tarea    = "Memoria", 
  grafico  = "boxplot"
)# + 
# theme(axis.title.x = element_blank(),
#       axis.text.x  = element_blank(),
#       strip.text.x = element_blank())

p1 | (p2/plot_spacer())

# guardar("./Artículo/memorytasks_jul.jpeg",
#         height = 150, width = 330,
#         units = "mm")

# * Aciertos - comisión ---------------------------------------------------

art_eeg_plot(
  variable = "RCE", 
  tarea    = "Emparejamiento", 
  grafico  = "boxplot",
  .interactive = FALSE
)

# guardar("./Artículo/Emp_RCE_AE.jpeg",
#         height = 120, width = 220,
#         units = "mm")

art_eeg_plot(
  variable = "RCE", 
  tarea    = "Memoria", 
  grafico  = "boxplot",
  .interactive = FALSE
)

# guardar("./Artículo/Mem_RCE_AE.jpeg",
#         height = 120, width = 220,
#         units = "mm")
