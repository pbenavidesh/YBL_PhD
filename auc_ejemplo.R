# Area under a curve

gg_df <- df_norm %>%
  # filter(Sujeto %in% c("ADA8M")) %>% 
  group_by(t, Grupo,Condición,
           Evaluación, Electrodo) %>% 
  summarise(Amplitud = mean(value)) %>%
  filter(Condición == "Alegría", 
         Electrodo == "Pz",
         Grupo == "Grupo emoción") %>% 
  mutate(t_eval = str_c(t, Evaluación, 
                        sep = "_"))

gg_area_pre <- gg_df %>% 
  filter(t %in% seq(478,678, by = 2) &
           Evaluación == "PRE")

gg_area_post <- gg_df %>% 
  filter(t %in% seq(490,690, by = 2) &
           Evaluación == "POST")

gg <- gg_df %>%
  
  ggplot(aes(x = t, y = Amplitud, 
             color = Evaluación)) +
  
  scale_x_continuous(breaks = seq(-100,1000,
                                  by = 100)) +
  theme_classic() + 
  theme(text = element_text(family = "serif", size = 14)) +
  labs(x = "ms", y = "µV") + #plotly::TeX('$\\mu V$')
  scale_color_manual(values=c("PRE"= "turquoise3",
                              "POST"="orchid2"),
                     name="")

ggplotly( gg + geom_line(size = 1) + 
            geom_area(data = gg_area_pre, 
                      aes(x = t, y = Amplitud),
                      fill = "turquoise3", alpha = 0.5)+
            geom_area(data = gg_area_post, 
                      aes(x = t, y = Amplitud),
                      fill = "orchid1", alpha = 0.5)
)

DescTools::AUC(
  x = gg_area_pre$t, y =gg_area_pre$Amplitud,
               absolutearea = TRUE)
DescTools::AUC(
  x = gg_area_post$t, y =gg_area_post$Amplitud,
  absolutearea = TRUE)
