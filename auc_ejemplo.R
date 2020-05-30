# Area under a curve


# pkgs --------------------------------------------------------------------

library(easypackages)
libraries("tidyverse")

# Tidy data regarding AUC points
source("eeg_plots/scripts/latevolts_auc_tidy.R",
       encoding = "UTF-8")

# df_norm
df_norm <- read_csv("eeg_plots/data/df_norm.csv")


gg_df <- df_norm %>%
  filter(Electrodo == "Pz") %>% 
  group_by(t, Grupo,Condición,
           Evaluación, Sujeto) %>% 
  summarise(Amplitud = mean(value)) %>%
  filter(Condición == "Alegría", 
         Grupo == "Grupo emoción") %>% 
  mutate(t_eval = str_c(t, Evaluación, 
                        sep = "_")) %>% 
  left_join(df, by = c("Sujeto" = "código",
                       "Grupo" = "grupo",
                       "Condición" = "Condición",
                       "Evaluación" = "Evaluación"
                       ))

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
