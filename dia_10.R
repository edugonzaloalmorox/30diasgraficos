library(sf)
library(tidyverse)
library(scico)
library(extrafont)

# Dia 10: 21/05/2020
# Paletas
# @EdudinGonzalo
#---------------------------------------------



barrios = read_sf("data/geo/SHP_ETRS89/BARRIOS.shp")
frawm <- read_csv("data/locales-fravm-completo.csv")

juego = frawm %>% count(desc_distr, sort = TRUE)

barrios = barrios %>% mutate(distrito = str_to_upper(NOMDIS), 
                             barrio = str_to_upper(NOMBRE))

barrios_ext = barrios %>% left_join(., juego, by = c("distrito" = "desc_distr"))


juego_plot = ggplot(barrios_ext) +
  geom_sf(aes(fill = n), color = "grey35", 
           size = 0.045, 
          alpha = 0.55) +
  scale_fill_scico(palette = 'lajolla', na.value = "ivory2") +
  theme_void() +
  labs(title = "Mapa del juego en Madrid",
       subtitle = "Casas de apuestas por distrito",
       fill = "",
       caption =  "#30diasdegraficos \n Fuente: Federaci??n Regional de Asociaciones Vecinales de Madrid | @EdudinGonzalo") +
  theme(text = element_text(family = "Andale Mono"),
        legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, size = 21.5, face = "bold"), 
        plot.subtitle  = element_text(hjust = 0.5, size = 10, face = "bold")) 

ggsave("figs/juego_plot.png", juego_plot, width = 9)


