# Day 3: 13/05/2020
# Grafico de burbujas
# @EdudinGonzalo
#---------------------------------------------

library(tidyverse)
library(ggrepel)

redes = read_csv("data/redes_vecinales.csv")

# Limpieza  -------------------------
colnames(redes) = c("distrito", "red", "voluntarios", "casos", "alimentos_familias", "alimentos_personas")
redes = redes[3:57, ] 

# Grafico --------------------------
vecinos = redes %>%
  mutate_at(vars(-distrito, -red), as.numeric) %>%
  ggplot(aes(casos, voluntarios)) +
  geom_point(aes(size = alimentos_personas, color = distrito),
             alpha = 0.5) +
  labs(title = "Redes de Apoyo Vecinal en Madrid",
       subtitle = "Por distrito a 30 Abril 2020",
       size = "Personas Alimentadas", 
       color = "", 
       caption = "Fuente: Federaci??n Regional de Asociaciones Vecinales de Madrid | @EdudinGonzalo "
       ) +
  geom_text_repel(data = . %>%
                    filter(casos >= 800 | voluntarios >= 280 ), 
                  aes(label = distrito),
                  fontface = 'bold', color = 'grey50',
                  size = 2.75,
                  point.padding = unit(0.2, "lines")) +
  theme(text = element_text(family = "PT Sans Narrow Bold"),
        strip.background =element_rect(fill="#fcef03"),
        panel.background = element_rect(fill = "#faf9f5"),
        plot.title = element_text(size=18))

ggsave("figs/vecinos.png", vecinos, width = 9)