# Day 5: 15/05/2020
# Grafico de donut
# @EdudinGonzalo
#---------------------------------------------


library(enseResp)
library(tidyverse)
library(ggpubr)


kids = enseResp::children_19

info = enseResp::children_19_info

# Preparo datos --------------------------------

obesity = kids %>%
  count(CCAA, IMCm) %>%
  mutate_at(vars(IMCm), as.factor) %>%
  mutate(IMCm = case_when(IMCm == "1"~  "Peso insuficiente",
          IMCm == '2' ~ "Normopeso", 
          IMCm == '3' ~ "Sobrepeso", 
          IMCm == '4' ~ "Obesidad", 
          IMCm == '9' ~ "No consta", 
          is.na(IMCm) ~ "No disponible"))

obesity$IMCm = factor(obesity$IMCm , levels = c("No disponible", 
                                                "No consta", 
                                                "Peso insuficiente",
                                                "Normopeso",
                                                "Sobrepeso",
                                                "Obesidad"))



regions = read_csv("data/regions.csv")

obesity = obesity %>%
  left_join(., regions, by = c("CCAA" = "id_ense")) %>%
  select(ccaa, IMCm, n)

# Grafico -----------------------------------------

obesidad = obesity %>%
  group_by(ccaa) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = 2, y = prop, fill = IMCm)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.85) +
  facet_wrap(facets=. ~ ccaa) +
  xlim(0.5, 2.5) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "Dark1") +
  labs(title = "Obesidad Infantil en Espa??a", 
       subtitle = "Proporci??n de Indice de Masa Corporal", 
       caption = "#30diasdegraficos \n Fuente: Encuesta Nacional de Salud (2017-2019) | @EdudinGonzalo") +
  theme(
    text = element_text(family = "Open Sans Condensed Bold"),
    legend.position = "bottom",
    legend.title = element_blank(), 
    plot.title = element_text(hjust = 0.5,
                              margin=margin(0,0,10.25,0),
                              size = 32, 
                              color="#759672"),
    plot.subtitle = element_text(hjust = 0.5,
                                 margin=margin(0,0,10.25,0),
                                 size = 12, 
                                 color = "grey50"),
    panel.background  = element_blank(),
    strip.text.x = element_text(
      size = 8.5, color = "grey45", face = "bold"
    ))

ggsave("figs/obesidad.png", obesidad, width = 9, height = 12)
 
 
  
