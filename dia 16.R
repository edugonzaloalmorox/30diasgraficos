# Dia 16: 27/05/2020
# Waffle
# @EdudinGonzalo
#---------------------------------------------



library(enseResp)
library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)
library(scico)

kids_info = enseResp::children_19_info
children = enseResp::children_19

waffle_plot = children %>%
  select(CCAA, L72_13) %>%
  filter(CCAA == 13) %>%
  count(L72_13) %>% 
  mutate_at(vars(L72_13), as.factor) %>%
  ggplot(aes(fill=L72_13, values=n)) +
  geom_waffle(n_rows = 15, size = 0.65, colour = "white", flip = TRUE, alpha = 0.85) +
  scale_fill_manual(
    name = NULL,
    values = c( "#2B194C", "#274376", "#3367AC", "#788BDA", "#B7B0F0", "#D9D6F6", "#FFFFFF"),
    labels = c("Una o m??s veces al d??a",
               "De 4 a 6 veces a la semana",
               "Tres veces a la semana", 
               "Una o dos veces a la semana",
               "Menos de una vez a la semana", 
               "Nunca", 
               "No contesta")
  ) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(title = "Frecuencia de consumo de comida r??pida en ni??os",
       subtitle = "Comunidad de Madrid",
       caption =  "#30diasdegraficos \n Fuente: Encuesta Nacional de Salud 2017-19 (Ministerio de Sanidad) \n @EdudinGonzalo") +
  theme(text = element_text(family = "PT Sans Narrow"), 
        plot.title = element_text(hjust = 0.5,
                                  margin=margin(0,0,12.25,0),
                                  size = 16, 
                                  face = "bold",
                                  color="#274376"),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin=margin(0,0,10.25,0),
                                     size = 10, 
                                     color="#274376"),
        plot.caption = element_text(color="#274376"))


ggsave("figs/waffle_children.png", waffle_plot, width = 9)

