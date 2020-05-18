# Day 7: 18/05/2020
# Grafico ridglelines
# @EdudinGonzalo
#---------------------------------------------


library(tidyverse)
library(ggridges)

provincias = read_csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-provincias-spain_consolidated.csv")


ridges = provincias %>%
  ggplot(aes(x = cases_per_cienmil, y = fct_rev(province), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.035) +
  scale_fill_viridis_c(name = "Casos", option = "A", direction = -1)  +
  labs(title = "Casos COVID-19 Espa??a",
       subtitles = "Por 100,000 habitantes (provincias)", 
       x = "", 
       y = "", 
       caption = "#30diasdegraficos \n Fuente: lab.montera34.com/covid19 |  @EdudinGonzalo") +
  theme(
    text = element_text(family = "Madrid Grunge"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5,
                        margin=margin(0,0,10.25,0),
                              size = 32, 
                              color="#968972"),
    plot.subtitle = element_text(hjust = 0.5,
                                 margin=margin(0,0,10.25,0),
                                 size = 12, 
                                 color = "grey50"),
    plot.caption = element_text(family = "Avenir", 
                                size = 8.5, 
                                colour = "grey40", 
                                face = "bold"), 
    panel.background  = element_blank(),
    strip.text.x = element_text(
      size = 8.5, color = "grey45", face = "bold"
    )) 

ggsave("figs/ridges.png", ridges, width = 9, height = 12)
