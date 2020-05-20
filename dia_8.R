# D??a 8: 19/05/2020
# Grafico de arcos
# @EdudinGonzalo
#---------------------------------------------



library(tidyverse)
library(readxl)
library(glue)
library(janitor)

files <- list.files(path = "~/Dropbox/side_projects/data_science/care_homes_spain/data/raw/year_2019", pattern = "*.xlsx", full.names = T)


x = files %>%
  map(read_excel) %>%
  reduce(bind_rows)


colnames(x) = x[3, ]


x = x %>% clean_names()

resis  = x %>% 
  filter(!is.na(latitud), latitud != "Latitud") %>%
  select(denominacion, municipio, c_postal, titularidad, latitud, longitud) %>%
  mutate(longitud = gsub(",", "", longitud), 
         latitud = gsub(",", "", latitud)) %>%
  mutate_at(vars(longitud, latitud), as.numeric)


resi_dist = resis %>%
  mutate(type = ifelse(str_detect(titularidad, "Privada"), "Privada",  "Publica")) %>%
  ggplot( aes(x = longitud, y = latitud) ) + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  geom_point(aes(colour = type), size = 0.1) +
  stat_density2d(aes(fill = ..level..), geom = "polygon",  alpha = .4) +
  scale_fill_viridis_c(option = "magma") + 
  labs(title = "Distribuci??n de residencias en Espa??a",
       subtitle = "P??blicas (verde) y privadas (roja)", 
       caption = "#30diasdegraficos \n Fuente: CSIC | @EdudinGonzalo")  +
  theme_void() +
  theme(legend.position = 'none',
        text = element_text(family = "Overpass Regular"), 
        plot.title = element_text(hjust = 0.5, margin=margin(0,0,6.25,0), size = 16), 
        plot.subtitle = element_text(hjust = 0.5, margin=margin(0,0,5,0), size = 9))

ggsave("figs/resis.png", resi_dist, width = 9)
