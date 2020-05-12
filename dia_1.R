# Day 1: 12/05/2020
# Grafico de barras 
# @EdudinGonzalo
#---------------------------------------------



library(geojsonio)
library(tidyverse)

spdf <- geojson_read("https://gisco-services.ec.europa.eu/pub/healthcare/geojson/all.geojson",  what = "sp") 


df = as.data.frame(spdf)

spain = df %>% filter(country == "Spain")


beds_spain = spain %>%
  group_by(city, hospital_name, public_private) %>%
  summarise(beds_ava = sum(cap_beds)) %>%
  arrange(desc(beds_ava))

hospis = beds_spain %>%
  mutate(public_private = ifelse(public_private == "private", "Privado", "P??blico")) %>%
  group_by(public_private) %>%
  top_n(10, beds_ava) %>%
  ggplot(aes(fct_reorder(hospital_name, beds_ava), beds_ava)) +
  geom_col(alpha = 0.75) +
  theme_grafico() +
  labs(x = "",
       title = "Hospitales m??s grandes de Espa??a", 
       subtitle = "Por n??mero de camas y propiedad",
       y = "",
       caption = "Fuente: Eurostat | @EdudinGonzalo") +
  facet_wrap(~ public_private, ncol = 1, scales = "free_y") +
  coord_flip()  +
  theme(plot.title = element_text(size=16, face = "bold", family = "Avenir"))

ggsave("figs/hospis.png", hospis, width = 10)
