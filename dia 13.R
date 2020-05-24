# Dia 13: 24/05/2020
# Datos temporales
# @EdudinGonzalo
#---------------------------------------------


library(tidyverse)
library(readxl)
library(lubridate)
library(scico)
library(rayshader)



accidentes_2019 <- read_excel("data/AccidentesBicicletas_2019.xlsx")
accidentes_2020 <- read_excel("data/AccidentesBicicletas_2020 (1).xlsx")


bicis = accidentes_2019 %>%
  janitor::clean_names() %>%
  bind_rows(., accidentes_2020 %>%
              janitor::clean_names()) %>%
  count(dia_semana = wday(fecha, label = TRUE), hora = hour(hora)) %>%
  mutate(hora = as.character(str_pad(hora, 2, "left", "0")))


df= data_frame(dia_semana = rep(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 24), 
               hora = as.character(rep(c(00:23), 7))) %>%
  mutate(hora = str_pad(hora, 2, "left", "0")) %>%
  left_join(bicis, by = c("dia_semana", "hora") ) %>%
  arrange(dia_semana, hora) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  rename(accidentes = n)


df$dia_semana  = factor(df$dia_semana, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

accs = df %>%
  ggplot(aes(hora, fct_reorder(dia_semana, desc(dia_semana)))) +
  geom_tile(aes(fill = accidentes), colour = "white", alpha = 0.85) + 
  scale_fill_scico(palette = "vik", na.value = "black") + 
  scale_y_discrete(breaks = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                   labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")) +
  coord_fixed(ratio = 1.75) +
  labs(x = "", 
       y = "", 
       title = "Accidentes de bicicleta en Madrid durante la semana",
       fill = "Accidentes",
       caption =  "#30diasdegraficos \n Fuente: Ayuntamiento de Madrid | @EdudinGonzalo"
       ) +
  theme(text = element_text(family = "Avenir"),
    legend.position = "bottom")
  



plot_gg(accs,multicore=TRUE,width=5,height=5,scale=250)

plot_gg(accs, width = 6, height = 6, scale = 300, multicore = TRUE)
render_camera(phi = 55, theta = 45, zoom = 0.5, fov = 70)
render_snapshot()
render_movie("bicis.mp4", frames = 90)



filename_movie = tempfile()

