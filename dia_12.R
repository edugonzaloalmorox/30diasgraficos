# Dia 12: 23/05/2020
# Lollipop
# @EdudinGonzalo
#---------------------------------------------


library(tidyverse)
library(readxl)


accidentes_2019 <- read_excel("data/AccidentesBicicletas_2019.xlsx")
accidentes_2020 <- read_excel("data/AccidentesBicicletas_2020 (1).xlsx")

bicis = accidentes_2019 %>%
  janitor::clean_names() %>%
  bind_rows(., accidentes_2020 %>%
              janitor::clean_names()) %>%
  count(distrito, tipo_accidente) %>%
  ggplot(aes(n, tipo_accidente)) +
  geom_segment(aes(x = 0, y = tipo_accidente, xend = n, yend = tipo_accidente), color = "grey50") +
  geom_point() +
  labs(x = "", 
       y = "", 
       title = "N??mero de accidentes con bicicleta en Madrid (2019 - 2020)",
       subtitle = "Por tipo de accidente", 
       caption =  "#30diasdegraficos \n Fuente: Ayuntamiento de Madrid | @EdudinGonzalo"
       ) +
  theme_minimal() +
  theme(text = element_text(family = "Schoolbell"), 
        panel.grid = element_line(size = 0.3)) +
  facet_wrap(~ distrito)


ggsave("figs/bicis_acc.png", bicis, width = 9)
