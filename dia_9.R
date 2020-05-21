# D??a 9: 20/05/2020
# Grafico de areas apiladas
# @EdudinGonzalo
#---------------------------------------------


library(tidyverse)
library(readxl)
library(anytime)
library(hrbrthemes)
library(viridis)
library(plotly)

year_2020 = read.csv("~/Downloads/volpre2020.csv", sep = ";")
year_2019 = read.csv("~/Downloads/volpre2019.csv",  sep = ";")

mercado = bind_rows(year_2019, year_2020)

origen = mercado %>%
  ungroup() %>%
  count(descripcion_origen, sort = TRUE) 


mercado_plot  = mercado %>%
  ungroup() %>%
  count(inicio, descripcion_origen) %>%
  mutate(inicio = anydate(inicio), 
         descripcion_origen = str_trim(descripcion_origen, "both")) %>%
  filter(descripcion_origen %in% c("PONTEVEDRA",
                                   "MURCIA",
                                   "GUIPUZCOA", 
                                   "VALENCIA",
                                   "MADRID",
                                   "HUELVA",
                                   "FRANCIA", 
                                   "TOLEDO",
                                   "CASTELLON",
                                   "CADIZ", "ALMERIA", "PORTUGAL", "ALICANTE", 
                                   "SEVILLA", "MALAGA")) %>%
         ggplot(aes(x=inicio, y=n, fill=descripcion_origen)) + 
  geom_area(alpha=0.7 , size=.1, colour="grey50") +
  scale_fill_viridis(discrete = T, option = "plasma") +
  labs( x = "", 
        y = "N??mero de productos...",
        title = "Procedencia productos Mercamadrid", 
        subtitle = "??Cu??les son los or??genes m??s frecuentes?", 
        caption =  "#30diasdegraficos \n Fuente: Mercamadrid | @EdudinGonzalo") +
  theme_ipsum() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("figs/mercado_plot.png", mercado_plot, width = 9)





