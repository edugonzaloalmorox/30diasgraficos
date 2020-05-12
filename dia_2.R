# Day 2: 12/05/2020
# Grafico de lineas 
# @EdudinGonzalo
#---------------------------------------------

library(tidyverse)
library(gghighlight)


read_covid = function(start_date, end_date, region = NULL){
  
  require(tidyverse)
  require(janitor)
  require(glue)
  require(rlang)
  
  if(missing(region)) {
    df = read_csv("https://covid19.isciii.es/resources/serie_historica_acumulados.csv") %>%
      janitor::clean_names() %>%
      mutate(fecha = lubridate::dmy(fecha)) %>%
      filter(!is.na(fecha), fecha >= {{ start_date }} & fecha <= {{ end_date}})
  } else {
    df = read_csv("https://covid19.isciii.es/resources/serie_historica_acumulados.csv") %>%
      janitor::clean_names() %>%
      mutate(fecha = lubridate::dmy(fecha)) %>%
      filter(!is.na(fecha), fecha >= {{ start_date }} & fecha <= {{ end_date}}, ccaa %in% {{ region }})
    
  }
  return(df)
  
}    

theme_covid <- function () {
  
  require(extrafontdb)
  
  theme_minimal(base_size=10, base_family="Trebuchet MS") %+replace%
    theme(
      panel.background  = element_blank(),
      plot.title = element_text(hjust = 0.5, margin=margin(0,0,6.25,0), size = 12, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, margin=margin(0,0,5,0)),
      strip.background = element_rect(
        color="white", fill="#818e91", size=1, linetype="solid"),
      strip.text.x = element_text(
        size = 8.85, color = "white", face = "bold"
      ),
      # plot.background = element_rect(fill="gray96", colour=NA),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.1),
      panel.border = element_blank()
      
      
    )
}

covid_spain = read_covid('2020-03-01', '2020-05-10')

covid_plot = covid_spain %>%
  arrange(ccaa, fecha) %>%
  group_by(ccaa) %>%
  mutate(recuperados = replace_na(recuperados, 0), 
       daily_cases = recuperados - lag(recuperados))  %>%
  ungroup() %>%
  ggplot(aes(fecha, daily_cases, colour = ccaa)) +
  geom_line() +
  theme_covid() +
  gghighlight(use_direct_label = FALSE) +
  scale_x_date(breaks = as.Date(c("2020-03-01", 
                                  "2020-03-15",
                                  "2020-04-01",
                                  "2020-04-15", 
                                  "2020-05-01"))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ ccaa) +
  labs(x = "",
       y = "Recuperados diarios",
       title = "Recuperados diarios COVID-19 Espa??a",
       subtitle = "Regiones (c??digos)", 
       caption = "Nota: Galicia (GA) pasa de reportar 1841 recuperados el 27 Abril a 5393 el 28 de Abril 
       \n Fuente: Instituto Carlos III | @EdudinGonzalo")

ggsave("figs/covid_spain.png", covid_plot, width = 10)


    
