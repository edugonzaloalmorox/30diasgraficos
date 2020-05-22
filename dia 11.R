# Dia 11: 22/05/2020
# Heatmap
# @EdudinGonzalo
#---------------------------------------------


library(tidyverse)
library(viridis)


provincias = read_csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-provincias-spain_consolidated.csv")

df =  provincias %>% select(date, province, daily_cases, daily_deaths) 


df = df %>%
  filter(date >= "2020-03-01") %>%
  mutate(lab_death =  ordered(case_when(daily_deaths <= 0 ~ "0", 
                                daily_deaths > 0 & daily_deaths <= 2 ~ "1-2",
                                daily_deaths > 3 & daily_deaths <= 6 ~ "3-6",
                                daily_deaths > 6 & daily_deaths <= 15 ~ "7-15",
                                daily_deaths > 15 & daily_deaths <= 30 ~ "16-30",
                                daily_deaths > 30 & daily_deaths <= 60 ~ "31-60",
                                daily_deaths > 60 & daily_deaths <= 120 ~ "61-120",
                                daily_deaths > 120 & daily_deaths <= 240 ~ "120-240",
                                daily_deaths > 240 ~ ">240")))

df$lab_death = factor(df$lab_death, levels = c("0", "1-2", "3-6", "7-15", "16-30", "31-60", "61-120", "120-240", ">240" ))

textcol <- "grey40"
  
heat_covid = df %>% 
    ggplot(aes(date, fct_rev(province), fill = lab_death)) +
  geom_tile(colour="grey90",size=0.15)  +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_viridis_d(option = "plasma",
                     na.value = "grey65") +
  labs(x = "",
       y = "",
       fill = "",
       title = "Fallecimientos por COVID en Espa??a", 
       caption = "#30diasdegraficos \n Fuente: lab.montera34.com/covid19 |  @EdudinGonzalo") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Avenir"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=10,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour="grey25",hjust=0,size=14,face="bold"))


ggsave("figs/heat_covid.png", heat_covid)

