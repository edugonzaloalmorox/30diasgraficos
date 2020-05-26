# Dia 14: 25/05/2020
# Treemaps
# @EdudinGonzalo
#---------------------------------------------




library(tidyverse)
library(treemapify)
library(scico)
library(treemapify)
library(ggplotify)

devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-04-07')

stage = tuesdata$stage_data %>% tibble()
stages = tuesdata$tdf_stages %>% tibble()
winners = tuesdata$tdf_winners %>% tibble()

tour = winners %>%
  filter(start_date >= "1990-01-01") %>%
  group_by(winner_name, winner_team, nationality) %>%
  summarise(victorias = n(),
            etapas_lider = sum(stages_led)) 
  
tour_plot = ggplot(tour, aes(area = victorias, fill = winner_team,  label = winner_name, subgroup = winner_team)) +
  geom_treemap() +
  viridis::scale_fill_viridis(option = "inferno", discrete = TRUE) +
  geom_treemap_text(fontface = "bold", colour = "grey40", place = "topleft",
                    reflow = TRUE) +
  labs(title = "Ganadores del Tour de Francia desde los 90",
       fill = "",
       caption =  "#30diasdegraficos \n Fuente: tdf (Alastair Rushworth) | @EdudinGonzalo"
       ) +
  geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = .9, colour =
                               "grey50", fontface = "bold", min.size = 0) +
  theme(text = element_text(family = "PT Sans Narrow Bold"), 
        plot.title = element_text(hjust = 0.5,
                                  margin=margin(0,0,10.25,0),
                                  size = 24, 
                                  color="gold2"),
        plot.subtitle = element_text(hjust = 0.5,
                                  margin=margin(0,0,10.25,0),
                                  size = 12, 
                                  color="gold2"),
        plot.caption = element_text(color="gold2"))

ggsave("figs/tour_tree.png", tour_plot, width = 9)



