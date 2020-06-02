# Dia 22. 02/06/2020
# Texto
# @EdudinGonzalo
#---------------------------------------------


library(rtweet)
library(tidyverse)
library(quanteda)
library(tidytext)
library(ggrepel)

# Get data 

rt <- search_tweets(
  "#blackouttuesday", n = 18000, include_rts = FALSE
)

rt_es = rt %>% filter(lang == 'es')

stop_es = c(stopwords("spanish"), "blackouttuesday", "blacklivesmatter", "https", "t.co")


words_rt = rt_es %>% 
  filter(screen_name != "mlynerweb") %>%
  mutate(id = dplyr::row_number()) %>%
  select(id, text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_es) %>%
  group_by(id) %>%
  count(word) %>%
  mutate(total = sum(n), 
         rank = row_number(), 
         term_freq =n/total) 

# Get important words using the  tf-idf
words_rt = words_rt %>%
  bind_tf_idf(word, id, n)

ranks = words_rt %>%
  group_by(word) %>%
  summarise(avg_rank = mean(tf_idf))

# Data to plot 
ranks_clean = ranks[165:4125, ] %>%
  left_join(., rt_es %>% 
              filter(screen_name != "mlynerweb") %>%
              mutate(id = dplyr::row_number()) %>%
              select(id, text) %>%
              unnest_tokens(word, text) %>%
              filter(!word %in% stop_es) %>%
              count(word, sort = TRUE))

  

# Plot
chatter = ranks_clean %>%
  arrange(desc(n)) %>%
  top_n(100, wt = n) %>%
  ggplot(aes(avg_rank, n, label = word)) +
  geom_text_repel(segment.alpha = 0, 
                  aes(colour=avg_rank, size=n)) +
  scale_color_gradient(low="blue", high="orange", 
                       trans = "log10",
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position ="top")) +
  scale_size_continuous(range = c(3, 11),
                        guide = FALSE) +
  theme_minimal() +
  labs(title = "100 palabras m??s frecuentes del #blackouttuesday", 
       subtitle = "Frecuencia (tama??o) ~ Importancia media (color)", 
       x = "Importancia media (tf-idf)", 
       y = "Frecuencia de la palabra", 
       caption =  "#30diasdegraficos \n Fuente:Twitter \n @EdudinGonzalo", 
       colour = "Importancia media") +
  theme(legend.position=c(.99, .99),
        text = element_text(family = "Overpass Regular"),
        legend.justification = c("right","top"),
        plot.title = element_text(size = 24, 
                                  face = "bold",
                                  color="blue"),
        plot.subtitle = element_text(size = 12, 
                                  face = "bold",
                                  color="blue"), 
        plot.caption = element_text(color="blue"),
        panel.grid.major = element_line(colour = "whitesmoke"))
  
  
ggsave("figs/chatter.png", chatter, width = 9)
