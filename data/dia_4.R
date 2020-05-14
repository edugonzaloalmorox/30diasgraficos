# Day 4: 14/05/2020
# Grafico de arcos
# @EdudinGonzalo
#---------------------------------------------


library(rtweet)
library(tidyverse)
library(tidytext)
library(quanteda)
library(ggraph)
library(tidygraph)

# Tweets ---------------------------------------------
ayuso <- search_tweets(
  "#AyusoGate", n = 10000, include_rts = FALSE
)

# Bigramas -------------------------------------------

bi = ayuso %>%
  select(text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
  

stop_spanish = c(quanteda::stopwords(language = "es"), "https", "19", "covid", "t.co")


bigram_clean = bi %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_spanish) %>%
  filter(!word2 %in% stop_spanish) 


bigram_test = bigram_clean %>% head(., 80)


#  Definicion redes ------------------------------------

# Nodos 
first_word  <- bigram_test %>%
  distinct(word1) %>%
  rename(label = word1)

second_word  <- bigram_test %>%
  distinct(word2) %>%
  rename(label = word2)


nodes <- full_join(first_word, second_word, by = "label") 

nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())


bigram_test <- bigram_test %>%
  rename(weight = n)

# Vertices
edges <- bigram_test %>% 
  left_join(nodes, by = c("word1" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("word2" = "label")) %>% 
  rename(to = id)

edges <- edges %>% select(from, to, weight)

# Crear objeto redes ------------------------------------
net.tidy <- tbl_graph(
  nodes = nodes, edges = edges, directed = TRUE)


# Grafico ----------------------------------------------

arc_ayuso = net.tidy %>% ggraph(layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.3, colour = "blue") + 
  scale_edge_width(range = c(0.4, 6)) +
  geom_node_text(aes(label = label), 
                 size = 2.5, 
                 angle = 90,
                 vjust = 0,
                 fontface = "bold") +
  labs(title = "#AyusoGate en Twitter",
       subtitle = "Relaciones mas frecuentes entre palabras", 
       caption = "#30diasdegraficos \n Fuente: Twitter | @EdudinGonzalo",
    edge_width = "N?? de relaciones ...") +
  theme(text = element_text(family = "Overpass Regular"))+
  theme(legend.position = "bottom", 
        strip.background =element_rect(fill="#ffe887"),
        panel.background = element_rect(fill = "#faf9f5"),
        plot.title = element_text(hjust = 0.5, margin=margin(0,0,6.25,0), size = 20), 
        plot.subtitle = element_text(hjust = 0.5, margin=margin(0,0,5,0), size = 9))

ggsave("figs/arc_ayuso.png", arc_ayuso, width = 9)
       