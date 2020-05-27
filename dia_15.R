# Dia 15: 26/05/2020
# Treemaps
# @EdudinGonzalo
#---------------------------------------------


library(readxl)
library(cluster)
library(tidyverse)
library(factoextra)




seguridad <- read_excel("data/datos_abril_2020.xlsx", range = "A3:F24") %>% janitor::clean_names()

rownames(seguridad) = seguridad$distritos

res.hc <- seguridad %>%
  select(-distritos) %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2") 

dend = fviz_dend(res.hc, k = 3, # Cut in four groups
          cex = 0.5, # label size
          #k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "red"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups,
          main = "Dendograma Seguridad en Madrid",
          xlab = "",
          ylab = ""
          caption =  "#30diasdegraficos \n Fuente: Ayuntamiento de Madrid | @EdudinGonzalo",
          horiz = TRUE
          
)

ggsave("figs/dend_seg.png", dend, width = 9)
