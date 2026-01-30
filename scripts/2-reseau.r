library(visNetwork)
library(tidyverse)

nodes <- read_csv("data/nodes.csv") %>% 
  mutate(label = name,group = as.character(group) )

edges <- read_csv("data/edges.csv")

# PARTIE 1 : Extraction des pays
legend_data <- nodes %>%
  distinct(group) %>%
  transmute(
    label = group,           # Légende
    group = group,           # Mêmes couleurs dans les bulles et la légende
    font.size = 35,          # Police extra grande
    shape = "dot"
  )

# PARTIE 2 : Afficher le réseau
visNetwork(nodes, edges, width = "100%") %>%
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle") %>%
  visLegend(
    addNodes = legend_data,  
    useGroups = FALSE,     
    width = 0.3,
    stepY = 100         
  )