library(igraph)
library(ggplot2)
library(dplyr)
library(influential)
library(ggraph)
library(graphlayouts)

largest_connected_component_g_reduced <- read_graph("largest_connected_component_g_reduced",
                                                    format="gml")

ggraph(largest_connected_component_g_reduced, 
       layout = "manual", 
       x = V(largest_connected_component_g_reduced)$x,
       y = V(largest_connected_component_g_reduced)$y) + 
  geom_edge_link(aes(end_cap = circle(1.5, 'mm')),
                 edge_colour = "#A8A8A8", 
                 edge_width = 0.3,
                 edge_alpha = 1, 
                 arrow = arrow(angle = 30, 
                               length = unit(1.5, "mm"), 
                               ends = "last", 
                               type = "closed")) + 
  geom_node_point(aes(size = IVIcentrality), 
                  fill = "#FF4500", 
                  colour = "white", 
                  shape = 21, 
                  stroke = 1) + 
  #geom_node_text(aes(label= ifelse(IVIcentrality > 4.5,V(largest_connected_component_g_reduced)$names,""),
  #family = 'Palatino')) +
  scale_size(range = c(0, 25)) + 
  theme_graph() + 
  theme(legend.position = "right") 
