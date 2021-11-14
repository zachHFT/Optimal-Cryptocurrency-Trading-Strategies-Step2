library(igraph)
library(ggplot2)
library(dplyr)
library(influential)
library(ggraph)
library(graphlayouts)

authors <- scan("g_nodes.txt", what="", sep="\n") #scan in node labels
g <- read_graph("graph_for_backtesting.gml", format='gml') #get graph from gml file
V(g)$names <- authors #assign node labels to graph 

#calculate some properties of the nodes for plotting
deg.in <- degree(g, mode="in") #in degree
deg.out <- degree(g, mode='out') #out degree
deg.total <- degree(g, mode='total')

node_data <- data.frame(Author=authors, InDegree=deg.in, OutDegree=deg.out, Degree=deg.total)
node_data <- node_data %>% arrange(desc(deg.in))

summary(deg.in)
hist(deg.in)
boxplot(deg.in)

##those with highest in degree have their comments replied to the most often

top_fifty <- node_data[1:50,] #top fifty redditors measured by in degree
top_fifty %>% ggplot(aes(x=reorder(Author, desc(InDegree), sum), y=InDegree)) + #reorder bar heights to descending
  geom_bar(stat='identity') +
  xlab("Reddit author") +
  ggtitle(label = "Top fifty reddit authors in crypto related subreddits by in-degree") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #rotate axis labels

##those with highest out degree are the most active commenters

top_fifty_commenters <- node_data[1:50,] #top fifty redditors measured by out degree
top_fifty_commenters %>% ggplot(aes(x=reorder(Author, desc(OutDegree), sum), y=OutDegree)) + #reorder bar heights to descending
  geom_bar(stat='identity') +
  xlab("Reddit author") +
  ggtitle(label = "Top fifty reddit authors in crypto related subreddits by out-degree") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),) #rotate axis labels

##identify influencers in the network using recent network methods 
##https://cran.r-project.org/web/packages/influential/vignettes/Vignettes.html#IVI

IVI <- ivi(g, vertices=V(g), directed=TRUE, loops=FALSE, scaled=TRUE, mode='in', d=3)

most_influential_authors <- V(g)$names[order(IVI, decreasing=TRUE)[1:10]]
write(most_influential_authors, file='most_influential_authors')

network_vis <- cent_network.vis(graph = g,
                                cent.metric = IVI,
                                layout='grid',
                                directed = TRUE,
                                plot.title = "IVI-based Network",
                                legend.title = "IVI value")

network_vis

####### visualisation #######

n <- 3
in_deg_more_than_n <- V(g)[degree(g, mode='in') > n]
g_reduced <- induced_subgraph(g, vids=in_deg_more_than_n)
largest_connected_component_g_reduced_index <- which(components(g_reduced)$membership==1)
largest_connected_component_g_reduced <- induced_subgraph(g_reduced,largest_connected_component_g_reduced_index)
largest_connected_component_g_reduced <- largest_connected_component_g_reduced %>%
  set_vertex_attr("IVIcentrality", value=IVI[which(V(g)$names %in% V(largest_connected_component_g_reduced)$names)])


# this function is borrowed from the ambient package
normalise <- function (x, from = range(x), to = c(0, 1)) {
  x <- (x - from[1])/(from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}
# map to the range you want
V(largest_connected_component_g_reduced)$degree_normalised <- normalise(degree(largest_connected_component_g_reduced), to = c(3, 11))

ggraph(largest_connected_component_g_reduced, layout = "manual", x = V(largest_connected_component_g_reduced)$x,
    y = V(largest_connected_component_g_reduced)$y) + 
	 geom_edge_link(aes(end_cap = circle(2,'mm')),
	                    edge_colour = "#A8A8A8", 
	                    edge_width = 0.2,
	                    edge_alpha = 1,
	                    arrow = arrow(angle = 30, length = unit(1, "mm"), 
	                                  ends = "last", type = "closed")) + 
  geom_node_point(aes(size = round(IVIcentrality, digits=1),
                      alpha = IVIcentrality/0.6),
                  fill = "green", 
                  shape = 21, 
                  stroke = 0.1) +
  geom_node_text(aes(label= ifelse(IVIcentrality > 4.5, round(IVIcentrality,digits=1), ""),
                 family = 'Palatino',
                 size = IVIcentrality/8)) +
  scale_size(range = c(0, 16)) + 
	theme_graph() + 
	theme(legend.position = "none")

palette <- c("#1A5878", "#C44237", "#AD8941", "#E99093", 
                 "#50594B", "#8968CD", "#9ACD32")

centrality_layout <- ggraph(largest_connected_component_g,
                            layout = "centrality",
                            cent = largest_connected_component_IVI) +
  geom_edge_link0(edge_colour = "grey66") +
  geom_node_point(shape = 21)+
  #geom_node_text(label=V(largest_connected_component_g)$names, family = "serif")+
  scale_edge_width_continuous(range = c(0.2,0.9))+
  scale_size_continuous(range = c(1,8))+
  scale_fill_manual(values = palette)+
  coord_fixed()+
  theme_graph()+
  theme(legend.position = "none")

centrality_layout


##################centrality measures##############################################
bc <- function(g) {betweenness(g, v=V(g), directed=TRUE)}
nc <- function(g) {neighborhood.connectivity(g, vertices=V(g), mode='in')}
Hindex <- function(g) {h_index(g, vertices=V(g), mode='in')}
LHindex <- function(g) {lh_index(g, vertices=V(g), mode='in')}
ci <- function(g) {collective.influence(g, vertices=V(g), mode='in', d=3)}
cRank <- function(g) {clusterRank(g, vids=V(g), directed=TRUE, loops=FALSE)}

centrality.measures.functions <- list("bc"=bc, "nc"=nc, "Hindex"=Hindex, "LHindex"=LHindex,
                            "ci"=ci, "cRank"=cRank)
N <- length(V(g))
centrality.measures <- matrix(rep(0, N*6), nrow=N, ncol=6)
colnames(centrality.measures) <- names(centrality.measures.functions)
for (i in 1:6){
  centrality.measures[,i] <- centrality.measures.functions[[i]](g)
}

centrality.measures <- cbind(centrality.measures, deg.total)
###########################################################################


