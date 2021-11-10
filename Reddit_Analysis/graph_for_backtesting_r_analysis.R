library(igraph)
library(ggplot2)
library(dplyr)

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

top_fifty <- node_data[1:50,] #top fifty redditors measured by in degree
top_fifty %>% ggplot(aes(x=reorder(Author, desc(InDegree), sum), y=InDegree)) + #reorder bar heights to descending
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #rotate axis labels

