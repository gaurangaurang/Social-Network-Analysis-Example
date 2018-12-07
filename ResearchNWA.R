#import libraries
library(igraph)
library(readr)
library(haven)

#import data
collNW <- PCMI_Personally.Know_Combined.Edgelist
dissNW <- PCMI_Discussion.Network_Combined_Edgelist

collEL <- collNW
collgraph <- graph.data.frame(collEL, directed = T)

dissEL <- dissNW
dissgraph <- graph.data.frame(dissEL, directed = T)

#First Try
set.seed(123)
plot(collgraph)

#2nd Try
set.seed(123)
#set layout
layout1 <- layout.fruchterman.reingold(collgraph)

V(collgraph)$size = degree(collgraph, mode = 'in')/5 #reduce high in degree node size

V(collgraph)$color = 'grey'
V(collgraph)[degree(collgraph, mode = 'in')>8]$color = "yellow" #High in degree nodes
E(collgraph)$color = 'grey'
plot(collgraph, edge.arrow.size = 0.25, edge.arrow.mode = '-') #got rid of arrowheads

#Remove Self Loops (3rd try)
collgraph2 <- simplify(collgraph, remove.multiple = T, remove.loops = T)

set.seed(123)
layout1 <- layout.fruchterman.reingold(collgraph2)
V(collgraph2)$size = degree(collgraph2, mode = 'in')/5

V(collgraph2)$color = 'grey'
V(collgraph2)[degree(collgraph2, mode = 'in')>8]$color = "yellow"
E(collgraph2)$color = 'grey'
plot(collgraph2, edge.arrow.size = 0.25, edge.arrow.mode = '-')


#4th try
collAttr <- PCMI_Know.Personally_Combined_Nodelist
set.seed(123)
layout1 <- layout.fruchterman.reingold(collgraph2)
V(collgraph2)$size = degree(collgraph2, mode = 'in')/5

V(collgraph2)$color = 'grey'
V(collgraph2)[degree(collgraph2, mode = 'in')>8]$color = "yellow"
V(collgraph2)$color = ifelse(collAttr[V(collgraph2), 2] =="Researcher", 'blue', 'red')
E(collgraph2)$color = 'grey'
plot(collgraph2, edge.arrow.size = 0.25, edge.arrow.mode = '-')#better than previous graphs

#Layout fruchterman.reingold without vertex labels (5th try)
collAttr <- PCMI_Know.Personally_Combined_Nodelist
set.seed(123)
layout1 <- layout.fruchterman.reingold(collgraph2, niter = 500)
V(collgraph2)$size = degree(collgraph2, mode = 'in')/5

V(collgraph2)$color = 'grey'
V(collgraph2)[degree(collgraph2, mode = 'in')>8]$color = "yellow"
V(collgraph2)$color = ifelse(collAttr[V(collgraph2), 2] =="Researcher", 'blue', 'red')
E(collgraph2)$color = 'grey'
plot(collgraph2, edge.arrow.size = 0.25, edge.arrow.mode = '-', vertex.label = NA)

# Layout KK without vertex labels
collAttr <- PCMI_Know.Personally_Combined_Nodelist
set.seed(123)
layout1 <- layout.kamada.kawai(collgraph2)
V(collgraph2)$size = degree(collgraph2, mode = 'in')/5

V(collgraph2)$color = 'grey'
V(collgraph2)[degree(collgraph2, mode = 'in')>8]$color = "yellow"
V(collgraph2)$color = ifelse(collAttr[V(collgraph2), 2] =="Researcher", 'blue', 'red')
E(collgraph2)$color = 'grey'
plot(collgraph2, edge.arrow.size = 0.25, edge.arrow.mode = '-', vertex.label = NA)

