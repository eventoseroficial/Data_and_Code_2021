#####################################################
#######         TALK PROPOSAL             ###########
#######             ***                   ###########
#######            V SER                  ###########
#######             ***                   ###########
#######    Orlando Fonseca Guilarte       ###########


library(dplyr)

##############################
######      IGRAPH      ######

library(igraph)

id <- 1:10
title <- paste("Paper_", LETTERS[1:10], sep="")
year<- c(2017, 2018,2018,2018,2019,2019,2020,2020,2020,2021)
nodes <- data.frame(id, title,year)  #level


from<-c(2,3,4,5,5,6,6,7,7,8,9,9,9,10,10)
to  <-c(1,1,1,1,2,2,3,3,8,3,3,4,1,8,4)
relationship <- data.frame(from, to)


net <- graph_from_data_frame(d=relationship, vertices=nodes, directed=T) 

plot(net) 


degree_value <- degree(net, mode = "in")
nodes$value <- degree_value[match(nodes$id, names(degree_value))]  #Number of citations

network <- graph_from_data_frame(d=relationship, vertices=nodes, directed=T) 

library(RColorBrewer)
colorBrewer <- brewer.pal(n = 5, name = "Dark2")

nodes$level<-c(1,2,2,2,3,3,4,4,4,5)
nodes<- nodes %>% mutate(color= colorBrewer[level], label = title )
V(network)$color <- colorBrewer[nodes$level]

nodes<- nodes %>% mutate(size = (value + 5) * 2 )
V(network)$size <- (V(network)$value + 5) * 2  

plot(network) 

plot(network, edge.arrow.size=0.4, edge.color="gray50", vertex.label= V(network)$title,vertex.label.dist=2,
     margin=-0.1)

layout <- layout_in_circle(network)
plot(network, layout=layout)


interactive <- tkplot(network) 


##############################
######      TREE JS     ######


library(threejs) 

graph_js <- graphjs(network)   #READ igraph Object !!
print(graph_js)  


################################
######      NETWORKD3     ######

library(networkD3)

simpleNetwork(relationship,fontFamily = "sans-serif",zoom = TRUE)


library(data.tree)
library(tidyr)

network <- graph_from_data_frame(d=relationship, vertices=nodes, directed=T) 
GraphD3 <- igraph_to_networkD3(network, group = nodes$level,what = 'both')

nodes<- nodes %>% mutate(size = (value + 1) * 10 ) 

GraphD3$nodes$size<- nodes$size

GraphD3$links$value<- 10

# Plot as a force Directed Network
fn<-forceNetwork(Links = GraphD3$links, Nodes = GraphD3$nodes, Source = 'source',
             Target = 'target', NodeID = 'name', Group = 'group',
             zoom = TRUE, linkDistance = 100,Nodesize = 'size',fontSize = 18,
             fontFamily = "serif",charge = -30,linkColour = "#666", 
             opacity = 1, arrows = TRUE, bounded = FALSE, opacityNoHover = 0,clickAction = NULL,Value = "value")  


fn

library(htmlwidgets)
html<-htmlwidgets::onRender(
  fn,
  'function() { 
    d3.select("body").style("background-color", "black");
  }'
)

saveWidget(html, file="NetworkD3 Visualization.html")
browseURL("NetworkD3 Visualization.html")


#################################
######      VISNETWORK     ######

library(visNetwork)

visNetwork(nodes, relationship)


visNetwork(nodes,relationship) %>%
  visNodes(
    shape = "dot",
    shadow = list(enabled = TRUE, size = 5)) %>%
  visEdges(arrows ="to")


visNetwork(nodes,relationship) %>%
  visNodes(
    shape = "dot",
    shadow = list(enabled = TRUE, size = 5)) %>%
  visEdges(arrows ="to") %>%
  visHierarchicalLayout()

visNetwork(nodes, relationship) %>%
  visNodes(
    shape = "dot",
    shadow = list(enabled = TRUE, size = 5)) %>%
  visEdges(arrows = "to", smooth = list(type = "vertical")) %>%
  visHierarchicalLayout() %>%
  visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE)    


###################

