#####################################################
#######         TALK PROPOSAL             ###########
#######             ***                   ###########
#######            V SER                  ###########
#######             ***                   ###########
#######    Orlando Fonseca Guilarte       ###########

#######   Neo4r 
library(neo4r)

library(dplyr)
library(purrr)

con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j", 
  password = "graphpass"
)


graphResult <- 'MATCH (a:Paper) where a.title = "A Multi-Level Typology of Abstract Visualization Tasks" RETURN a' %>% call_neo4j(con, type = "graph")

graphResult$nodes$properties


paper210_Out <- 'MATCH (a:Paper{idpaper:"210"}) -[r:Citation]-> (b:Paper) RETURN a,r,b' %>% call_neo4j(con, type = "graph")

nodes<-paper210_Out$nodes

relationship<-paper210_Out$relationships

head(nodes)
head(relationship)


library(igraph)

paper210_Out$nodes <- paper210_Out$nodes %>%
  unnest_nodes(what = "properties") 
  
head(paper210_Out$nodes)


paper210_Out$relationships <- paper210_Out$relationships %>%
  unnest_relationships() %>%
  select(startNode, endNode, type, everything())

head(paper210_Out$relationships)

graph_object <- igraph::graph_from_data_frame(
  d = paper210_Out$relationships, 
  directed = TRUE, 
  vertices = paper210_Out$nodes
)

plot(graph_object, vertex.label= V(graph_object)$idpaper)


library(visNetwork)

paper210_Out <- 'MATCH (a:Paper{idpaper:"210"}) -[r:Citation]-> (b:Paper) RETURN a,r,b' %>% call_neo4j(con, type = "graph")


paper210_Out$nodes <- paper210_Out$nodes %>%
  unnest_nodes %>%
  mutate (label=paste(substr(title, start = 1, stop = 15),'..',sep = '')) %>%
  select(id, title, label = label) 
  
head(paper210_Out$nodes)  


paper210_Out$relationships <- paper210_Out$relationships %>%
  unnest_relationships() %>%
  select(from = startNode, to = endNode)

head(paper210_Out$relationships)

visNetwork(paper210_Out$nodes,paper210_Out$relationships)  %>% visEdges(arrows = 'to', smooth =T)

###################### All GRAPH

G <-'MATCH p=()-[r:Citation]-() RETURN p' %>% call_neo4j(con, type = "graph") 










