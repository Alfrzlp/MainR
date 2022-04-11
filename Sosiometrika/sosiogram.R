library(tidyverse)
library(ggraph)
library(tidygraph)
library(babynames)


get_random_names <- function(n) { 
  unique_babynames <- distinct(babynames, name, .keep_all = TRUE) 
  index <- sample(1:nrow(unique_babynames), n, replace = FALSE) 
  names <- unique_babynames[index, ] 
  names 
}
nodes <- get_random_names(9)
nodes

source <- sample(1:nrow(nodes), nrow(nodes)*2, replace = TRUE)  
target <- sample(1:nrow(nodes), nrow(nodes)*2, replace = TRUE)

links <- data.frame(source, target)
links <- data.frame(
  src = source,target
  ) %>%  
  filter(!(src == target)) 
links

links <- unique(links[c("src", "target")])   
links

social_net_tbls <- tbl_graph(nodes = nodes, 
                             edges = links, 
                             directed = FALSE)
social_net_tbls



social_net <- 
  ggraph(social_net_tbls, layout = "kk") +                                                                                                         
  geom_node_point(
    aes(colour = sex),
    size = 5
  ) +                                         
  geom_node_text(aes(label = name), nudge_y = 0.05, nudge_x = 0.2)+ 
  geom_edge_link() +
  theme_void()

# Render the network 
show(social_net)



library(networkD3)  

# Create fake data
src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)
networkData <- data.frame(dat$no, dat$apg1)
networkData


# Plot
simpleNetwork(networkData, zoom = T)



data(MisLinks)
MisLinks
data(MisNodes)
MisNodes

MisNodesNew <- MisNodes %>% 
  mutate(
    group = sample(c('Female', 'Male'), 77, replace = T),
    sex = if_else(group == 'Female', "#bf3eff", "#666")
  ) %>% 
  dplyr::select(-size)

MisNodesNew

my_color <- 'd3.scaleOrdinal() .domain(["Male", "Female"]) .range(["#00BFC4", "#F8766D"])'
# Plot
forceNetwork(
  Links = MisLinks,
  Nodes = MisNodesNew,
  Source = "source",
  Target = "target",
  NodeID = "name",
  Group = "group",
  opacity = 0.9,
  zoom = T,
  fontSize = 15, 
  arrows = F,
  legend = T,
  fontFamily = 'Arial',
  opacityNoHover = 0,
  colourScale = my_color
) %>% 
  saveNetwork(file = 'E:/Net1.html')




library(igraph)

# Create data
set.seed(10)
data <- matrix(sample(0:1, 35^2, replace=TRUE), nrow=35)
colnames(data) = rownames(data) = LETTERS[1:5]


# build the graph object
network <- graph_from_adjacency_matrix(data, diag = F)

# plot it
par(mar = c(0, 0, 0, 0))
plot(network)


