# network package
install.packages('igraph')
install.packages('ggraph')

library('igraph')
library('ggraph')

## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(
            name = c("Alice", "Bob", "Cecil", "David","Esmeralda"),
            age = c(48, 33, 45, 34, 21),
            gender = c("F", "M", "F", "M", "F")
          )
relations <- data.frame(
            from = c("Bob", "Cecil", "Cecil", "David","David", "Esmeralda"),
              to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
            same.dept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
            friendship = c(4, 5, 5, 2, 1, 1), 
            advice = c(4, 5, 5, 4, 2, 3)
          )

gr <- graph_from_data_frame(relations, directed = F, vertices = actors)

V(gr)$name
E(gr)$friendship


## The opposite operation
as_data_frame(gr, what = "vertices")
as_data_frame(gr, what = "edges")

?igraph.plotting
# Plot the network
plot(gr, 
     edge.width = E(gr)$friendship,
     edge.arrow.size=0.1,
     edge.arrow.width=1.2,
     # edge.label	= E(gr)$friendship,
     
     layout=layout_with_kk(gr),
     # layout=layout_on_grid(gr),
     # layout=layout_on_sphere(gr),
     main="Friendship Network"
     )

# Centrality measures
degree <- degree(gr, mode="all")
betweenness <- betweenness(gr)
closeness <- closeness(gr)
igen_centrality <- eigen_centrality(gr)


data.frame(degree, betweenness, closeness, igen_centrality$vector)

head(gr)
