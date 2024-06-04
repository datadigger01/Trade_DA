
# network package
install.packages('igraph')
install.packages('ggraph')

# install.packages('tidygraph')

library('igraph')
library('ggraph')
# library('tidygraph')



# read file for network
df <- read.csv("./wine_network_sample.csv")

# filter
df_network <- df %>% mutate(wine_cd = substr(cmd_code,1,2)) %>% 
                     filter(wine_cd == '22' & cmd_code %in% c(2204) & net_wgt > 1000000) %>% 
                     filter(flow_code == "X" & !partner_iso %in% c('_X ','X1 ','X2 ','XX ', 'E19','S19')) %>% 
                     mutate(std_net_wgt = ((net_wgt - min(net_wgt) ) / max(net_wgt)))

#unique(df_network$cmd_code)
#quantile(df_network$net_wgt, na.rm=T)


# Assuming 'source' and 'target' are the column names for the edges
edges <- df_network %>% select(reporter_desc, partner_desc, flow_desc, std_net_wgt)

# Create a graph object
g <- graph_from_data_frame(edges, directed = T)

as_data_frame(g, what = 'vertices')
as_data_frame(g, what = 'edges')
# check edges(link) & edge(vertices)

V(g)$name
# Print network standard weight
E(g)$std_net_wgt


# Basic summary of the graph
# summary(g)

# Degree distribution
degree_distribution <- degree(g)
hist(degree_distribution, main="Degree Distribution", xlab="Degree", ylab="Frequency")

# Centrality measures
degree_centrality <- degree(g, mode="all")
betweenness_centrality <- betweenness(g)
closeness_centrality <- closeness(g)

data.frame(degree(g, mode='all'), closeness(g), betweenness(g))

# Community detection using the Louvain algorithm
# communities <- cluster_louvain(g)
# Print community sizes
# sizes(communities)


# Plot the network
plot(g, 
     vertex.color=rainbow(40),
     vertex.size= degree(g, mode='out') * .5,
     # vertex.size= betweenness(g)* .05,
     vertex.label.cex=0.8,
     # edge.color = "light gray",
     edge.width = E(g)$std_net_wgt * 10,
     edge.arrow.size=0.1,
     edge.arrow.width=0.7,
    
     # edge.arrow.width = E(g)$std_net_wgt,
     # edge.arrow.width = E(g)$qty,
     # layout=layout_with_kk(g),
     # layout=layout_on_grid(g),
     layout=layout_on_sphere(g),
     # layout=layout_randomly(g),
     main="Wine Network")


# plot(communities, g,
#      vertex.size=degree(g) * 0.12,
#      # vertex.size=betweenness(g)*0.01,
#      vertex.label.cex=0.8,
#      edge.arrow.size=0.1,
#      # layout=layout_randomly(g),
#      layout=layout_on_grid(g),
#      main="Wine Network with Communities")

