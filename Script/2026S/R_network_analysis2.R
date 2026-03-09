
# network package
install.packages('igraph')

library('igraph')
library('tidyverse')

# read file for network
df <- read.csv("./wine_trade_all.csv")

df %>% select(cmd_code, cmd_desc) %>% distinct_all() %>% view()

# filter
df_network <- df %>% mutate(wine_cd = substr(cmd_code,1,2)) %>% 
                     filter(cmd_code %in% c(2204,2205) & net_wgt > 0) %>% 
                     filter(flow_code %in% c("X") & 
                             !partner_iso %in% c('_X ','X1 ','X2 ','XX ', 'E19','F19','S19','ATA')) %>% 
                     mutate(std_net_wgt = ((net_wgt - min(net_wgt)) /(max(net_wgt)-min(net_wgt))))


## all node selection from the source and target
df_network_node1 <- df_network %>% select(reporter_iso, reporter_desc) %>% 
                                   rename(iso3c = reporter_iso,
                                          country= reporter_desc) %>% 
                                   distinct_all()
df_network_node2 <- df_network %>% select(partner_iso, partner_desc) %>% 
                                    rename(iso3c = partner_iso,
                                            country= partner_desc) %>% 
                                    distinct_all()
nodes <- rbind(df_network_node1, df_network_node2) %>% distinct_all()


country_info <- read.csv("./country_region_code.csv")
all_nodes <- country_info %>% select(alpha.3, region, sub.region) %>%  
                                  rename(iso3c      = alpha.3,
                                         sub_region = sub.region) %>% 
                                  left_join(nodes, ., by=c('iso3c')) %>%
                                  select(country, region, sub_region)


# Assuming 'source' and 'target' are the column names for the edges
edges <- df_network %>% select(reporter_desc, partner_desc, flow_desc, cmd_code, net_wgt, std_net_wgt)

# Create a graph object
g <- graph_from_data_frame(edges, directed = T, vertices = all_nodes)

# Basic summary of the graph
# summary(g)
V(g)$region
V(g)$sub_region
E(g)$flow_desc
E(g)$std_net_wgt

##### Degree distribution
# degree_distribution <- degree(g, mode='out')
# hist(degree_distribution, main="Degree Distribution", xlab="Degree", ylab="Frequency")
# quantile(degree_distribution)
#### Centrality measures
# degree_centrality <- degree(g, mode="out")
# betweenness_centrality <- betweenness(g)
# closeness_centrality <- closeness(g)
# data.frame(degree(g, mode='out'), closeness(g), betweenness(g))


# Plot the network
plot(g, 
     vertex.color=rainbow(40),
     vertex.size= degree(g, mode='in') * .2,
     # vertex.size= betweenness(g),
     vertex.label.cex=0.8,
     # edge.color = "light gray",
     edge.width = E(g)$std_net_wgt * 10,
     edge.arrow.size=0.1,
     edge.arrow.width=0.5,
    
     # edge.arrow.width = E(g)$std_net_wgt,
     # edge.arrow.width = E(g)$qty,
     
     # layout=layout_with_kk(g),
     # layout=layout_on_grid(g),
     layout=layout_on_sphere(g),
     # layout=layout_randomly(g),
     main="Wine Export Network")




## Visualization using ggraph
install.packages('tidygraph')
install.packages('ggraph')

library('tidygraph')
library('ggraph')


# Convert to tidygraph object
tidy_g <- as_tbl_graph(g)
tidy_g <- as_tbl_graph(g) %>% 
          mutate(degree   = centrality_degree(mode = 'out'),
                 between  = centrality_betweenness(),
                 eigen    = centrality_eigen(),
                 close    = centrality_closeness()
                )
V(tidy_g)$degree
E(tidy_g)$std_net_wgt
E(tidy_g)$cmd_code
central_network <- data.frame(ids = V(tidy_g)$name, net = "wine trade",
                              degree =    V(tidy_g)$degree,
                              between =   V(tidy_g)$between,
                              closeness = V(tidy_g)$close,
                              eigen =     V(tidy_g)$eigen)


# Create a basic network plot with ggraph
tidy_g %>% activate(edges) %>% filter(net_wgt > 500000) %>% 
           # activate(nodes) %>% filter(eigen > 0) %>%
            ggraph(., layout = 'igraph', algorithm = 'sphere') +
            # ggraph(., layout = 'igraph', algorithm = 'randomly') +
            geom_edge_fan( aes( edge_alpha = std_net_wgt, 
                                edge_width=std_net_wgt, 
                                color = as.factor(cmd_code),
                                ), 
                           arrow = arrow( length = unit(4, 'mm')),
                           end_cap = circle(4, 'mm'),
                          # color='lightgray',
                          show.legend = T) +
            geom_node_point(aes( alpha = degree , size = degree, color=region )) +
            scale_size_continuous(range=c(2,12))+
            geom_node_text(aes(label = name), repel = F, size = 3) + 
            # facet_grid(~flow_desc) +
            theme_void() +
            ggtitle("Wine Export Network")