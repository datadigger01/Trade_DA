
# network package
# install.packages('igraph')
# install.packages('tidygraph')
# install.packages('ggraph')


library('igraph')
library('ggraph')
library('tidygraph')


library('tidyverse')

# read file for network
df_coeff_23 <- read_csv("./trade/coffee_trade_all_2023.csv")
df_coeff_22 <- read_csv("./trade/coffee_trade_all_2022.csv")
df_coeff_21 <- read_csv("./trade/coffee_trade_all_2021.csv")
df_coeff_20 <- read_csv("./trade/coffee_trade_all_2020.csv")


df_coeff_4y <- rbind(df_coeff_20, df_coeff_21, df_coeff_22, df_coeff_23)
df_coeff_4y %>% select(cmd_code, cmd_desc) %>% distinct_all() %>% view()

# filter
df_network <- df_coeff_4y %>% select(ref_year,
                                     ref_month,
                                     reporter_iso,
                                     reporter_desc,
                                     flow_code,
                                     flow_desc,
                                     partner_iso,
                                     partner_desc,
                                     cmd_code,
                                     net_wgt,
                                     primary_value) %>%
                              filter(flow_code =='X' & cmd_code %in% c('090111','090112','090121','090122') & net_wgt > 1000000 ) %>%
                              mutate(std_p_value = ((net_wgt - min(net_wgt)) / (max(net_wgt)-min(net_wgt))) )
#unique(df_network$cmd_code)
# quantile(df_network$std_p_value, na.rm=T)




# Assuming 'source' and 'target' are the column names for the edges
edges <- df_network %>% select(reporter_desc, 
                               partner_desc, 
                               cmd_code, 
                               flow_desc,
                               net_wgt, 
                               std_p_value, 
                               ref_year,
                               reporter_iso,
                               partner_iso)
# generate all node from sourc and target and add regional info 
df_network_node1 <- df_network %>% select(reporter_iso, reporter_desc) %>% 
                                   rename(iso3c = reporter_iso,country= reporter_desc) %>% 
                                   distinct_all()
df_network_node2 <- df_network %>% select(partner_iso, partner_desc) %>% 
                                   rename(iso3c = partner_iso, country= partner_desc) %>% 
                                   distinct_all()
nodes <- rbind(df_network_node1, df_network_node2) %>% distinct_all()

country_info <- read.csv("./country_region_code.csv")
all_nodes <- country_info %>% select(alpha.3, region, sub.region) %>%  
                              rename(iso3c      = alpha.3,
                                     sub_region = sub.region) %>% 
                              left_join(nodes, ., by=c('iso3c')) %>%
                              select(country, region, sub_region)


# Create a graph object
g <- graph_from_data_frame( edges, directed = T, vertices = all_nodes )

# Basic summary of the graph
# summary(g)

# Degree distribution
# degree_distribution <- degree(g)
# hist(degree_distribution, main="Degree Distribution", xlab="Degree", ylab="Frequency")

# Centrality measures
# degree_centrality <- degree(g, mode="out")
# betweenness_centrality <- betweenness(g)
# closeness_centrality <- closeness(g)
# data.frame(degree(g, mode='out'), closeness(g), betweenness(g))

# E(g)$std_p_value

#### Plot the network
plot(g, 
     vertex.color=rainbow(40),
     vertex.size= degree(g, mode = 'out') * 0.2,
     # vertex.size= betweenness(g),

     vertex.label.cex=0.8,
     # edge.color = "light gray",
     # edge.width = E(g)$std_p_value,
     edge.arrow.size=0.1,
     edge.arrow.width=0.5,
    
     # edge.arrow.width = E(g)$std_net_wgt,

     # layout=layout_with_kk(g),
     # layout=layout_on_grid(g),
     layout=layout_on_sphere(g),
     # layout=layout_randomly(g),
     main="coeff Network")




# Convert to tidygraph object
tidy_g <- as_tbl_graph(g)

tidy_g <- as_tbl_graph(g) %>% 
          mutate(degree   = centrality_degree(mode = 'out'),
                 between  = centrality_betweenness(),
                 eigen    = centrality_eigen(),
                 close    = centrality_closeness(),
                 hub      = centrality_hub(),
                 auth     = centrality_authority())
V(tidy_g)$degree
#V(tidy_g)$hub
E(tidy_g)$std_p_value
E(tidy_g)$flow_desc


central_network <- data.frame(ids = V(tidy_g)$name, net = "trade", 
                              degree =    V(tidy_g)$degree, 
                              between =   V(tidy_g)$between, 
                              closeness = V(tidy_g)$close, 
                              hub =       V(tidy_g)$hub,
                              authority = V(tidy_g)$auth, 
                              eigen =     V(tidy_g)$eigen)

E(tidy_g)$std_p_value
# Create a basic network plot with ggraph
tidy_g %>% activate(edges) %>%  filter(net_wgt > 500000 ) %>% 
                                filter(ref_year >= 2020 & ref_year < 2021) %>%
           # activate(nodes) %>% filter(region %in% c('Europe','Africa','Americas')) %>%
           ggraph(layout = 'igraph', algorithm = 'sphere') +
                  geom_edge_fan(aes(edge_alpha = std_p_value, edge_width= std_p_value,color=cmd_code),
                                    arrow = arrow(length = unit(4, 'mm')),
                                    end_cap = circle(4, 'mm'),
                                    # color='darkgray',
                                    show.legend = T) +
                  geom_node_point(aes( alpha = degree, size = degree, color=region)) +
                  scale_size_continuous(range=c(2,12))+
                  geom_node_text(aes(label = name), repel = F, size = 3) + 
                  facet_grid(~ref_year) +
                  ggtitle("Coffee Export Network")
