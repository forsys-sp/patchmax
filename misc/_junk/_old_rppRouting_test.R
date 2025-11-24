pacman::p_load(cppRouting, igraph, proxy, sf, dplyr, ggplot2, 
               concaveman, ggmap, tmap, animation)

source('misc/rppRouting_func.R')

# load data and identify adjacency
test_forest <- forsys::test_forest
# test_forest <- test_forest |> filter(!stand_id %in% c(4050:4070))
data <- test_forest |> st_drop_geometry() |> as.data.frame()
adj <- Patchmax::calculate_adj(test_forest, St_id = test_forest$stand_id, calc_dst = TRUE)
igraph::vertex_attr(adj) <- data |> dplyr::select(matches('priority'))
xy <- sf::st_centroid(test_forest) |> sf::st_coordinates() |> as.data.frame()
nodes <- V(adj) |> as_ids() |> as.numeric()

# define key parameters
r = 3060
field = 'priority4'
area_field = 'area_ha'
max_area = 100000
sdw = 10

out <- 1:10 |> purrr::map(function(x){
  print(x)
  sdw = x
  # calculate distance cost edge list (0.08 sec)
  rpp_edgelist <- calc_cost_edgelist(net = adj, xy = xy, objective_field = field, sdw = sdw)
  # convert to rpp graph object (0.07 sec)
  rpp_graph <- rpp_edgelist |> select(from, to, dist_adj) |> makegraph()
  # calculate distance matrix (0.01 sec)
  rpp_dmat <- rpp_graph |> get_distance_matrix(from=r, to=nodes, allcores=TRUE) 
  # convert into nearest node list (<0.01 sec)
  rpp_nn <- rpp_dmat |> calc_nearest_nodes(area_field = 'area_ha', proj_area = max_area)
  # add objective to nearest node list (<0.01 sec)
  rpp_nn$objective <- data[match(rpp_nn$node, data$stand_id),field]
  return(rpp_nn)
})

seq1 <- c(seq(0.1,1,by=0.1),seq(1,5,by=0.25))
seq1 <- c(seq(10000,200000, by=2500))

ani.options(interval = 0.2, nmax = 300)
saveVideo({
  # for(i in seq(10000,200000, by=2500)){
  for(i in c(seq1, rev(seq1))){
    
    r = 5050
    field = 'priority4'
    area_field = 'area_ha'
    max_area = 100000
    sdw = 1
    
    print(i)
    max_area = i
    # sdw = i

    rpp_edgelist <- calc_cost_edgelist(net = adj, xy = xy, objective = data$priority4, sdw = sdw)
    rpp_graph <- rpp_edgelist |> select(from, to, dist_adj) |> makegraph()
    rpp_dmat <- rpp_graph |> get_distance_matrix(from=r, to=nodes, allcores=TRUE)
    rpp_nn <- rpp_dmat |> calc_nearest_nodes(area_field = 'area_ha', proj_area = max_area)
    rpp_nn$objective <- data[match(rpp_nn$node, data$stand_id),field]
    plot_project(test_forest |> mutate(objective = get(field)), adj, rpp_nn, buffer = 50000)
  }
}, other.opts = '-pix_fmt yuyv422', ani.width = 1000, ani.height = 1000, ani.res = 150, interval = 0.01)



plot_project(test_forest |> mutate(objective = get(field)), adj, rpp_nn, buffer = 1000000)




library(animation)
v <- c(1/5:1, seq(1, 5, by=0.25)[-1])
v <- c(v, rev(v))

r = 4845
r = 5050
r = 5555

saveGIF({
  for(i in 1:length(v)){
    
    ani.options(interval = 0.2, nmax = 300)
    area_crit <- 175
    
    sdw = v[i]
    E(adj)$dist_mod <- E(adj)$dist * E(adj)$priority^sdw
    edges <- igraph::as_edgelist(adj, names=F) |> cbind(1)
    edges[,3] <- E(adj)$dist_mod
    nodes <- 1:10000
    grph <- makegraph(edges)
    dist <- get_distance_matrix(Graph=grph, from=nodes[r], to=nodes, allcores=TRUE)
    nearest_nodes <- dist[1,] |> sort() |> head(area_crit)
    nearest_nodes_df <- data.frame(node = names(nearest_nodes), dist = nearest_nodes)
    sg <- igraph::subgraph(adj, vids = nearest_nodes_df$node)
    V(sg)$color <- leaflet::colorNumeric("Blues", domain = NULL)(V(sg)$objective)
    xy_s <- xy[match(nearest_nodes_df$node, V(adj)$name),] |> as.matrix()
    objective_sum <- sum(V(sg)$objective)
    
    # network graph 
    # plot(sg, layout = xy_s, vertex.size = 10, vertex.label = NA, edge.color = NA,
    #      edge.arrow.size = 0, vertex.shape = "square")
    # s_ids <- V(sg)$name
    
    bbox <- data |> filter(stand_id == r) |> st_buffer(20000) |> st_bbox() |> st_as_sfc()
    data2 <- data |> st_crop(bbox)
    p <- ggplot(data2, aes(fill = priority4)) + 
      geom_sf(alpha=0.4, color='white') +
      geom_sf(data = data |> filter(stand_id %in% V(sg)$name)) +
      geom_sf(data = data |> filter(stand_id == r), fill='yellow') +
      scale_fill_viridis_c(direction = -1) + 
      cowplot::theme_map() + 
      guides(fill = 'none') +
      labs(
        title = paste0('Stand ID: ', r), 
        subtitle = paste0('Flexibility = ', round(sdw,1), 
        ', Objective: ', 100 - round(objective_sum)
        ))
    print(p)
    # ggsave(paste0('misc/patchmax_',i,'.png'))
  }
}, movie.name = paste0('animation_',r,'.gif'))


E(adj)$dist_mod <- E(adj)$dist * E(adj)$priority^sdw
edges <- igraph::as_edgelist(adj, names=F) |> cbind(1)
edges[,3] <- E(adj)$dist_mod
nodes <- 1:10000
grph <- makegraph(edges)
dist <- get_distance_matrix(Graph=grph, from=nodes[r], to=nodes, allcores=TRUE)
nearest_nodes <- dist[1,] |> sort() |> head(100)
nearest_nodes_df <- data.frame(node = names(nearest_nodes), dist = nearest_nodes)
sg <- igraph::subgraph(adj, vids = nearest_nodes_df$node)
V(sg)$color <- leaflet::colorNumeric("Blues", domain = NULL)(V(sg)$objective)
xy_s <- xy[match(nearest_nodes_df$node, V(adj)$name),] |> as.matrix()
# plot(sg, layout = xy_s, vertex.size = 10, vertex.label = NA, edge.color = NA,
#      edge.arrow.size = 0, vertex.shape = "square")
# s_ids <- V(sg)$name

bbox <- data |> filter(stand_id == r) |> st_buffer(15000) |> st_bbox() |> st_as_sfc()
data2 <- data |> st_crop(bbox)
ggplot(data2, aes(fill = priority4)) + 
  geom_sf(fill='grey', color='white') +
  geom_sf(data = data |> filter(stand_id %in% V(sg)$name)) +
  scale_fill_viridis_c() + 
  cowplot::theme_map()

          