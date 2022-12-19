############ PREP DATA ################

# load data and identify adjacency
geom <- forsys::test_forest %>% 
  mutate(priority5 = range01(sqrt(priority1 + priority2 + priority4))) %>%
  mutate(boundary3 = range01(boundary1 * (mosaic1 + priority4 * 3))) %>%
  mutate(revenue = ((priority2 + priority4 - cluster1) * 1000) + 2400)

geom$X <- rep(c(1:100), 100)
geom$Y <- floor(geom$stand_id / 100) + 1
geom <- geom %>% filter(X <= 23 & Y <= 23) %>% dplyr::select(-X, -Y)

# build adjacency network
adj_net <- calc_adj_network_func(geom, St_id = geom$stand_id, calc_dist = TRUE)
vertex_attr(adj_net) <- bind_cols(vertex_attr(adj_net), select(geom, matches('priority|threshold'), area_ha))
vertex_attr(adj_net, name = 'objective') <- vertex_attr(adj_net, 'priority5')
vertex_attr(adj_net, name = 'area') <- vertex_attr(adj_net, 'area_ha')
vertex_attr(adj_net, name = 'constraint') <- vertex_attr(adj_net, 'priority2')
