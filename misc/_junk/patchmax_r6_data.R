############ PREP DATA ################

# load data and identify adjacency
geom <- forsys::test_forest |> 
  mutate(priority5 = range01(sqrt(priority1 + priority2 + priority4))) |>
  mutate(boundary3 = range01(boundary1 * (mosaic1 + priority4 * 3))) |>
  mutate(revenue = ((priority2 + priority4 - cluster1) * 1000) + 2400)

test_forest <- forsys::test_forest |> 
  rename(id = stand_id, group = proj_id, ha = area_ha) |>
  rename_with(~gsub("priority","p",.x), contains('priority')) |>
  rename_with(~gsub("threshold","t",.x), contains('threshold')) |>
  rename_with(~gsub("cluster","c",.x), contains('cluster')) |>
  rename_with(~gsub("boundary","b",.x), contains('boundary')) |>
  rename_with(~gsub("mosaic","m",.x), contains('mosaic')) |>
  mutate(col = rep(c(1:100), 100)) |>
  mutate(row = floor(id / 100) + 1) |>
  relocate(id, group, ha, col, row)

geom$X <- rep(c(1:100), 100)
geom$Y <- floor(geom$stand_id / 100) + 1
geom <- geom |> filter(X <= 23 & Y <= 23) |> dplyr::select(-X, -Y)

# build adjacency network
adj_net <- calc_adj_network_func(geom, St_id = geom$stand_id, calc_dist = TRUE)
igraph::vertex_attr(adj_net) <- bind_cols(vertex_attr(adj_net), select(geom, matches('priority|threshold'), area_ha))
igraph::vertex_attr(adj_net, name = 'objective') <- igraph::vertex_attr(adj_net, 'priority5')
igraph::vertex_attr(adj_net, name = 'area') <- igraph::vertex_attr(adj_net, 'area_ha')
igraph::vertex_attr(adj_net, name = 'constraint') <- igraph::vertex_attr(adj_net, 'priority2')
