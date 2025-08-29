library(patchmax)
library(dplyr)
library(sf)
# library(future)
# library(ggplot2)
# library(tidyr)
# library(data.table)

# load stand geometry
shp <- patchmax::test_forest %>% 
  # filter(row > 23, row <= 42, col > 25, col <= 42) %>%
  # filter(row > 10, row <= 20, col > 10, col <= 20) %>%
  filter(m1 == 3) |>
  mutate(cost = ((p2 + p4 - c1) * 1000) + 3000) %>%
  mutate(p5 = p4 * (1 - p3)) %>%
  mutate(p5 = p5 * p5)

# display stand attributes
shp %>%
  select(matches('p[0-9]|t[0-9]|b[0-9]|m[0-9]|c[0-9]|cost')) %>%
  plot(max.plot = 20, border=NA)

bw.colors <- function(n){
  grey.colors(n, start = 0, end = 1, rev = T)
}
plot(shp[,'p5'], nbreaks = 10, pal = bw.colors)

plot(shp$geometry)

net <- create_adj_network(shp, 'id')
plot(net, vertex.label = NA, vertex.size = 1, edge.arrow.size = 0)

el <- net |> igraph::as_edgelist()
net <- create_adj_network(shp, 'id', adj_edgelist = el)
plot(net, vertex.label = NA, vertex.size = 1, edge.arrow.size = 0)

# create new patchmax object
pm <- patchmax$new(
  geom = shp,
  id_field = 'id', 
  objective_field = 'p5', 
  area_field = 'ha', 
  threshold = 'c3 == 1',
  area_min = 200,
  area_max = 10000)

# create new patchmax object w pre-generated network
pm <- patchmax$new(
  geom = shp,
  id_field = 'id', 
  objective_field = 'p5', 
  area_field = 'ha', 
  threshold = 'c3 == 1',
  area_min = 200,
  area_max = 10000, 
  adj_method = 'buffer')

# create new patchmax object w pre-generated network
pm <- patchmax$new(
  geom = shp,
  id_field = 'id', 
  objective_field = 'p5', 
  area_field = 'ha', 
  threshold = 'c3 == 1',
  area_min = 200,
  area_max = 10000, 
  adj_network = net)

pm$reset()$plot()
pm$search()$build()$record()$plot()
pm$reset()$search()$build()$record(write=F)$plot()
pm$params <- list(sdw = 0)

pm$net
plot(pm$net, vertex.label = NA)
pm$patch_stands$id
igraph::delete_vertices(pm$net, pm$patch_stands$id) |> plot()

# plot priority
pm$plot()
pm$build(1311)
pm$search(print_errors = T)

# search, build, record, and plot best patch
pm$search()$build()$record()$plot()

pm$build(1014)
pm$available_stands

pm$reset()$simulate(11)$plot()

# plot search results
pm$reset()$search(T)

pm$search(T)$record()

# add additional cost constraint
pm$params = list(
  constraint_field = 'cost',
  constraint_max = 20000)

# re-plot search results
pm$reset()$search(T)

# search score clearly lowered in areas with high cost
pm$plot('cost')

# let's further limit constraints by increasing the minimum project area
pm$params = list(area_min = 800)

# re-plot search results (note grey areas where valid patch is impossible)
pm$reset()$search(T)

# build, record, and plot
pm$build()$record()

# compare patch objective vs cost
pm$plot('p4')
pm$plot('cost')

# let's speed up search by adding multiple processes
plan(multisession, workers = 8)

# continue simulating additional 9 projects
pm$simulate(9)

# compare patch objective vs cost
pm$plot('p4')
pm$plot('cost')

# show stats
pm$patch_stats
plot(pm$patch_stats$objective)

# add threshold constraint
pm$reset()
pm$threshold = 'm1 == 3'

# let's add an availability constraint based 
pm$plot('m1')

# simulate 10 projects
pm$simulate(10)
pm$plot()

# show stats
pm$patch_stats
plot(pm$patch_stats$objective)

# compare objectives trends among scenarios
pm_a <- patchmax$new(shp, 'id', 'p4', 'ha', 1000)
pm_b <- patchmax$new(shp, 'id', 'p4', 'ha', 1000, constraint_field = 'cost', constraint_max = 20000)
pm_c <- patchmax$new(shp, 'id', 'p4', 'ha', 1000, constraint_field = 'cost', constraint_max = 20000, threshold = 'm1 != 3')

# simulate 10 patches each
pm_a$reset()$simulate(10)
pm_b$reset()$simulate(10)
pm_c$reset()$simulate(10)

pdat <- data.frame(rank = 1:10,
           a = pm_a$patch_stats$objective,
           b = pm_b$patch_stats$objective,
           c = pm_c$patch_stats$objective) %>%
  pivot_longer(-rank) 

pdat %>% ggplot(aes(x = rank, y = value, color = name)) + geom_line()
