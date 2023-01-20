library(patchmax)
library(dplyr)
library(sf)
library(future)
library(ggplot2)
library(tidyr)

# load stand geometry
shp <- patchmax::test_forest %>% 
  filter(row > 25, row <= 45, col > 25, col <= 45) %>%
  mutate(cost = ((p2 + p4 - c1) * 1000) + 3000)

# display stand attributes
shp %>%
  select(matches('p[0-9]|t[0-9]|b[0-9]|m[0-9]|c[0-9]|cost')) %>%
  plot(max.plot = 20, border=NA)

# create new patchmax object
pm <- patchmax$new(
  geom = shp, 
  id_field = 'id', 
  objective_field = 'p4', 
  area_field = 'ha', 
  area_max = 1000, 
  area_min = 500)

# plot priority
pm$plot()

# search, build, record, and plot best patch
pm$search()$build()$record()$plot()

# plot search results
pm$reset()$search(T)

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
