library(patchmax)
library(dplyr)
library(sf)

# load stand geometry
shp <- patchmax::test_forest %>% 
  filter(row > 20, row <= 40, col > 20, col <= 40)

# display stand attributes
shp %>%
  select(matches('p[0-9]|t[0-9]|b[0-9]|m[0-9]|c[0-9]')) %>%
  plot(max.plot = 20, border=NA)

# create new cost field by combining existing fields
shp <- shp %>% mutate(cost = ((p2 + p4 - c1) * 1000) + 3000)
plot(shp[,'cost'], border=NA)

# create new patchmax object
pm <- patchmax_generator$new(
  geom = shp, 
  id_field = 'id', 
  objective_field = 'p4', 
  area_field = 'ha', 
  area_max = 1000)

# search for best patch
pm$search()$build()$plot()

# diagnostic plot showing how search results
pm$search(sample_frac = 1, plot_search = T)

# add patch cost constraint of 10000 
pm$params <- list(constraint_field = 'cost', constraint_max = 15000)

# notice how the selected patch changes due to cost constraints
pm$search(1)$build()
pm$plot(plot_field = 'p4')
pm$plot(plot_field = 'cost')

# add minimum patch area as additional constraint
pm$params <- list(area_min = 800)

# notice in search plot how valid patches are impossible in most of the study area
pm$search(1, plot_search = T)

# add threshold for inclusion limits potential patches even further
pm$params <- list(threshold = 'm1 == 3')
pm$search(1, plot_search = T)

# plot best patch between 1000 and 800 acres with a cost less than 10000 in areas where m1 == 3
pm$search(1)$build()
pm$plot(plot_field = 'm1')

# find and record sequence of patches in order of quality
pm$search(1)$build()$record()
pm$search(1)$build()$record()
pm$search(1)$build()$record()
pm$plot('m1')
pm$plot()

# repeat using simulate with strong exclusion penalty
pm$reset()
pm$epw = 1
pm$simulate(3, 1)
pm$plot('m1')
pm$plot()

# summarize recorded patches
pm$describe()

# find top 10 patches in study area
pm$simulate(10)
pm$plot()
pm$plot(plot_field = 'm1')

# fire break example
shp2 <- shp %>% filter(b1 == 1)
pm2 <- patchmax_generator$new(shp2, 'id', 'p4', 'ha', 1000)
pm2$simulate(10, 1)
pm2$plot()
pm2$search(1, plot_search = T)
pm2$build()


future::plan(future::multisession, workers = 8)
tictoc::tic()
pm$reset()
pm$simulate(20, 1)
tictoc::toc()
pm$plot()

# 1 = 34 sec; 4 = 15 sec