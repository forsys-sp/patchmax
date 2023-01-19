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
pm$search(1)$build()$record()$plot()

# plot search results
pm$reset()$search(1, T)

# add additional cost constraint
pm$params = list(
  constraint_field = 'cost',
  constraint_max = 20000)

# re-plot search results
pm$reset()$search(1, T)

# search score clearly lowered in areas with high cost
pm$plot('cost')

# let's further limit constraints by increasing the minimum project area
pm$params = list(area_min = 800)

# re-plot search results (note grey areas where valid patch is impossible)
pm$reset()$search(1, T)

# build, record, and plot
pm$build()$record()

# compare patch objective vs cost
pm$plot('p4')
pm$plot('cost')

# let's speed up search by adding multiple processes
plan(multisession, workers = 8)

# continue simulating additional 9 projects
pm$simulate(19,1)

# compare patch objective vs cost
pm$plot('p4')
pm$plot('cost')

# add threshold constraint 
pm$reset()
pm$threshold = 'm1 != 3'

# let's add an availability constraint based 
pm$plot('m1')

# simulate 10 projects
pm$simulate(10, 1)
pm$plot()

# compare objectives trends among scenarios
pm_a <- patchmax$new(shp, 'id', 'p4', 'ha', 1000)
pm_b <- patchmax$new(shp, 'id', 'p4', 'ha', 1000, constraint_field = 'cost', constraint_max = 20000)
pm_c <- patchmax$new(shp, 'id', 'p4', 'ha', 1000, constraint_field = 'cost', constraint_max = 20000, threshold = 'm1 != 3')

# simulate 10 patches each
pm_a$reset()$simulate(10,1)
pm_b$reset()$simulate(10,1)
pm_c$reset()$simulate(10,1)

data.frame(rank = 1:10,
           a = pm_a$patch_stats$objective,
           b = pm_b$patch_stats$objective,
           c = pm_c$patch_stats$objective) %>%
  pivot_longer(-rank) %>%
  ggplot(aes(x = rank, y = value, color = name)) + geom_line()



# END NEW AAAAAA


# search for best patch
pm$search()$build()$record()$plot()

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
pm2 <- patchmax$new(shp2, 'id', 'p4', 'ha', 1000)
pm2$simulate(10, 1)
pm2$plot()
pm2$search(1, plot_search = T)
pm2$build()


tictoc::tic()
pm <- patchmax$new(shp, 'id', 'cost', 'ha', 1000)
pm$params <- list(constraint_field = 'p4', constraint_min = 5, constraint_max = 10)
future::plan(future::multisession, workers = 8)
pm$reset()
# pm$search(1, show_progress = T, plot_search = T)
# pm$build()
# pm$search(show_progress = T)$build()$record()
pm$simulate(n_projects = 10, sample_frac = .1)
# pm$search(show_progress = T, plot_search = T)
pm$plot()
pm$patch_stats
pm$patch_stats$objective %>% plot()
tictoc::toc()

tictoc::tic()
pm$reset()
pm$simulate(20, 1)
tictoc::toc()
pm$plot()

# 1 = 34 sec; 4 = 15 sec