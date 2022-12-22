library(patchmax)
library(dplyr)
library(sf)
library(future)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Test with new data...  <<<<<<<<<<<<<<<<<<<<
shp <- st_read('~/Downloads/Hexnet_Stanislaus/Hexnet_Stanislaus.shp')
shp_s <- shp %>% filter(POINT_X > -2090000, POINT_X < -2060000) %>% filter(POINT_Y > 1930000, POINT_Y < 1960000)
plot(shp[,'man_alldis'], border=F)

pm <- patchmax$new(shp_s, 'CELL_ID', 'HUSUM_PCP', 'Acres', 10000)
pm$params # view existing patchmax parameters
pm$params <- list(threshold = 'OwnerCat == "USFS"')
plan(multisession(workers=6)) # setup multiple sessions in parallel to increase search speed
pm$search(.1, show_progress = T)$build()$record()$plot() # search, build, record, and plot single patch
pm$simulate(20) # create 20 additional patches
pm$plot() # plot 
pm$recorded_patch_stats # patch stats

# isolate and plot treated stands
pdat <- shp_s %>% 
  mutate(CELL_ID = as.character(CELL_ID)) %>% 
  inner_join(pm$recorded_patch_stands %>% filter(include == 1), by=c('CELL_ID'='node'))
plot(pdat[,'HUSUM_PCP.x'])
plot(pdat[,'patch_id'])
plot(pdat[,'OwnerCat'])
  
pm$reset()
# End test with new data... <<<<<<<<<<<<<<<<<<<<
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# load stand geometry
shp <- patchmax::test_forest %>% 
  filter(row > 25, row <= 35, col > 25, col <= 35)

# display stand attributes
shp %>%
  select(matches('p[0-9]|t[0-9]|b[0-9]|m[0-9]|c[0-9]')) %>%
  plot(max.plot = 20, border=NA)

# create new cost field by combining existing fields
shp <- shp %>% mutate(cost = ((p2 + p4 - c1) * 1000) + 3000)
plot(shp[,'cost'], border=NA)

# create new patchmax object
pm <- patchmax$new(
  geom = shp, 
  id_field = 'id', 
  objective_field = 'p4', 
  area_field = 'ha', 
  area_max = 1000)

pm$simulate(12,1)
pm$search(1, T)
 pm$plot()

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