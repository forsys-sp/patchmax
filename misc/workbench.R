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

# add patch cost constraint of 10000
pm$params <- list(constraint_field = 'cost', constraint_max = 10000)
pm$search(1)$build()$plot('cost')

# add threshold for inclusion
pm$params <- list(threshold = 'm1 == 2')
pm$search(1)$build()$plot('cost')

# find top 10 patches in study area
pm$simulate(10)
pm$plot()
pm$plot(plot_field = 'm1')

# fire break example
shp2 <- shp %>% filter(b1 == 1)
pm2 <- patchmax_generator$new(shp2, 'id', 'p4', 'ha', 1000)
pm2$simulate(10)
pm2$plot()