pacman::p_load(cppRouting, igraph, proxy, sf, dplyr, ggplot2, 
               animation, glue, purrr, furrr, R6, assertive)

# R6 objects: private = network, public = initialize, search, build, apply
# private: objective, constraint, area, network
# scenario$search()$build()$apply()

source('misc/cppRouting_func.R')
source('misc/cppRouting_r6.R')
source('misc/cppRouting_data.R')
source('misc/cppRouting_func_aux.R')

plan(multisession, workers = 6)

geom$stand_id <- paste0(geom$stand_id,'a')

pm <- patchmax_generator$new(
  geom = geom, 
  id_field = 'stand_id', 
  obj_field = 'priority1', 
  area_field = 'area_ha', 
  proj_area = 10000)

# run sequence of class methods
pm$search()
pm$select()
pm$plot()

# chaining example
pm$search()$select()$plot()

# update parameters
pm$obj_field <- 'priority4'
pm$proj_area <- 50000

# rerun search and build
pm$search()$select()$plot()

pm$plot()
