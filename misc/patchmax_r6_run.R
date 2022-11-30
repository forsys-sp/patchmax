pacman::p_load(cppRouting, igraph, proxy, sf, dplyr, ggplot2, 
               animation, glue, purrr, furrr, R6, assertive)

# R6 objects: private = network, public = initialize, search, build, apply
# private: objective, constraint, area, network
# scenario$search()$build()$apply()

source('misc/patchmax_r6_func.R')
source('misc/patchmax_r6_base.R')
source('misc/patchmax_r6_data.R')
source('misc/patchmax_r6_aux.R')

plan(multisession, workers = 6)

geom$stand_id <- paste0(geom$stand_id,'a')

pm <- geom %>% patchmax_generator$new(id_field = 'stand_id', area_field = 'area_ha')

# run sequence of class methods
pm$proj_area = 25000
pm$obj_field = 'priority5'
pm$sdw = 5
pm$search(.1)
pm$select()
pm$plot()

# chaining example
pm$search()$select()$plot()

# update parameters
pm$obj_field <- 'priority4'
pm$proj_area <- 50000

# rerun search and build
pm$sdw <- 5
pm$search()$select()$plot()
pm$plot()
