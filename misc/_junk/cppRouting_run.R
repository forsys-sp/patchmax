pacman::p_load(cppRouting, igraph, proxy, sf, dplyr, ggplot2, 
               animation, glue, purrr, furrr, R6, assertive)

# R6 objects: private = network, public = initialize, search, build, apply
# private: objective, constraint, area, network
# scenario$search()$build()$apply()

source('misc/patchmax_oop_func.R')
source('misc/patchmax_oop_r6.R')
source('misc/patchmax_oop_data.R')
source('misc/patchmax_oop_func_aux.R')

plan(multisession, workers = 6)
geom$stand_id <- paste0(geom$stand_id,'a')
stands <- geom

# create new patch generator
patchmax <- stands %>% patchmax_generator$new(id_field = 'stand_id')

# set needed parameters
patchmax$obj_field = 'priority3'
patchmax$area_field = 'area_ha'
patchmax$proj_area = 10000

# search for best project
pm$search()

# select stands associated with best project
pm$select()

# plot best project
pm$plot()

# change the project size
pm$proj_area <- 25000

# rerun the using chaining
pm$search()$select()$plot




pm$obj_field = 'priority2'
pm$search()$select()$plot()

pm$obj_field <- 'priority5'
pm$sdw <- 1
pm$search()$select()$plot()

# run sequence of class methods
pm$search()
pm$select()
pm$plot()

# update parameters
pm$obj_field <- 'priority4'
pm$proj_area <- 50000

# chaining example
pm$search()$select()$plot()


############ ANIMATE THRESHOLD VALUES ################

adj_mod <- adj_net
V(adj_mod)$exclude <- 0
v_best <- "3580"

ani.options(interval = .2)

x <- c(seq(0.1,1,by=0.1), seq(1,20,by=1))
x <- c(x,rev(x))

saveVideo(expr = {
  for(i in x){
    print(i)
    adj_mod <- set_threshold_func(adj_net, V(adj_net)$threshold2 > 0.5, area_adj = 0, obj_adj = 0)
    cpp_graph <- build_graph_func(adj_mod, obj_field = ofield, sdw = 1, epw = i)
    p_best <- grow_project_func(cpp_graph, net = adj_mod, v = v_best, proj_area = psize)

    geom$exclude <- V(adj_mod)$exclude
    geom_sub <- geom %>% filter(stand_id %in% p_best$node)
    p <- plot_project(geom, adj_mod, p_best, 'priority5', plot = F) + labs(title=glue::glue('epw: {i}'))
    print(p)
  }
})

plan(multisession, workers = 6)
search <- search_best_func(cpp_graph, net = adj_mod, obj_field = ofield, proj_area = psize, sample_frac = 0.1)
v_best <- V(adj_mod)$name[which(V(adj_mod)$name == names(which.max(search)))]




data <- geom %>% st_drop_geometry() %>% as.data.frame()

############ CIRCLE SEARCH EXAMPLE ################

# define key parameters
r = 1010
area_field = 'area_ha'
psize <- 25000
sdw = 1 
pfield <- 'priority2'

adj_mod <- set_threshold(adj, V(adj)$threshold2 == 0, area_penality=0, obj_penality=0)

circle_vertices <- circleVertices(50, 50, radius = 30, npoints = 300) %>% 
  round() %>% apply(1, paste, collapse = '') %>% as.integer()
i <- circle_vertices[27]
ani.options(interval = 0.05)

saveVideo(expr = {
  for(i in circle_vertices){
    print(i)
    cpp_graph <- build_graph_func(adj_mod, pfield, sdw)
    cpp_nn <- grow_project_func(cpp_graph, adj_mod, v = i, proj_area = psize)
    obj <- sum(V(adj_mod)$priority1[as.integer(cpp_nn$node)]) %>% round()
    p1 <- plot_project(geom, adj_mod, cpp_nn, pfield, plot = F) + theme_void() + 
      theme(plot.title = element_text(hjust = 0.5, vjust = - 4), 
            plot.subtitle = element_text(hjust = 0.5, vjust = - 5))
    p1 <- p1 + labs(title = glue('Objective score: {obj}'), 
             subtitle = glue('Stand ID: {i} | Flexibility: {sdw} | Area: {psize}'))
    print(p1)
  }
}, video.name = 'circle_animation_priority2_sdw4_psize25k.mp4')


################ PROJECT BUILDING EXAMPLE ################

plan(multisession, workers = 6)

sdw = 1
psize = 10000
pfield = 'priority1'
adj_mod <- adj
proj_ids <- NULL

ani.options(interval = 1)

# START PROJECT BUILDING CYCLE
saveVideo(expr = {
  for(i in 1:25){
    print(i)
    cpp_graph <- build_graph_func(adj_mod, pfield, sdw)
    search <- search_best_func(cpp_graph, net = adj_mod, obj_field = pfield, psize = psize, sample_frac = 0.1)
    i_best <- V(adj_mod)$name[which(V(adj_mod)$name == names(which.max(search)))]
    cpp_nn <- grow_project_func(cpp_graph, v = i_best, proj_area = psize)
    # plot_project(geom, adj_mod, cpp_nn, pfield, search, plot=T)
    
    proj_ids <- bind_rows(proj_ids, 
      geom %>% filter(stand_id %in% cpp_nn$node) %>% summarize() %>% mutate(id = i))
    
    p <- plot_project(geom, adj_mod, cpp_nn, pfield, plot=F, pname = i)
    p <- p + geom_sf(data = proj_ids, fill=NA, linewidth=0.5, color='black') + geom_sf_text(data = proj_ids, aes(label = id))
    print(p)
    
    adj_mod <- delete_vertices(adj_mod, cpp_nn$node)
  }
})
tictoc::toc()


