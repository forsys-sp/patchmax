pacman::p_load(cppRouting, igraph, proxy, sf, dplyr, ggplot2, 
               animation, glue, furrr)

source('misc/rppRouting_func.R')

############ PREP DATA ################

# load data and identify adjacency
geom <- forsys::test_forest %>%
  mutate(priority5 = sqrt(priority1 * priority2 * priority4) %>% range01)
plot(geom[,'priority5'])

adj <- Patchmax::calculate_adj(geom, St_id = geom$stand_id, calc_dst = TRUE)
data <- geom %>% st_drop_geometry() %>% as.data.frame()
vertex_attr(adj) <- bind_cols(vertex_attr(adj), select(data, contains('priority')))

############ CIRCLE SEARCH EXAMPLE ################

# define key parameters
r = 1010
area_field = 'area_ha'
psize <- 25000
sdw = 4 
pfield <- 'priority2'

circle_vertices <- circleVertices(50, 50, radius = 30, npoints = 300) %>% 
  round() %>% apply(1, paste, collapse = '') %>% as.integer()

ani.options(interval = 0.05)

saveVideo(expr = {
  for(i in circle_vertices){
    print(i)
    cpp_graph <- build_graph(adj, pfield, sdw)
    cpp_nn <- build_project(cpp_graph, v = i, proj_area = psize)
    obj <- sum(V(adj)$priority1[as.integer(cpp_nn$node)]) %>% round()
    p1 <- plot_project(geom, adj, cpp_nn, pfield, plot = F) + theme_void() + 
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
adj2 <- adj
proj_ids <- NULL

ani.options(interval = 1)

# START PROJECT BUILDING CYCLE
saveVideo(expr = {
  for(i in 1:25){
    print(i)
    cpp_graph <- build_graph(adj2, pfield, sdw)
    search <- search_best(cpp_graph, net = adj2, obj_field = pfield, psize = psize, sample_frac = 0.1)
    i_best <- V(adj2)$name[which(V(adj2)$name == names(which.max(search)))]
    cpp_nn <- build_project(cpp_graph, v = i_best, proj_area = psize)
    # plot_project(geom, adj2, cpp_nn, pfield, search, plot=T)
    
    proj_ids <- bind_rows(proj_ids, 
      geom %>% filter(stand_id %in% cpp_nn$node) %>% summarize() %>% mutate(id = i))
    
    p <- plot_project(geom, adj2, cpp_nn, pfield, plot=F, pname = i)
    p <- p + geom_sf(data = proj_ids, fill=NA, linewidth=0.5, color='black') + geom_sf_text(data = proj_ids, aes(label = id))
    print(p)
    
    adj2 <- delete_vertices(adj2, cpp_nn$node)
  }
})
tictoc::toc()


##### TEST 




plan(multisession, workers = 6)

sdw = 1
psize = 10000
pfield = 'priority1'

adj2 <- adj
proj_ids <- NULL

# START PROJECT BUILDING CYCLE
jout <- seq(.1,1,by=.1) %>% purrr::map(function(j){
  adj2 <- adj
  proj_ids <- NULL
  for(i in 1:10){
    print(i)
    cpp_graph <- build_graph(adj2, pfield, sdw)
    search <- search_best(cpp_graph, net = adj2, obj_field = pfield, psize = psize, sample_frac = j)
    i_best <- V(adj2)$name[which(V(adj2)$name == names(which.max(search)))]
    cpp_nn <- build_project(cpp_graph, v = i_best, proj_area = psize)
    proj_ids <- bind_rows(proj_ids, geom %>% filter(stand_id %in% cpp_nn$node) %>% summarize(obj = sum(get(pfield))) %>% mutate(id = i))
    adj2 <- delete_vertices(adj2, cpp_nn$node)
  }
  return(proj_ids)
})

i = 0
jout %>% purrr::map_df(function(x){
  # browser()
  i = i + 1
  x <- x %>% st_drop_geometry()
  x$frac <- seq(.1,1,by=.1)[i]
  return(x)
}) -> jout2

boxplot(jout2$obj ~ jout2$id)
plot(jout2$id, jout2$obj)


plot(jout[[1]]$obj, type='l', col='red')
points(jout[[10]]$obj, add=T, type='l')
data.frame(jout)
