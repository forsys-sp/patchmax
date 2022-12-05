pacman::p_load(cppRouting, igraph, proxy, 
               sf, dplyr, ggplot2, 
               animation, glue, purrr, 
               furrr, R6, assertive)

source('misc/patchmax_r6.R')
source('misc/patchmax_r6_func.R')
source('misc/patchmax_r6_data.R')
plan(multisession, workers = 6)
geom$stand_id <- paste0(geom$stand_id,'a')


pm <- patchmax_generator$new(geom, 'stand_id', 'priority1', 'area_ha', 5000)
pm$sdw = 1; pm$constraint_field = 'priority4'; pm$constraint_min = 0; pm$constraint_max = 50
pm$availability = NULL
pm$availability = 'threshold2 == 1'
pm$grow('1536a')
pm$patch
pm$plot(plot_field = 'threshold2')
pm$search()
pm$grow('1536a')


# SEARCH PLOT ACROSS WEIGHTING GRADIENT BETWEEN TWO PRIORITIES
animation::saveVideo({
  ani.options(interval =  0.1)

  func <- function(x, s=3) 0.5+log10(x/(1-x))/s
  sq2 = plogis(-5:5)
  sq2 = range01(func(seq(0.05,.95,.05)))
  
  for(q in c(sq2,rev(sq2))){
    geom$priority_12 = geom$priority1 * q + geom$priority4 * (1 - q)
    pm <- patchmax_generator$new(geom, 'stand_id', 'priority_12', 'area_ha', 2500); pm$sdw = 1
    pm$search(sample_frac = 1, return_all = T, plot_search = T)
  }
})

# SEARCH PLOT ACROSS WEIGHTING GRADIENT BETWEEN TWO PRIORITIES
animation::saveVideo({
  ani.options(interval =  0.1)
  
  func <- function(x, s=3) 0.5+log10(x/(1-x))/s
  sq2 = plogis(-5:5)
  sq2 = range01(func(seq(0.05,.95,.01)))
  
  for(q in c(sq2,rev(sq2))){
    print(q)
    geom$priority_12 = geom$priority4 * q + geom$priority5 * (1 - q)
    pm <- patchmax_generator$new(geom, 'stand_id', 'priority_12', 'area_ha', 2500); pm$sdw = 1
    pm$search(sample_frac = 1, return_all = T)
    
    for(i in 1:10){
      pm$search(sample_frac = 1)$grow()$record()
    }
    
    patch_geom <- pm$geom %>% filter(patch_id != 0) %>% group_by(patch_id) %>% summarize()
    p_out <- ggplot() + 
      geom_sf(data=pm$geom, linewidth=0, aes(fill=priority_12)) +
      # geom_sf(data=pm$geom, aes(fill=search_score)) + 
      geom_sf(data=patch_geom, linewidth=2, fill=NA, color='white') +
      geom_sf_label(data=patch_geom, aes(label=patch_id)) +
      scale_fill_gradientn(colors=sf.colors(10)) +
      theme_void() +
      theme(legend.position = 'none') 
    print(p_out)
    # plot(pm$geom[,'priority_12'])
    # plot(pm$geom[,'patch_id'], add=T)
  }
}, ani.width = 960, ani.height = 480)

# SEARCH PLOT ACROSS SDW GRADIENT BETWEEN 1 AND 10
animation::saveVideo({
  ani.options(interval =  0.1)
  for(s in c(1:10,9:2)){
    # plot(geom[,'priority_12'])
    print(s)
    pm <- patchmax_generator$new(geom, 'stand_id', 'priority4', 'area_ha', 2500); pm$sdw = s
    pm$search(sample_frac = 1, return_all = T, plot_search = T)
  }
})

pm <- patchmax_generator$new(geom, 'stand_id', 'priority_12', 'area_ha', 2500); pm$sdw = 1
for(i in 1:10){
  pm$search(sample_frac = 0.25)$grow()$record()
}
plot(pm$geom[,'patch_id'])

pm$search(sample_frac = 1, return_all = T, plot_search = T)$grow()$record()

pm$plot()
pm$selected
pm$sdw = 1; pm$patch_area = 25000; pm$objective_field = 'priority1'
pm$search(sample_frac = 1, show_progress = T)$grow()$plot()$record()
record <- pm$describe()
plot(record$priority1)

pm$record(1)
pm$plot()

# for(i in 1:2){
  i = 1
  set.seed(12345)
  pm$search(.01)$select()$plot()
  pm$apply_project(i)
  i = i + 1
# }

patches <- pm$geom %>% group_by(patch_id) %>% summarize() %>% filter(patch_id != 0)
plot(patches)

pm$sdw = 1
pm$select('5050a')
pm$plot()

# chaining example
pm$search()$select()$record()$substract()
pm$search()$select()$modify()

# update parameters
pm$obj_field <- 'priority4'
pm$proj_area <- 50000

# rerun search and build
pm$sdw <- 5
pm$search()$select()$plot()
pm$plot()
