pacman::p_load(dplyr, sf, ggplot2, R6, assertive,
               igraph, cppRouting, proxy,
               furrr, animation, glue)

source('misc/patchmax_r6.R')
source('misc/patchmax_r6_func.R')
source('misc/patchmax_r6_data.R')

# set number of session to run in parallel
plan(multisession, workers = 6)

# create new patchmax generator with stand geometry and required field names
patchmax <- patchmax_generator$new(geom, 'stand_id', 'priority1', 'area_ha', 2000); 

# set secondary constraints
patchmax$sdw = 5
# patchmax$availability = 'threshold2 == 1'
patchmax$patch_area = 10000

patchmax$reset()
patchmax$build(1010)$plot()
patchmax$simulate(n_projects = 10)
patchmax$describe()
patchmax$plot()
patchmax$build(1010)$plot()

pdat <- patchmax$describe() |> 
  dplyr::select(matches('patch_id|priority')) |> 
  tidyr::pivot_longer(-1) |>
  filter(patch_id != 0) |>
  arrange(name) |>
  group_by(name) |>
  mutate(cs = cumsum(value))

pdat |>
  ggplot(aes(x=patch_id, y=value, color=name)) + 
  geom_line(linetype=2) +
  geom_smooth(se = FALSE) +
  labs(title = 'Sequential patch value by priority', x = 'Patch ID', y = 'Patch value')


patchmax$search(sample_frac = 1, plot_search = T)
patchmax$build(1010)$plot()

patchmax <- patchmax_generator$new(geom, 'stand_id')
patchmax$objective_field = 'priority4'
patchmax$patch_area = 5000
patchmax$sdw = 1; 
patchmax$constraint_field = 'priority4'; 
patchmax$constraint_min = 0; 
patchmax$constraint_max = 50
patchmax$availability = 'threshold2 == 1'
patchmax$availability_area_adjust = 0
patchmax$availability_objective_adjust = 0
patchmax$availability = NULL

patchmax$availability_objective_adjust
patchmax$patch

patchmax$search(sample_frac = 1)
patchmax$search(sample_frac = 1, plot_search = T)
patchmax$build()$plot()
patchmax$build('1536a')


# SEARCH PLOT ACROSS WEIGHTING GRADIENT BETWEEN TWO PRIORITIES
animation::saveVideo({
  ani.options(interval =  0.1)

  func <- function(x, s=3) 0.5+log10(x/(1-x))/s
  sq2 = plogis(-5:5)
  sq2 = range01(func(seq(0.05,.95,.05)))
  
  q = 0.5
  for(q in c(sq2,rev(sq2))){
    geom$priority_12 = geom$priority1 * q + geom$priority4 * (1 - q)
    patchmax <- patchmax_generator$new(geom, 'stand_id', 'priority_12', 'area_ha', 2500); patchmax$sdw = 1
    patchmax$search(sample_frac = 1, return_all = T, plot_search = T)
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
    patchmax <- patchmax_generator$new(geom, 'stand_id', 'priority_12', 'area_ha', 2500); patchmax$sdw = 1
    patchmax$search(sample_frac = 1, return_all = T)
    
    for(i in 1:10){
      patchmax$search(sample_frac = 1)$build()$record()
    }
    
    patch_geom <- patchmax$geom |> filter(patch_id != 0) |> group_by(patch_id) |> summarize()
    p_out <- ggplot() + 
      geom_sf(data=patchmax$geom, linewidth=0, aes(fill=priority_12)) +
      # geom_sf(data=patchmax$geom, aes(fill=search_score)) + 
      geom_sf(data=patch_geom, linewidth=2, fill=NA, color='white') +
      geom_sf_label(data=patch_geom, aes(label=patch_id)) +
      scale_fill_gradientn(colors=sf.colors(10)) +
      theme_void() +
      theme(legend.position = 'none') 
    print(p_out)
    # plot(patchmax$geom[,'priority_12'])
    # plot(patchmax$geom[,'patch_id'], add=T)
  }
}, ani.width = 960, ani.height = 480)

# SEARCH PLOT ACROSS SDW GRADIENT BETWEEN 1 AND 10
animation::saveVideo({
  ani.options(interval =  0.1)
  for(s in c(1:10,9:2)){
    # plot(geom[,'priority_12'])
    print(s)
    patchmax <- patchmax_generator$new(geom, 'stand_id', 'priority4', 'area_ha', 2500); patchmax$sdw = s
    patchmax$search(sample_frac = 1, return_all = T, plot_search = T)
  }
})

patchmax <- patchmax_generator$new(geom, 'stand_id', 'priority_12', 'area_ha', 2500); patchmax$sdw = 1
for(i in 1:10){
  patchmax$search(sample_frac = 0.25)$build()$record()
}
plot(patchmax$geom[,'patch_id'])

patchmax$search(sample_frac = 1, return_all = T, plot_search = T)$build()$record()

patchmax$plot()
patchmax$selected
patchmax$sdw = 1; patchmax$patch_area = 25000; patchmax$objective_field = 'priority1'
patchmax$search(sample_frac = 1, show_progress = T)$build()$plot()$record()
record <- patchmax$describe()
plot(record$priority1)

patchmax$record(1)
patchmax$plot()

# for(i in 1:2){
  i = 1
  set.seed(12345)
  patchmax$search(.01)$select()$plot()
  patchmax$apply_project(i)
  i = i + 1
# }

patches <- patchmax$geom |> group_by(patch_id) |> summarize() |> filter(patch_id != 0)
plot(patches)

patchmax$sdw = 1
patchmax$select('5050a')
patchmax$plot()

# chaining example
patchmax$search()$select()$record()$substract()
patchmax$search()$select()$modify()

# update parameters
patchmax$obj_field <- 'priority4'
patchmax$proj_area <- 50000

# rerun search and build
patchmax$sdw <- 5
patchmax$search()$select()$plot()
patchmax$plot()
