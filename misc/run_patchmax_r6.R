# load required packages
pacman::p_load(dplyr, sf, ggplot2, R6, assertive,
               igraph, cppRouting, proxy, 
               furrr, animation, glue, purrr)

remotes::install_github("xoopR/param6")


# source patchmax, patchmax functions, and example data
source('misc/patchmax_r6.R')
source('misc/patchmax_r6_func.R')
source('misc/patchmax_r6_data.R')

# set number of session to run in parallel
plan(multisession, workers = 8)

# geom <- geom %>% filter(boundary3 != 0)

# create new patchmax generator with stand geometry and required field names
patchmax <- patchmax_generator$new(geom, 'stand_id', 'priority1', 'area_ha', 25000)
patchmax$constraint_field = 'priority2'; patchmax$constraint_min = 1; patchmax$constraint_max = 40
patchmax$params
patchmax$params <- list(sdw = 4, epw = 10, patch_area = 50000)
patchmax$params <- list(constraint_field = NULL, objective = 'test')

patchmax$search(plot_search = T)
patchmax$sdw = 10
patchmax$area_slack = 0.25
patchmax$build(2063)
patchmax$patch
patchmax$plot()
patchmax$record()
patchmax$search(sample_frac = 1, show_progress = T, plot_search = T)
patchmax$search()$build()
patchmax$plot()
patchmax$describe()

# setting optional parameters
patchmax$sdw = 0
patchmax$availability = 'threshold2 == 1'
patchmax$availability = NULL
patchmax$constraint_field = 'priority1'
patchmax$constraint_max = 100

patchmax$patch_area = 10000

# example of building a project at center of study area 
patchmax$build(2363)
patchmax$plot()

# example of searching study area for best patch seed
patchmax$search()
patchmax$build()
patchmax$plot()

# example of chaining together commands
patchmax$search()$build()$plot()

# plotting search results (used to select best project)
patchmax$search(sample_frac = 1, show_progress = T, plot_search = TRUE)

# notice difference (including processing time) when sampling from stands
patchmax$search(sample_frac = 0.25, show_progress = T, plot_search = TRUE)
patchmax$search(sample_frac = 0.1, show_progress = T, plot_search = TRUE)

# example building three patches in sequence
patchmax$patch_area = 10000
patchmax$search()$build()$record() # patch 1
patchmax$search()$build()$record() # patch 2
patchmax$search()$build()$record() # patch 3
patchmax$plot()

# report statistics for recorded patches
patchmax$describe()

# reset recorded patches
patchmax$reset()

# simulate multiple patches in sequence
patchmax$patch_area = 5000
patchmax$simulate(n_projects = 50)
patchmax$describe()
patchmax$plot()

# plot patch project values across all priorities
pdat <- patchmax$describe() %>% 
  dplyr::select(matches('patch_id|priority')) %>% 
  tidyr::pivot_longer(-1) %>%
  filter(patch_id != 0) %>%
  arrange(name) %>%
  group_by(name) %>%
  mutate(cs = cumsum(value))

pdat %>%
  ggplot(aes(x=patch_id, y=value, color=name)) + 
  geom_point(size=0.5) + 
  # geom_line(linetype=1, alpha=0.5) +
  geom_smooth(se = TRUE, aes(fill=name), alpha=0.25) +
  facet_wrap(~name) +
  labs(title = 'Sequential patch value by priority', x = 'Patch ID', y = 'Patch value')

plot(geom[,c('priority1','priority2','priority3','priority4','priority5')], border=NA)





######################################### STOP ####################





patchmax$patch_area = 20000
patchmax$simulate(n_projects = 50)
patchmax$describe()



func <- function(x, s=3) 0.5+log10(x/(1-x))/s
sq2 = range01(func(seq(0.05,.95,.1)))

q = 0.5
p_out <- c(sq2,rev(sq2)) %>% purrr::map(function(q){
  print(q)
  geom$priority_12 = geom$priority1 * q + geom$priority2 * (1 - q)
  patchmax <- patchmax_generator$new(geom, 'stand_id', 'priority_12', 'area_ha', 20000)
  patchmax$simulate(n_projects = 20)
  patch_out <- patchmax$describe()
  stand_out <- patchmax$geom %>% 
    dplyr::select(stand_id, priority_12, patch_id) %>%
    filter(patch_id != 0)
  return(list(patch_out, stand_out))
})

# save(p_out, file = 'multi_run_tradeoff_v2.Rdata')

stand_dat <- p_out %>% purrr::map(2)

saveVideo({
  for(i in 1:20){
    ani.options(interval = 1)
    patch_pdat <- stand_dat[[i]] %>% 
      group_by(patch_id) %>% 
      summarize() 
    
    plot_out <- ggplot(data = patch_pdat) + geom_sf(aes(fill = patch_id)) + 
      scale_fill_gradientn(colors = rev(sf.colors(10))) +
      theme(legend.position = 'bottom') +
      geom_sf_text(aes(label=patch_id)) +
      theme_void() + guides(fill = 'none')
    
    scale = ggplot() + 
      geom_line(data=data.frame(x=c(1,10),y=c(1,1)), aes(x,y)) +
      geom_point(aes(x=c(1:10,10:1)[i], y=1), size=3) + theme_void() +
      theme(axis.text.x = element_text()) + 
      scale_x_continuous(breaks=c(1,5.5,10), labels=c('P2','Split','P1'))
    
    out <- cowplot::plot_grid(plot_out, scale, nrow = 2, rel_heights = c(10,1))
    
    print(out)
  }
}, ani.width = 960, ani.height = 960, ani.res = 200)


patch_data <- p_out %>% purrr::map(1)

curve_data <- 1:20 %>% purrr::map_dfr(function(x){
  out <- patch_data[[x]] %>% 
    filter(patch_id != 0) %>%
    select(patch_id, priority1, priority2)
  out$x = x
  return(out)
})

saveVideo({
  for(i in 1:20){
    ani.options(interval = 1)

    curve_plot <- ggplot(
      curve_data, aes(x=priority1, y=priority2, color=patch_id, group=patch_id)) +
      geom_point(alpha=0.5) +
      scale_color_gradientn(colors = rev(sf.colors(10))) +
      geom_point(size=3, alpha=0.5) +
      geom_point(data=curve_data %>% filter(x == i), color = 'black', size=3) +
      geom_text(data=curve_data %>% filter(x == i), aes(label=patch_id), size=2, color='white') +
      cowplot::theme_cowplot() +
      theme(legend.position = 'none', 
            axis.line = element_blank(), 
            axis.ticks = element_blank(),
            axis.text = element_blank()) +
      labs(x = 'P1', y = 'P2')
    
    patch_pdat <- stand_dat[[i]] %>% 
      group_by(patch_id) %>% 
      summarize() 
    
    patch_out <- ggplot(data = patch_pdat) + geom_sf(aes(fill = patch_id)) + 
      scale_fill_gradientn(colors = rev(sf.colors(10))) +
      theme(legend.position = 'bottom') +
      geom_sf_text(aes(label=patch_id)) +
      theme_void() + guides(fill = 'none')
      
    scale = ggplot() + 
      geom_line(data=data.frame(x=c(1,10),y=c(1,1)), aes(x,y)) +
      geom_point(aes(x=c(1:10,10:1)[i], y=1), size=3) + theme_void() +
      theme(axis.text.x = element_text()) + 
      scale_x_continuous(breaks=c(1,5.5,10), labels=c('P2','Split','P1'))
    
    out <- cowplot::plot_grid(curve_plot, patch_out, scale, scale, nrow = 2, rel_heights = c(10,1))
  
    print(out)
  }
}, ani.width = 1920, ani.height = 960, ani.res = 200)
