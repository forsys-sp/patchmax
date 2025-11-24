# load packages
pacman::p_load(dplyr, sf, future, ggplot2, tidyr, patchmax, animation, glue)

# load stand geometry
shp <- patchmax::test_forest |> 
  filter(row > 30, row <= 70, col > 30, col <= 70) |>
  mutate(cost = ((p2 + p4 - c1) * 1000) + 3000)

# display stand attributes
shp |>
  select(matches('p[0-9]|t[0-9]|b[0-9]|m[0-9]|c[0-9]|cost')) |>
  plot(max.plot = 20, border=NA)

# create new patchmax object
pm <- patchmax$new(
  geom = shp, 
  id_field = 'id', 
  objective_field = 'p4', 
  area_field = 'ha', 
  area_max = 10000)

pm$id_field <- 'id'

pm$reset()$build('5050')$record('X')$plot(return_plot = T) 

bounce <- function(x){
  return(c(x, rev(x)))
}

# variable size
ani.options(loop = 0, ani.res=100, interval=0.1)
saveVideo({
  sq <- seq(1000,50000,1000)
  for (i in bounce(sq)) {
    pm$area_max <- i
    p <- pm$reset()$build('5050')$record('X')$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('Area max {i}'))
    print(p)
  }
}, video.name = 'misc/animations/variable_size.mp4')


# variable SDW
ani.options(loop = 0, ani.res=100, interval=0.1)
saveVideo({
  pm$params <- list(area_max = 25000)
  sq <- seq(-1, 1, length.out = 25) |> round(2)
  for (i in bounce(sq)) {
    pm$sdw <- i
    p <- pm$reset()$build('5050')$record('X')$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('SDW {i}'))
    print(p)
  }
}, video.name = 'misc/animations/variable_sdw.mp4')

# variable EDW
ani.options(loop = 0, ani.res=100, interval=0.1)
saveVideo({
  pm$params <- list(area_max = 25000, sdw = 0.5, threshold = 'c3 == 1')
  sq <- seq(-1, 1, length.out = 50) |> round(2)
  for (i in bounce(sq)) {
    pm$epw <- i
    p <- pm$reset()$build('5050')$record('X')$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('EPW {i}; Excluded {pm$patch_stats$excluded}%'))
    print(p)
  }
}, video.name = 'misc/animations/variable_epw.mp4')

# variable priorities variable ratio
ani.options(loop = 0, ani.res=100, interval=0.1)
saveVideo({
  
  func <- function(x, s=3) 0.5+log10(x/(1-x))/s
  sq <- func(seq(0.05,.95,.02))
  
  for (i in bounce(sq)) {
    shp$p14 = shp$p1 * i + shp$p4 * (1 - i)
    pm <- patchmax$new(shp, 'id', 'p14', 'ha', 40000, sdw = 1)
    p <- pm$reset()$build('5050')$record('X')$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('Priority 1 weight {round(i,2)}'))
    print(p)
  }
}, video.name = 'misc/animations/variable_priorities_alt.mp4')


# sequential patch building demo (SDW = 1)

ani.options(loop = 0, ani.res=100, interval=1)
saveVideo({

  i = 0.5
  shp$p14 = shp$p1 * i + shp$p4 * (1 - i)
  pm <- patchmax$new(shp, 'id', 'p14', 'ha', 10000, sdw = 1)
  pm$random_sample(1)
  
  for (i in 1:10) {
    plan(multisession, workers=8)
    p <- pm$search()$build()$record()$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('Patch {i} SDW 1.0'))
    print(p)
  }
}, video.name = 'misc/animations/build_10_patches_sdw1.mp4')


# sequential patch building demo (SDW = 0.25)

ani.options(loop = 0, ani.res=100, interval=1)
saveVideo({
  
  i = 0.5
  shp$p14 = shp$p1 * i + shp$p4 * (1 - i)
  pm <- patchmax$new(shp, 'id', 'p14', 'ha', 10000, sdw = 0.25)
  pm$random_sample(1)
  
  for (i in 1:10) {
    plan(multisession, workers=8)
    p <- pm$search()$build()$record()$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('Patch {i} SDW 0.25'))
    print(p)
  }
}, video.name = 'misc/animations/build_10_patches_sdw25.mp4')


# typewriter search animation demo

pm <- patchmax$new(
  geom = shp, 
  id_field = 'id', 
  objective_field = 'p4', 
  area_field = 'ha', 
  area_max = 25000,
  sdw = .1)

ani.options(loop = 0, ani.res=100, interval=0.1, ani.width = 1000, ani.height = 600)
saveVideo({
  
  pm$reset()$search()
  geom_template <- pm$geom |> left_join(pm$search_results) |> rename(objective = search)
  
  sq <- pm$geom |> 
    filter(row_number() %% 10 == 1) |>
    pull(id)
  
  for (i in sq) {
    
    pm$reset()$build(i)$record('X')
    p <- pm$reset()$build(i)$record('X')$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('Search area'))
    g2 <- geom_template |> 
      mutate(objective = ifelse(as.numeric(geom_template$id) > as.numeric(i), NA, objective))
    p2 <- ggplot() + 
      geom_sf(data = g2, aes(fill = objective), linewidth=0) + 
      scale_fill_viridis_c(limits = c(115, 170)) + 
      geom_sf(data = g2 |> filter(!is.na(objective)) |> slice_tail(n = 1), fill = 'black',) +
      geom_sf(data = g2 |> filter(objective == max(objective, na.rm=T)), fill = 'red',) +
      theme_void() + theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('Objective score'))
    print(cowplot::plot_grid(p, p2))
  }
}, video.name = 'misc/animations/search_demo_sdw_01.mp4')


# typewriter animation w/ threshold and secondary constraint

pm <- patchmax$new(
  geom = shp, 
  id_field = 'id', 
  objective_field = 'p4', 
  area_field = 'ha', 
  area_max = 10000)

pm$params <- list(
  sdw = .1, 
  threshold = 'c3 == 1', 
  epw = .5, 
  exclusion_limit = 0.1,
  area_min = 7500,
  constraint_field = 'cost',
  constraint_max = 200000
)

pm$random_sample(.25)
pm$search(verbose = T, plot = T)
pm$reset()
pm$simulate(7, verbose = T)
pm$random_sample(1)
pm$simulate(5)
pm$reset()$search()
pm$plot()
pm$reset()$search()
pm$reset()$search(verbose = T)
pm$reset()$search(plot = T, verbose = F)
pm$reset()$search(plot = T, verbose = T)
pm$record()$plot()

ani.options(loop = 0, ani.res=100, interval=0.1, ani.width = 1000, ani.height = 600)
saveVideo({
  
  pm$reset()$search()
  
  geom_template <- pm$geom |> 
    left_join(pm$search_results) |> 
    left_join(pm$search_errors) |> 
    rename(objective = search)

  sq <- pm$geom |> 
    filter(row_number() %% 10 == 1) |>
    pull(id)
  
  i <- sq[100]
  for (i in sq) {
    
    pm$reset()$build(i)$record('X')
    p <- pm$reset()$build(i)$record('X')$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('Search area'))
    g2 <- geom_template |> 
      mutate(objective = ifelse(as.numeric(geom_template$id) > as.numeric(i), NA, objective)) |>
      mutate(error = ifelse(as.numeric(geom_template$id) > as.numeric(i), NA, error))
    p2 <- ggplot() + 
      geom_sf(data = g2, aes(fill = objective), linewidth=0) + 
      scale_fill_viridis_c() + 
      geom_sf(data = g2 |> filter(grepl('threshold', error)), fill = 'purple') +
      geom_sf(data = g2 |> filter(grepl('constraint', error)), fill = 'hotpink') +
      geom_sf(data = g2 |> filter(!is.na(objective)) |> slice_tail(n = 1), fill = 'black',) +
      geom_sf(data = g2 |> filter(objective == max(objective, na.rm=T)), fill = 'red',) +
      theme_void() + theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('Objective score'))
    
    print(cowplot::plot_grid(p, p2))
  }
}, video.name = 'misc/animations/search_demo_errors.mp4')

pm$plot('p4')
pm$plot('cost')
pm$plot('c3')



