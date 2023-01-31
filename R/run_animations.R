library(dplyr)
library(sf)
library(future)
library(ggplot2)
library(tidyr)
library(patchmax)
library(animation)
library(glue)

# load stand geometry
shp <- patchmax::test_forest %>% 
  filter(row > 30, row <= 70, col > 30, col <= 70) %>%
  mutate(cost = ((p2 + p4 - c1) * 1000) + 3000)

# shp <- patchmax::test_forest

# display stand attributes
shp %>%
  select(matches('p[0-9]|t[0-9]|b[0-9]|m[0-9]|c[0-9]|cost')) %>%
  plot(max.plot = 20, border=NA)

# create new patchmax object
pm <- patchmax$new(
  geom = shp, 
  id_field = 'id', 
  objective_field = 'p4', 
  area_field = 'ha', 
  area_max = 10000)

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
}, video.name = 'variable_size.mp4')


# variable SDW
ani.options(loop = 0, ani.res=100, interval=0.1)
saveVideo({
  pm$params <- list(area_max = 25000)
  sq <- seq(0,1,0.02)
  for (i in bounce(sq)) {
    pm$sdw <- i
    p <- pm$reset()$build('5050')$record('X')$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('SDW {i}'))
    print(p)
  }
}, video.name = 'variable_sdw.mp4')

# variable EDW
ani.options(loop = 0, ani.res=100, interval=0.1)
saveVideo({
  pm$params <- list(area_max = 25000, sdw = 0.5, threshold = 'c3 == 1')
  sq <- seq(0,1,0.02)
  for (i in bounce(sq)) {
    pm$epw <- i
    p <- pm$reset()$build('5050')$record('X')$plot(return_plot = T) + 
      theme(legend.position = 'none', plot.title = element_text(hjust=0.5)) +
      ggtitle(glue('EPW {i}; Excluded {pm$patch_stats$excluded}%'))
    print(p)
  }
}, video.name = 'variable_epw.mp4')

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
}, video.name = 'variable_priorities_alt.mp4')


# project building (SDW = 1)
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
}, video.name = 'build_10_patches_sdw1.mp4')

# project building (SDW = 0.25)
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
}, video.name = 'build_10_patches_sdw25.mp4')





