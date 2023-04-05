pacman::p_load(patchmax, sf, dplyr, future)

shp <- st_read('~/Downloads/SERAL_sample/SERAL_sample.shp')
shp$Am4RevBio_ac <- shp$Am4RevBio / shp$Acres
shp$Am4RevBio_ac_ac <- shp$Am4RevBio_ac / shp$Acres

sum(shp$Acres)

pm <- patchmax$new(
  geom = shp, id_field = 'LMU_ID',
  objective_field = 'Am4RevBio',
  area_field = 'Acres',
  area_max = 10000, 
  # area_min = 9000,
  sdw=1, 
  epw=1, 
  # threshold = 'Am4RevBio > 1000',
  constraint_field = 'TotConVol')

plan(multisession, workers=8)
pm$search()$build()$record()
pm$reset()

pm$build('56662')
pm$patch_stands
pm$simulate(3)
pm$search(1)$build()$record()
pm$search(1)$build()$record()
pm$plot()
pm$summarize()
pm$patch_stats
pm$summarize(sum_vars = c('Am4VolSaw','TotConVol'))


pm$patch_stats
pm$patch_stands
pm$constraint_field = 'Acres'
pm$exclusion_limit = .75
pm$build('55104')
pm$record() 

plot(pm$patch_stats$objective, type='l')
plot(pm$patch_stats$objective, pm$patch_stats$excluded)

