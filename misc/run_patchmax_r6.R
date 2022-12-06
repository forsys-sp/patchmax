# load required packages
pacman::p_load(dplyr, sf, ggplot2, R6, assertive,
               igraph, cppRouting, proxy,
               furrr, animation, glue)

# source patchmax, patchmax functions, and example data
source('misc/patchmax_r6.R')
source('misc/patchmax_r6_func.R')
source('misc/patchmax_r6_data.R')

# set number of session to run in parallel
plan(multisession, workers = 6)

# create new patchmax generator with stand geometry and required field names
patchmax <- patchmax_generator$new(geom, 'stand_id', 'priority1', 'area_ha', 2000)

# setting optional parameters
patchmax$sdw = 5
patchmax$availability = 'threshold2 == 1'
patchmax$patch_area = 10000

# example of building a project at center of study area 
patchmax$build(5050)
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
patchmax$patch_area = 20000
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

