#The package was built in R 4.0.3, update the new version to install the package.

#Install those packages previously to Patchmax:
#igraph, data.table, sf, doParallel, parallel

library(dplyr)
library(sf)
library(data.table)
library(Patchmax)

#Read shapefile to access the stand field values
geom = read_sf("~/GitHub/forsys-data/shasta.geojson") %>% st_make_valid()
adj_object <- calculate_adj(Shapefile = geom, Adjdist = 3, St_id = Shape$cell_id)
# save(adj_object, file = '~/GitHub/forsys-data/test_adj.Rdata')


#Method 2
#Generating igraph object from adjacency table (numbers in the adjacency table need to match the Stand_ID vector)
#Adj_object <- read_adj(read.csv("N:/PatchMax/Input/Adjacency_table.csv"))

#Simulating the projects

geom = read_sf("~/GitHub/forsys-data/test_forest.geojson") %>% st_make_valid()
load('~/GitHub/forsys-data/test_adj.Rdata')
args <- list()
args$id <- geom$cell_id
args$adj <- adj_object
args$area <- geom$area_ha
args$objective <- geom$priority2

geom = read_sf("~/GitHub/forsys-data/shasta.geojson") %>% st_make_valid()
load('~/GitHub/forsys-data/stf_adj.Rdata')
args <- list()
args$id <- geom$cell_id
args$adj <- adj_object
args$area <- geom$AREA_HA
args$objective <- geom$HUIDW_SPM
args$threshold <- geom$TVSUM_STND
args$constraint <- geom$AREA_HA

x <- 1000
sample_sizes <- rep(1000, 10) 
tmp <- sample_sizes %>% 
  purrr::map(function(x){
    print(x)
    if(x == Inf) x = NULL
    generate_outputs <- simulate_projects(
      St_id = args$id,
      St_adj = args$adj,
      St_area = args$area,
      St_objective = args$objective,
      P_size = 10000,
      P_size_slack = 0.05,
      P_number = 3,
      # St_threshold = args$threshold,
      # St_threshold_value = 150,
      # P_constraint = args$constraint,
      # P_constraint_max_value = 10000,
      # P_constraint_min_value = 1000,
      Candidate_min_size = 40,
      Sample_n = x,
      Sample_seed = 123
    )
  })

generate_outputs[[1]]
col <- colorRampPalette(colors=rev(c('steelblue1','yellow','orange','red')))
geom2 <- geom %>% left_join(generate_outputs[[2]] %>% dplyr::rename(cell_id = Stands))
plot(geom2[,'Project'], border=rgb(0,0,0,.1), pal = col(nrow(generate_outputs[[1]])))

plot(c(72, 35, 18, 11, 5) ~ c(22000, 10000, 5000, 2500, 1000), type='b',
     xlab='Stand count', ylab='Time per project (sec)')

# save(tmp, file='misc/temp_results.Rdata')

tmp %>% purrr::map_dfr(function(x){
  data.frame(Objective = x[[1]]$Objective %>% sum())
})

tmp_out <- tmp %>% purrr::map_dfr(function(x){
  data.frame(Stand = x[[2]]$Stands)
}) %>% dplyr::group_by(Stand) %>% summarize(n = n()) %>%
  arrange(-n) %>% rename(cell_id = Stand)

geom %>% left_join(tmp_out) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = n), color=NA) +
  ggplot2::scale_fill_viridis_c() +
  cowplot::theme_cowplot()

tmp_out <- tmp %>% purrr::map_dfr(function(x){
  st <- x[[2]] %>% dplyr::select(cell_id = Stands, Project)
  geom %>% inner_join(st) %>% group_by(Project) %>% summarize()
})

tmp_out %>% ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=Project), color=rgb(0,0,0,0.1), alpha=0.1)





par(mfrow=c(3,2))
pout <- tmp %>% purrr::map(function(x){
  geom2 <- geom %>% left_join(x[[2]] %>% dplyr::rename(cell_id = Stands))
  ggplot2::ggplot(geom2) +
    ggplot2::geom_sf(ggplot2::aes(fill = Project), color=NA) +
    ggplot2::scale_fill_viridis_c(direction = -1) + 
    cowplot::theme_nothing()
})

ggplot2::ggsave('misc/test_plot.png', plot = cowplot::plot_grid(plotlist = pout))
gridExtra::grid.arrange(gList =- pout)



generate_outputs[[1]] #project level outputs
generate_outputs[[2]] #stand level outputs
generate_outputs[[1]]$Objective %>% sum()

geom2 <- geom %>% left_join(generate_outputs[[2]] %>% dplyr::rename(cell_id = Stands))
col <- colorRampPalette(colors=rev(c('steelblue1','yellow','orange','red')))(10)
plot(geom2[,'Project'], border=rgb(0,0,0,.1), pal = col)

write.csv(generate_outputs[[1]], file = "N:/Patchmax/Output/ExProject.csv")
write.csv(generate_outputs[[2]], file = "N:/Patchmax/Output/ExStands.csv")



