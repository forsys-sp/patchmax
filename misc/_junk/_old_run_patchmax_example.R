#The package was built in R 4.0.3, update the new version to install the package.

#Install those packages previously to Patchmax:
#igraph, data.table, sf, doParallel, parallel

library(Patchmax)
library(dplyr)
library(sf)
library(data.table)
library(Patchmax)
library(ggplot2)
library(reshape2)
library(forsys)
library(RANN)
library(proxy)
library(cppRouting)

data("test_forest")
stands <- test_forest |> st_drop_geometry() 

kn = 1000

# extract polygon centroids
xy <- st_centroid(test_forest) |> st_coordinates() |> as.data.frame()

adj = Patchmax::calculate_adj(
  Shapefile = test_forest, 
  St_id = test_forest$stand_id,
  calc_dst = TRUE
  )
V(adj)$name <- as_ids(V(adj))

# build nearest neighbor spatial index
# dist_kdtree <- nn2(data = xy, query = xy, k = kn)
# dist = Patchmax::calculate_dist(test_forest)

geom <- test_forest

# load('~/GitHub/forsys-data/test_adj.Rdata')
args <- list()
args$id <- geom$stand_id
args$adj <- adj
args$dist <- TRUE
args$area <- geom$area_ha
args$objective <- geom$priority4 # objective
args$constraint <- geom$priority3
args$threshold <- geom$priority1 # threshold
args$threshold_val <- 0.5
args$SDW <- 3
args$candidate_min_size <- NULL

patchmax_sample_n = 100

if(!is.null(patchmax_sample_n))
  args$st_seed <- sample(geom$stand_id, patchmax_sample_n, F)

generate_outputs <- simulate_projects(
  # St_seed = args$st_seed,
  St_id = args$id, # stand id vector
  St_adj = args$adj, # igraph adjacency network
  St_area = args$area, # stand area vector
  St_objective = args$objective, # vector of stand values to maximize 
  P_size = 100000, # project size target
  P_size_slack = 0.01, # project size slack (%)
  # P_size_ceiling = 90000,
  P_number = 3, # project count
  # St_threshold = args$threshold, # vector containing values applied to threshold
  # St_threshold_value = args$threshold_val, # minimum threshold
  # P_constraint = args$constraint,
  # P_constraint_max_value = 100,
  # P_constraint_min_value = 50,
  Candidate_min_size = args$candidate_min_size,
  St_distance = args$dist,
  SDW = args$SDW 
)

generate_outputs[[1]]
generate_outputs[[2]]

geom$Project <- generate_outputs[[2]]$Project[match(geom$stand_id, generate_outputs[[2]]$Stands)]
plot(geom[,'Project'], border=rgb(0,0,0,.1))
geom$DoTreat <- generate_outputs[[2]]$DoTreat[match(geom$stand_id, generate_outputs[[2]]$Stands)]
plot(geom[,'DoTreat'], border=rgb(0,0,0,.1))


# Example number 2 --------------

geom = read_sf("data/stf_forest.geojson")

adj_object <- calculate_adj(Shapefile = geom, St_id = geom$stand_id, method = 'nb') # FAILS
adj_object <- calculate_adj(Shapefile = geom, Adjdist = 3, St_id = geom$stand_id, method = 'buffer') # 8 minutes
# save(adj_object, file='~/GitHub/forsys-data/stf_adj.Rdata')

load('~/GitHub/forsys-data/stf_adj.Rdata')
args <- list()
args$id <- geom$cell_id
args$adj <- adj_object
args$area <- geom$AREA_HA
args$objective <- geom$HUIDW_SPM
args$constraint <- geom$AREA_HA
args$threshold <- geom$TVSUM_STND
args$threshold_val <- 150
args$p_number <- 4
args$p_size <- 25000

generate_outputs <- simulate_projects(
  St_id = args$id, # stand id vector
  St_adj = args$adj, # igraph adjacency network
  St_area = args$area, # stand area vector
  St_objective = args$objective, # vector of stand values to maximize 
  P_size = args$p_size, # project size target
  P_size_slack = 0.01, # project size slack (%)
  # P_size_ceiling = 90000,
  P_number = args$p_number, # project count
  St_threshold = args$threshold, # vector containing values applied to threshold
  St_threshold_value = args$threshold_val, # minimum threshold
  # P_constraint = args$constraint,
  # P_constraint_max_value = 10000,
  # P_constraint_min_value = 1000,
  # Candidate_min_size = 40, 
  Sample_n = 1000,
  Sample_seed = 123
  )

generate_outputs[[1]] |> dplyr::mutate(Density = Objective/TotalArea * 1000)
generate_outputs[[2]]

geom$Project <- generate_outputs[[2]]$Project[match(geom$cell_id, generate_outputs[[2]]$Stands)]
plot(geom[,'Project'], border=rgb(0,0,0,.1))
geom$DoTreat <- generate_outputs[[2]]$DoTreat[match(geom$cell_id, generate_outputs[[2]]$Stands)]
plot(geom[,'DoTreat'], border=rgb(0,0,0,.1))


# summarize projects
generate_outputs[[1]]
generate_outputs[[2]] |> group_by(Project) |> summarize(Area = sum(Area))
proj_summary <- generate_outputs[[2]] |> 
  group_by(Project, DoTreat) |> 
  summarize(Area = sum(Area)) |> 
  tidyr::pivot_wider(id_cols = Project, names_from = DoTreat, values_from = Area) |>
  dplyr::rename(NoTrt = `0`, Trt = `1`) |>
  tidyr::replace_na(list(NoTrt = 0, Trt = 0)) |>
  mutate(PctTrt = round(Trt/(Trt+NoTrt) * 100))
proj_summary
generate_outputs[[2]] |> filter(DoTreat == 0) |> left_join(geom, by=c('Stands'='stand_id'))

# map projects
col <- colorRampPalette(colors=rev(c('steelblue1','yellow','orange','red','purple')))
geom2 <- geom |> left_join(generate_outputs[[2]] |> dplyr::rename(stand_id = Stands))
geom3 <- geom2 |> group_by(Project) |> summarize_if(is.numeric, sum)
geom3 |> left_join(proj_summary) |>
  ggplot() + geom_sf(aes(fill=Project)) + geom_sf_label(aes(label=Project))

geom3 |> st_write('~/Dropbox/!!projects/aa_10yr_uncertainity/fire_trt_interaction/projects.shp')



# Graph showing linear increase in processing time between stands and node sampling size
plot(c(72, 35, 18, 11, 5) ~ c(22000, 10000, 5000, 2500, 1000), type='b',
     xlab='Stand count', ylab='Time per project (sec)')

# save(tmp, file='misc/temp_results.Rdata')

tmp |> purrr::map_dfr(function(x){
  data.frame(Objective = x[[1]]$Objective |> sum())
})

tmp_out <- tmp |> purrr::map_dfr(function(x){
  data.frame(Stand = x[[2]]$Stands)
}) |> dplyr::group_by(Stand) |> summarize(n = n()) |>
  arrange(-n) |> rename(cell_id = Stand)

geom |> left_join(tmp_out) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = n), color=NA) +
  ggplot2::scale_fill_viridis_c() +
  cowplot::theme_cowplot()

tmp_out <- tmp |> purrr::map_dfr(function(x){
  st <- x[[2]] |> dplyr::select(cell_id = Stands, Project)
  geom |> inner_join(st) |> group_by(Project) |> summarize()
})

tmp_out |> ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=Project), color=rgb(0,0,0,0.1), alpha=0.1)





par(mfrow=c(3,2))
pout <- tmp |> purrr::map(function(x){
  geom2 <- geom |> left_join(x[[2]] |> dplyr::rename(cell_id = Stands))
  ggplot2::ggplot(geom2) +
    ggplot2::geom_sf(ggplot2::aes(fill = Project), color=NA) +
    ggplot2::scale_fill_viridis_c(direction = -1) + 
    cowplot::theme_nothing()
})

ggplot2::ggsave('misc/test_plot.png', plot = cowplot::plot_grid(plotlist = pout))
gridExtra::grid.arrange(gList =- pout)



generate_outputs[[1]] #project level outputs
generate_outputs[[2]] #stand level outputs
generate_outputs[[1]]$Objective |> sum()

geom2 <- geom |> left_join(generate_outputs[[2]] |> dplyr::rename(cell_id = Stands))
col <- colorRampPalette(colors=rev(c('steelblue1','yellow','orange','red')))(10)
plot(geom2[,'Project'], border=rgb(0,0,0,.1), pal = col)

write.csv(generate_outputs[[1]], file = "N:/Patchmax/Output/ExProject.csv")
write.csv(generate_outputs[[2]], file = "N:/Patchmax/Output/ExStands.csv")





stands <- st_read('~/GitHub/forsys-data/YSS_LandMgtUnits_v16_20210924/YSS_LandMgtUnits_v16_20210924.shp') 
adj = Patchmax::calculate_adj(Shapefile = stands, St_id = stands$LMU_ID, method = 'buffer', Adjdist = 1)
