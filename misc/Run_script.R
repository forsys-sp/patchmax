#The package was built in R 4.0.3, update the new version to install the package.

#Install those packages previously to Patchmax:
#igraph, data.table, sf, doParallel, parallel

library(dplyr)
library(sf)
library(data.table)
library(Patchmax)

#Read shapefile to access the stand field values
Shape = read_sf("N:/PatchMax/Input/Shapefile2.shp",layer="Shapefile2")

#Method 1
#Generating igraph object from shapefile
Adj_object <- calculate_adj(Shapefile = "N:/PatchMax/Input/Shapefile2.shp", Adjdist = 3, St_id = Shape$Stand_ID)

#Method 2
#Generating igraph object from adjacency table (numbers in the adjacency table need to match the Stand_ID vector)
#Adj_object <- read_adj(read.csv("N:/PatchMax/Input/Adjacency_table.csv"))


start_time <- Sys.time()
#Simulating the projects

generate_outputs <- simulate_projects(
                            St_id = Shape$Stand_ID,
                            St_adj = Adj_object,
                            St_area = Shape$Acres,
                            St_objective = Shape$Mortal_BA,
                            P_size = 1000,
                            P_size_slack = 0.05,
                            P_number = 10,
                            #St_threshold = Shape$NetValue,
                            #St_threshold_value = 0,
                            P_constraint = Shape$NetValue,
                            P_constraint_max_value = Inf,
                            P_constraint_min_value = 521610,
                            Candidate_min_size = 40
                            )




end_time <- Sys.time()
time_spent <- end_time - start_time

#project level outputs
generate_outputs[[1]]

#stand level outputs
generate_outputs[[2]]$DoTreat

write.csv(generate_outputs[[1]], file = "N:/Patchmax/Output/ExProject.csv")
write.csv(generate_outputs[[2]], file = "N:/Patchmax/Output/ExStands.csv")

