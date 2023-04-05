library(patchmax)
library(dplyr)
library(sf)
library(future)
library(ggplot2)
library(tidyr)
library(data.table)

shp <- st_read('~/Downloads/El_Dorado_Poly/El_Dorado_Poly.shp')
tmp <- shp |> st_drop_geometry() |> group_by(PODid) |> summarize(n = n())
View(tmp)
# PODid 44 == 84714 stands
# PODid 19 == 15673 stands
# PODid 38 == 25000 stands
shp_2 <- shp |> filter(PODid == 38)

pm <- patchmax$new(
  geom = shp_2, 
  id_field = 'CELL_ID', 
  objective_field = 'EC_Targ_50', 
  area_field = 'Acres', 
  area_max = 1000)

pm$search()
