library(patchmax)
library(dplyr)
library(sf)
# library(future)
library(ggplot2)
# library(tidyr)
# library(data.table)

arc_ident <- function(layer_a, layer_b){
  int_a_b <- st_intersection(layer_a, layer_b)
  rest_of_a <- st_difference(layer_a, st_union(layer_b))
  output <- bind_rows(int_a_b, rest_of_a)
  return(st_as_sf(output))
}

# load stand geometry
shp <- patchmax::test_forest %>% 
  # filter(m1 == 3, t2 == 1) |>
  filter(row > 35, row <= 45, col > 25, col <= 45) %>%
  mutate(cost = ((p2 + p4 - c1) * 1000) + 3000) %>%
  mutate(p5 = p4 * (1 - p3)) %>%
  mutate(p5 = p5 * p5)

plot(shp[,'p4'])

hex <- st_make_grid(shp, n = c(10,20), square = F) |> 
  # st_buffer(-300) |>
  # st_buffer(250) |>
  st_as_sf() |>
  mutate(hex_id = 1:n())

  
out <- arc_ident(shp, hex) 
out$area <- st_area(out)
out_2 <- out |> st_drop_geometry() |> group_by(hex_id) |> 
  arrange(-area) |>
  slice_head(n = 1) |>
  arrange(id)

hex2 <- hex |> left_join(out_2)
plot(hex2, max.plot=20)

pm <- patchmax$new(
  geom = hex2,
  id_field = 'hex_id', 
  objective_field = 'p4',
  # threshold = 't2 == 1',
  area_field = 'ha', 
  sdw = 1,
  area_max = 1000)

pm$simulate(10)
pm$plot()

tmp <- pm$geom |> #filter(m1 > 1) |>
  select(id = ..patch_id) |> filter(id >= 1) 
tmp2 <- tmp |> st_buffer(-500) |> st_buffer(475)

nf <- st_read('/Users/codyevers/Library/CloudStorage/Dropbox/!gis/shp_misc/CONUS_NationalForests_rp.shp') |> st_transform(st_crs(tmp))

tmp2[nf,] |> plot(border=NA)

ggplot() + geom_sf(data = tmp2, aes(fill = factor(id)), color=NA) + 
  theme_void() + theme(legend.position = 'none')


ggsave('test.svg')
  
