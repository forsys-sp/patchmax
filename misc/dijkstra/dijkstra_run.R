pacman::p_load(tidyverse, Rcpp, microbenchmark, cppRouting)

Rcpp::sourceCpp('misc/dijkstra/dijkstra_func.cpp')
source('misc/dijkstra/dijkstra_func.R')

# test 1: simple 6 node network

edgelist_test <- data.frame(
  from=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
  to=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
  dist=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9),
  aux=rep(10,18))

limit = 999999
speed_test <- microbenchmark(
  'cpp' = dijkstra_cpp(edgelist_test, 1, limit),
  'r' = dijkstra_r(edgelist_test, 1, limit) 
)
speed_test
plot(speed_test)

# test 2: 400 node network from test forest

edgelist_tf <- read.csv('misc/dijkstra/dijkstra_test_400nodes_edgelist.csv') |> 
  mutate(dist = dist/1000, aux = 100)

out <- dijkstra_cpp(edgelist_tf, 1, 5000) |> arrange(dist)
out <- dijkstra_r(edgelist_tf, 1, 5000) |> arrange(dist)

load('misc/dijkstra/dijkstra_cpp_test_data_400.Rdata')

limit = 5000
speed_test <- microbenchmark(
  'cpp' = dijkstra_cpp(edgelist_tf, 1, limit),
  'r' = dijkstra_r(edgelist_tf, 1, limit),
  'cppRouting' = get_distance_matrix(edge_dat, 2526, edge_dat$dict$ref)
)
speed_test
plot(speed_test)
