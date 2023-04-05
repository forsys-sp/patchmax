
source('misc/dijstra_data.R')
dict <- read.csv('misc/dijstra_test_400nodes_dictionary.csv') |> rename(id = 1, vertex =2)
shp <- test_forest_400()
dim(shp)


animation::saveGIF({
  for(i in nodes[1:10]){
    print(i)
    out <- dijstra_cpp(edgelist_tf, nodes, i, 99)
    out |> arrange(dist) |> head()
    shp2 <- shp |> left_join(out |> left_join(dict, by='vertex'), by='id')
    plot(shp2[,'dist'], border=NA)
  }
}, interval=0.05)

i = 50
microbenchmark(
  out <- dijstra_cpp(edgelist_tf, nodes, i, 10000),
  out_b <- dijkstra_r(edgelist_tf, i, max_aux = 10000, stop_type = 2),
  times = 25
)

# break occurs when the distance to start exceeds max distance. unfortunately, there are still quick a few nodes that are within that distance which are never fully evaluated because they have yet to be visited. 

# time to calc all 10k: 30 seconds
# time to calc w/ stop 1: 0.5 sec
# time to calc w/ stop 2: 5 sec
#
# @ 1k distance limit: 0.5 vs 4 sec
# @ 2.5k distance limit: 1.5 vs 5 sec
# @ 5k distance limit: 5 vs 9 sec
# @ 10k distance limit: 16 vs 20 sec

limit = 10
mb <- microbenchmark(
  # x1 <- dijkstra_r(edgelist_tf, 10, Inf),
  x2 <- dijkstra_r(edgelist_tf, 10, limit/0.8, stop_type = 1),
  x3 <- dijkstra_r(edgelist_tf, 10, limit, stop_type = 2),
  times = 1
)
mb

x1 <-  dijkstra_r(edgelist_tf, 10, Inf)
x2 <- dijkstra_r(edgelist_tf, 10, limit, stop_type = 1)
x2 <- dijkstra_r(edgelist_tf, 10, limit, stop_type = 3)
x <- x1 |> left_join(x2 |> select(vertex, aux_alt = aux), by='vertex')
col <- ifelse(x$aux_alt == Inf, 'uncounted','counted')
col[x$aux_alt == Inf & x$aux < limit] <- 'error'
table(col) |> prop.table()

min_x <- x |> filter(x$aux_alt == Inf) |> pull(aux) |> min()
min_x/limit

ggplot(x, aes(x=flag, y=aux, color=col)) + 
  geom_point(size=1) + 
  geom_hline(yintercept = c(min_x, limit), lty=c(3,2)) +
  labs(x = 'Search order', y = 'Search distance')
