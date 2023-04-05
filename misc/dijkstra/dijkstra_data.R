# sample edge list
make_test_graph <- function(){
  edgelist_test <- data.frame(
    from=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
    to=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
    dist=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9),
    aux=rep(10,18))
  
  g <- graph_from_edgelist(edgelist_test[,1:2] |> as.matrix())
  g2 <- as.undirected(g)
  E(g2)$weight = c(7,9,14,7,3,15,5,10,11)
  
  set.seed(1)
  plot(g2, edge.label = E(g2)$weight, vertex.size=30)
  
  edgelist_test2 <- as_edgelist(g2) |> 
    as.data.frame() |> 
    rename(from = 1, to = 2) |> 
    mutate(dist = c(7,9,14,7,3,15,5,10,11)) |>
    mutate(aux = rep(10,9)) |>
    arrange(from, to)
  return(edgelist_test2)
}

test_forest_400 <- function(){
  range01 <- function(x){
    (x-min(x))/(max(x)-min(x))
  }
  
  shp <- patchmax::test_forest %>% 
    filter(row > 25, row <= 45, col > 25, col <= 45) %>%
    mutate(cost = ((p2 + p4 - c1) * 1000) + 3000) |>
    mutate(p5 = range01(p1 + p2 + p4 + cost))
  return(shp)
}
