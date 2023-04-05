build_func <- function(
    seed, 
    cpp_graph, 
    net, 
    a_max, 
    a_min=-Inf, 
    c_max=Inf, 
    c_min=-Inf, 
    c_enforce=TRUE
) {
  
  # calculate distance matrix using Dijkstra's algorithm
  dmat <- get_distance_matrix(cpp_graph, seed, cpp_graph$dict$ref)[1,]
  
  # sort nodes by distance
  i = match(cpp_graph$dict$ref, V(net)$name)
  dt <- data.table(
    node = names(dmat), 
    dist = dmat, 
    area = vertex_attr(net, '..area', i), 
    include = vertex_attr(net, '..include', i),
    objective = vertex_attr(net, '..objective', i), 
    constraint_met = TRUE,
    row.names = NULL) 
  
  # sort nodes by distance
  dt <- dt[order(dist)
     ][,threshold_met := (include == 1)
      ][,area_cs := cumsum(area * threshold_met)
         ][,area_met := (area_cs <= a_max) & (area_cs >= a_min)
            ][,objective_cs := cumsum(objective * threshold_met)]

  # select stands up to max patch size
  dt <- dt[1:which.min(
    ifelse(a_max - area_cs < 0, NA, a_max - area_cs)
    )]
  
  # evaluate secondary constraint if present
  const <- vertex_attr(net, '..constraint')
  if (!is.null(const)){
    c_v <- const[match(dt$node, V(net)$name)]
    dt <- dt[,constraint := c_v
             ][,constraint_cs := cumsum(c_v * include)
               ][,constraint_met := (c_cs > c_min) & (c_cs < c_max)]
    if (c_enforce){
      if (sum(dt$constraint_met) > 0) {
        dt <- dt[1:max(which(dt$constraint_met == TRUE)),]
      } else {
        dt <- NA
      }
    }
  }
  
  return(dt)
}