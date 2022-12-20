#' Calculate stands adjacency from input shapefile
#'
#' @param Shapefile Input shapefile
#' @param Adjdist Distance between adjacent stands
#' @param St_id Integer vector containing stands IDs
#'
#' @return g igraph
#'
#' @export
#'
#' @importFrom sf read_sf
#' @importFrom sf st_buffer
#' @importFrom sf st_overlaps
#' @importFrom igraph graph_from_data_frame
#'
calculate_adj <- function(
  Shapefile, 
  Adjdist = 1, 
  St_id,
  method = 'buffer',
  calc_dst = FALSE
  ) {
  
  calculate_adj_func(
    Shapefile = Shapefile, 
    Adjdist = Adjdist, 
    St_id = St_id,
    method = method,
    calc_dist = calc_dst
    )
}


calculate_adj_func <- function(
  Shapefile, 
  Adjdist, 
  St_id,
  method,
  calc_dist = FALSE
  ) {
  
  # buffer approach
  shapefile2 <- Shapefile %>% sf::st_buffer(dist = Adjdist)
  adj <- sf::st_overlaps(shapefile2, sparse = TRUE) %>% data.frame()
  adj2 <- data.frame(A = St_id[adj$row.id], B = St_id[adj$col.id])
  g <- igraph::graph_from_data_frame(adj2, directed = TRUE)
  
  if(calc_dist){
    xy <- st_centroid(Shapefile) %>% st_coordinates() %>% as.data.frame()
    V(g)$X <- xy$X
    V(g)$Y <- xy$Y
    el <- igraph::as_edgelist(g, names=T) %>% as.data.frame() %>% setNames(c('from','to'))
    edge_attr(g) <- list(dist = proxy::dist(xy[el$from,], xy[el$to,], pairwise = T))
  }
  
  return(g)
}



#' Read stands adjacency from input matrix
#'
#' @param Adjlist Adjacency list
#'
#' @return g igraph
#'
#' @export
#'
#' @importFrom igraph graph_from_data_frame
#'
read_adj <- function(Adjlist) {
  read_adj_func(Adjlist)
}

read_adj_func <- function(Adjlist) {
  adj1 <- data.frame(Adjlist)
  adj2 <- data.frame(A = adj1[,1], B = adj1[,2])
  g <- igraph::graph_from_data_frame(adj2, directed = TRUE, vertices = NULL)
  return(g)
}

#' Calculate stands distance from input shapefile
#'
#' @param Shapefile Input shapefile
#'
#' @export
#'
#' @importFrom sf st_coordinates
#' @importFrom sf st_centroid
#'
calculate_dist <- function(Shapefile) {
  calculate_dist_func(Shapefile)
}

calculate_dist_func <- function(Shapefile) {
  Distance_table <- as.matrix(dist(data.frame(sf::st_coordinates(sf::st_centroid(Shapefile))), diag=T, upper=T))
  mode(Distance_table) <- "integer"
  Max <- max(Distance_table)
  Distance_table[] <- vapply(Distance_table, function(x){100*x/Max}, numeric(1))
  return(Distance_table)
}

