#' Calculate stands adjacency from input shapefile
#'
#' @param Shapefile Input shapefile
#' @param Adjdist Distance between adjacent stands
#' @param St_id Numeric vector of integer stands IDs
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
calculate_adj <- function(Shapefile, Adjdist, St_id) {
  fun3(Shapefile, Adjdist, St_id)
}


fun3 <- function(Shapefile, Adjdist, St_id) {
  shapefile <- sf::read_sf(Shapefile)
  shapefile2 <- sf::st_buffer(shapefile, dist = Adjdist)
  adj <- sf::st_overlaps(shapefile2, sparse = TRUE)
  adj1 <- data.frame(adj)
  adj2 <- data.frame(A = St_id[adj1$row.id], B = St_id[adj1$col.id])
  g <- igraph::graph_from_data_frame(adj2, directed = TRUE, vertices = NULL)
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
  fun3(Adjlist)
}

read_adj <- function(Adjlist) {
  adj1 <- data.frame(Adjlist)
  adj2 <- data.frame(A = adj1[,1], B = adj1[,2])
  g <- igraph::graph_from_data_frame(adj2, directed = TRUE, vertices = NULL)
  return(g)
}
