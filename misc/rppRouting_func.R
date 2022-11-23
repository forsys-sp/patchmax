#.....................................................................

#' Calculate edge list with distance cost
#'
#' @param g igraph adjacency network
#' @param xy node coordinates from graph
#' @param objective_field field name containing objective values
#' @param sdw distance adjustment
#'
#' @return
#' @export

calc_cost_edgelist <- function(net, obj_field, sdw){
  
  # extract adjacency network edge list
  el <- igraph::as_edgelist(net, names=T) %>% 
    as.data.frame() %>% 
    setNames(c('from','to'))
  
  # calculate pairwise distances
  el$dist <- E(net)$dist
  
  # calculate average objective score for each dyad
  a <- vertex_attr(net, obj_field, match(el$from, V(net)$name)) 
  b <- vertex_attr(net, obj_field, match(el$to, V(net)$name)) 
  el$objective = range01(a + b)
  
  # modify distance based on objective
  el$dist_adj <- el$dist * (1 - el$objective)^sdw
  
  return(el)
}

#.....................................................................

#' Build graph object used to calculate patch
#'
#' @param net igraph object
#' @param obj name of variable containing objective
#' @param sdw numeric between 0 and 5 controlling flexibility
#'
#' @return cpp graph object
#' @export
#'
#' @examples

build_graph <- function(net, obj_field, sdw=1){
  edgelist <- calc_cost_edgelist(net = net, obj_field = obj_field, sdw = sdw)
  cpp_graph <- edgelist %>% select(from, to, dist_adj) %>% makegraph()
  return(cpp_graph)
}

#.....................................................................

#' Build project patch
#'
#' @param cpp_graph graph object 
#' @param v node id to build project from
#' @param proj_area size of project
#'
#' @return dataframe of nearest nodes arranged by distance
#' @export
#'
#' @examples

build_project <- function(cpp_graph, v, psize){
  dmat <- get_distance_matrix(cpp_graph, from=v, to=cpp_graph$dict$ref, allcores=TRUE)
  nn <- find_nearest_ndoes(dmat[1,], area_field = 'area_ha', proj_area = psize)
  return(nn)
}

#.....................................................................

#' Evaluate objective score for all potential project seeds
#'
#' @param cpp_graph cpp graph object
#' @param net igraph graph object
#' @param obj_field name of field containing objective values
#' @param psize project size
#' @param sample_frac fraction of stands to evaluate
#'
#' @return
#' @export
#'
#' @examples

search_best <- function(cpp_graph, net, obj_field, psize, sample_frac = 1){
  out <- -99
  nodes <- sample(V(net)$name, size = (length(V(net)$name) * sample_frac))
  out <- nodes %>% furrr::future_map_dbl(function(i){
    tryCatch({
      cpp_nn <- build_project(cpp_graph, v = i, proj_area = psize)
      o <- vertex_attr(net, obj_field, match(cpp_nn$node, V(net)$name))
      out <- sum(o)
    }, error = function(e){
      e
    }, finally = {
      return(out)
    })
  }, .progress=T, .options = furrr_options(seed = NULL))
  names(out) <- nodes
  return(out)
}

#.....................................................................


subtract_project <- function(net, rpp_nn){
  net <- delete_vertices(net, rpp_nn$node)
  return(net)
}

#.....................................................................

#' Normalize vector to between 0 and 1
#'
#' @param x number or vector to normalize

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
  }

#.....................................................................

#' Arrange nodes by distance and identify nearest
#'
#' @param dist distance matrix
#' @param area_field field containing area
#' @param proj_area desired project area
#'
#' @return
#' @export

find_nearest_ndoes <- function(dist, area_field, proj_area){
  dist <- sort(dist)
  dist_df <- data.frame(node = names(dist), dist = dist)
  dist_df$area_cs <- cumsum(data[dist_df$node, area_field])
  nearest_nodes_df <- dist_df[1:which.min(abs(dist_df$area_cs - proj_area)),]
  return(nearest_nodes_df)
}

#.....................................................................

#' Plot subgraph based on nearest vertices
#'
#' @param geom stand geometry
#' @param net igraph adjacency network
#' @param nn project dataframe
#' @param obj_field field name with objective values
#' @param search logical
#' @param plot logical
#' @param pname project name
#'
#' @return
#' @export

plot_project <- function(geom, net, nn, obj_field, search = NULL, plot=T, pname = NULL){
  
  geom_s <- geom %>% filter(stand_id %in% V(net)$name)
  if(!is.null(search))
    geom_s <- geom_s %>% mutate(!!obj_field := ifelse(search < 0, NA, search))
  
  proj_geom <- geom %>% filter(stand_id %in% nn$node) 
  proj_geom_d <- proj_geom %>% summarize() %>% mutate(pname = pname)
  
  p <- ggplot() + 
    geom_sf(data = geom, aes(fill = get(obj_field)), alpha=0.6, linewidth = 0) +
    geom_sf(data = proj_geom, aes(fill = get(obj_field)), linewidth=0) + 
    geom_sf(data = proj_geom_d, fill=NA, color='black', linewidth=0.5, linetype=1) +
    geom_sf(data = geom_s %>% filter(stand_id == nn$node[1]), fill='black', color='black', linewidth=1) +
    scale_fill_gradientn(colors = sf.colors(10)) + 
    cowplot::theme_map() + 
    guides(fill = 'none')
  
  if(plot){
    print(p)
  } else {
    return(p)
  }
}

#.....................................................................

#' Generate vertex coordinates for a circle
#' 
circleVertices <- function(xc, yc, radius, npoints=25) {
  a <- seq(0, 2*pi, length.out = npoints + 1)
  x <- xc + radius * cos(a)
  y <- yc + radius * sin(a)
  m <- cbind("x" = x, "y" = y)
  return(m)
}