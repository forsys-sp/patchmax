
calc_adj_network_func <- function(shp, buf_dist = 1, St_id, calc_dist = FALSE) {
  
  # check overlap between buffered geometry to estimate adjacency
  shp_buf <- shp %>% sf::st_buffer(dist = buf_dist)
  adj <- sf::st_overlaps(shp_buf, sparse = TRUE) %>% data.frame()
  net <- data.frame(A = St_id[adj$row.id], B = St_id[adj$col.id]) %>%
    igraph::graph_from_data_frame(directed = TRUE)
  
  if(calc_dist){
    
    # calculate centroid coordinates
    suppressWarnings({
      xy <- st_centroid(shp) %>% 
        st_coordinates() %>% 
        as.data.frame()
    })
    V(net)$X <- xy$X
    V(net)$Y <- xy$Y
    
    # extract edge list 
    el <- igraph::as_edgelist(net, names=T) %>% 
      as.data.frame() %>% 
      setNames(c('from','to'))
    
    # calculate pairwise distances among dyads
    edge_attr(net) <- list(dist = proxy::dist(
      x = xy[match(el$from,St_id),], 
      y = xy[match(el$to,St_id),], 
      pairwise = T))
  }
  
  return(net)
}

#.....................................................................

#' Set threshold for project inclusion
#'
#' @param net igraph adjacency network
#' @param include logical vector of length `vcount(net)`
#' @param area_adj value 0-1, percent area 'cost' of excluded stands
#' @param obj_adj value 0-1, percent contribution of excluded stands to project score
#'
#' @return igraph adjecency network

set_threshold_func <- function(net, include, area_adj = 0, obj_adj = 0){
  message(paste0(round(sum(!include)/length(include)*100), '% outside threshold'))
  V(net)$exclude <- !include
  V(net)$objective[!include] <- V(net)$objective[!include] * obj_adj
  V(net)$area[!include] <- V(net)$area[!include] * area_adj
  V(net)$constraint[!include] <- V(net)$constraint[!include] * area_adj
  return(net)
}

#.....................................................................

#' Build graph object used to calculate patch
#'
#' @param net igraph object
#' @param obj name of variable containing objective
#' @param sdw numeric > 0 controlling flexibility
#' @param epw numeric > 0 distance multiplier for traversing unavailable stands
#'
#' @details `epw` values less than 1 will preferentially select excluded stands
#'   while values greater than 1 avoids excluded stands.
#'
#' @return cpp graph object
#' @export

build_graph_func <- function(net, obj_field, sdw=1, epw=1){
  
  # extract adjacency network edge list
  el <- igraph::as_edgelist(net, names=T) %>% 
    as.data.frame() %>% 
    setNames(c('from','to'))
  el$dist <- E(net)$dist
  
  # calculate average objective score for each dyad
  a <- vertex_attr(net, obj_field, match(el$from, V(net)$name)) 
  b <- vertex_attr(net, obj_field, match(el$to, V(net)$name)) 
  el$objective = range01(a + b)
  
  # calculate exclude penalty score for each dyad
  a <- vertex_attr(net, 'exclude', match(el$from, V(net)$name)) 
  b <- vertex_attr(net, 'exclude', match(el$to, V(net)$name)) 
  el$exclude = ifelse(a | b, 1, 0)
  
  # modify distance based on objective
  el$dist_adj <- el$dist * (1 - el$objective)^sdw
  
  # modify distance based on exclusion status
  el$dist_adj <- el$dist_adj * ifelse(el$exclude, epw, 1)
  
  cpp_graph <- makegraph(el[,c('from','to','dist_adj')])
  return(cpp_graph)
}

#.....................................................................

#' Build project patch
#'
#' @param cpp_graph graph object 
#' @param v node id to build project from
#' @param proj_area size of project
#'
#' @return data frame of nearest nodes arranged by distance
#' @export

grow_project_func <- function(cpp_graph, net, start_node, proj_area){
  
  dmat <- get_distance_matrix(cpp_graph, from=start_node, to=cpp_graph$dict$ref, allcores=TRUE)[1,]
  area <- vertex_attr(net, 'area')
  objective <- vertex_attr(net, 'objective')
  
  # arrange nodes by distance
  dist_df <- data.frame(node = names(dmat), dist = dmat, area = area, objective = objective)
  dist_df <- dist_df[order(dist_df$dist),]
  
  dist_df$area_cs <- cumsum(dist_df$area)
  dist_df$objective_cs <- cumsum(dist_df$objective)
  
  # identify nearest nodes up to limit
  pnodes <- dist_df[1:which.min(abs(dist_df$area_cs - proj_area)),]
  
  # TODO deal with secondary constraints
  
  return(pnodes)
}

#.....................................................................

#' Evaluate objective score for all potential project seeds
#'
#' @param cpp_graph cpp graph object
#' @param net igraph graph object
#' @param obj_field name of field containing objective values
#' @param proj_area project size
#' @param sample_frac fraction of stands to evaluate
#'
#' @return
#' @export

search_best_func <- function(cpp_graph, net, obj_field, proj_area, sample_frac = 1, return_all=FALSE){
  
  # sample fraction of stands to evaluate projects
  nodes <- sample(V(net)$name, size = (length(V(net)$name) * sample_frac))
  
  # calculate objective score for all potential projects
  out <- nodes %>% furrr::future_map_dbl(function(i){
    o <- -99
    tryCatch({
      cpp_nn <- grow_project_func(cpp_graph, net = net, start_node = i, proj_area = proj_area)
      o <- vertex_attr(net, obj_field, match(cpp_nn$node, V(net)$name)) %>% sum()
    }, error = function(e){
      e
    }, finally = {
      return(o)
    })
  }, .progress=T, .options = furrr_options(seed = NULL))
  
  names(out) <- nodes
  
  if(return_all){
    return(out)
  } else {
    return(out[which.max(out)])
  }
  
}

#.....................................................................

#' Subtract projects from adjacency network
#'
#' @param net 
#' @param rpp_nn 
#'
#' @return
#' @export

subtract_project_func <- function(net, rpp_nn){
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
