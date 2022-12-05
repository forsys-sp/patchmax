# TODO setup patchmax oob to be called by simulate projects

simulate_projects <- function(
    geom, # new
    St_id, 
    St_adj, 
    St_area, 
    St_objective,
    St_seed = NULL,
    P_size, 
    P_size_slack = 0.05, 
    P_size_ceiling = Inf,
    P_number = 1,
    St_threshold = NULL, 
    St_threshold_value = NULL,
    St_distances = NULL,
    SDW = NULL,
    P_constraint = NULL, 
    P_constraint_max_value = Inf, 
    P_constraint_min_value = -Inf, 
    Candidate_min_size = NULL
){
  dat <- data.frame(
    St_id = St_id,
    St_area = St_area,
    St_objective = St_objective,
    P_constraint = P_constraint)
 pm <- pm$initialize(geom, St_id, ) 
  
}

#' Modified version of Patchmax::calculate_adj that calculates edgewise distances
#'
#' @param shp sf-type geometry with attributes 
#' @param St_id vector of sf feature ideas (i.e., the stand id)
#' @param buf_dist numeric distance used to buffer geometry and calculate adjacency
#' @param calc_dist logical for calculating edgewise distance in adjacency network
#'
#' @return adjacency network saved as an igraph network object

#' @export

#.....................................................................

calc_adj_network_func <- function(shp, St_id, buf_dist = 1, calc_dist = FALSE) {
  
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

#' Set threshold for patch inclusion
#'
#' @param net igraph adjacency network
#' @param include logical vector of same length as the count of network nodes
#' @param area_penality value 0-1 represent area 'cost' of excluded stands
#' @param objective_penality value 0-1, percent contribution of excluded stands
#'   to patch score
#'
#' @details By default, excluded stands don't count towards the total project
#'   objective or the total project size. Setting the area and objective adjust
#'   to a fraction greater than 0 changes this behavior, allowing for a fraction
#'   of the both values for excluded stands to be counted.
#'
#' @return igraph adjacency network

set_threshold_func <- function(net, include, area_adjust = 0, objective_adjust = 0){
  message(glue::glue('{round(sum(include)/length(include)*100)}% stand available; adjusted excluded area by {area_adjust} and objective by {objective_adjust}'))
  
  V(net)$exclude <- !include
  V(net)$objective[!include] <- V(net)$objective[!include] * objective_adjust
  V(net)$area[!include] <- V(net)$area[!include] * area_adjust
  V(net)$constraint[!include] <- V(net)$constraint[!include] * area_adjust
  
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

build_graph_func <- function(net, objective_field, sdw=1, epw=1){
  
  # extract adjacency network edge list
  el <- igraph::as_edgelist(net, names=T) %>% 
    as.data.frame() %>% 
    setNames(c('from','to'))
  el$dist <- E(net)$dist
  
  # calculate average objective score for each dyad
  a <- vertex_attr(net, objective_field, match(el$from, V(net)$name)) 
  b <- vertex_attr(net, objective_field, match(el$to, V(net)$name)) 
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

#' Build patch of specified size while minimizing modified distance costs
#'
#' @param cpp_graph graph object 
#' @param v node id to start building patch
#' @param patch_area size of patch
#'
#' @return data frame of nearest nodes arranged by distance
#' @export

grow_patch_func <- function(cpp_graph, net, start_node, patch_area){
  
  # calculate distance matrix using Dijkstra's algorithm
  dmat <- get_distance_matrix(cpp_graph, from=start_node, to=cpp_graph$dict$ref, allcores=TRUE)[1,]
  
  # sort nodes by distance
  dist_df <- data.frame(
    node = names(dmat), 
    dist = dmat, 
    area = vertex_attr(net, 'area', match(cpp_graph$dict$ref,V(net)$name)), 
    objective = vertex_attr(net, 'objective', match(cpp_graph$dict$ref,V(net)$name)), 
    row.names = NULL)
  
  # identify nearest nodes up to area limit
  dist_df <- dist_df[order(dist_df$dist),]
  dist_df$area_cs <- cumsum(dist_df$area)
  pnodes <- dist_df[1:which.min(abs(dist_df$area_cs - patch_area)),]

  # TODO test for secondary project constraint as part of project stand list
  if(!is.null(constraint)){
    pnodes$constraint = vertex_attr(net, 'constraint', match(pnodes$node, V(net)$name))
    pnodes$constraint_cs <- cumsum(pnodes$constraint)
    gt_min = pnodes$constraint_cs > -Inf
    lt_max = pnodes$constraint_cs < 5
    pnodes$constraint_met = gt_min & lt_max
    max(which(pnodes$constraint_met == 1))
  }
  
  return(pnodes)
}

#.....................................................................

#' Evaluate objective score for all or fraction of patch stand seeds
#'
#' @param cpp_graph cpp graph object
#' @param net igraph graph object
#' @param objective_field name of field containing objective values
#' @param patch_area patch size
#' @param sample_frac fraction of stands to evaluate
#'
#' @details Calculates potential patches for all or fraction of landscape stands
#'   in order to identify the initial seed that leads to the highest total
#'   objective score.
#'
#' @return
#' @export

  search_best_func <- function(
    net, cpp_graph, objective_field, patch_area, sdw=1, epw=1, sample_frac = 1, 
    return_all=FALSE, show_progress=FALSE, sample_type = 'spatial'){
  
    # sample fraction of total nodes
    if(sample_frac > 0 & sample_frac < 1){
      sample_n = round(igraph::vcount(net) * sample_frac)
      # sample using regular spatial grid or as a simple random sample
      if(sample_type == 'spatial'){
        geom_s = geom %>% dplyr::filter(stand_id %in% V(net)$name)
        pt_grd = sf::st_sample(geom_s, size = sample_n, type = 'regular')
        nodes <- sf::st_join(st_as_sf(pt_grd), geom)$stand_id
      } else {
        nodes = V(net)$name[sort(sample(1:length(V(net)$name), sample_n))]
      }
    }

    # calculate objective score for all potential patches
    out <- nodes %>% furrr::future_map_dbl(function(i){
      proj_obj <- -99
      tryCatch({
        patch <- grow_patch_func(cpp_graph, net, i, patch_area)
        proj_obj <- sum(vertex_attr(net, objective_field, match(patch$node, V(net)$name)))
        return(proj_obj)
      }, error = function(e) return(0))
    }, .progress=show_progress, .options = furrr_options(seed = NULL))
    
    names(out) <- nodes
    
    if(return_all){
      return(out)
    } else {
      return(out[which.max(out)])
    }
}

#.....................................................................

#' Normalize vector to between 0 and 1
#'
#' @param x number or vector to normalize

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
  }
