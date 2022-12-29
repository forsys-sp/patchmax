#.....................................................................

#' Calculate stand adjacency and edgewise distances
#'
#' @param geom sf-type geometry with attributes 
#' @param St_id vector of sf feature ideas (i.e., the stand id)
#' @param buf_dist numeric distance used to buffer geometry and calculate adjacency
#' @param calc_dist logical for calculating edgewise distance in adjacency network
#' @return adjacency network saved as an igraph network object
#' @export

net_func <- function(geom, id_field, method = 'queen', calc_dist = FALSE) {
  
  id = pull(geom, id_field)
  
  # check overlap between buffered geometry to estimate adjacency
  adj <- switch(method, 
         rook = st_buffer(geom, dist = 1) %>% 
           st_overlaps(sparse = TRUE) %>% 
           data.frame(),
         queen = st_rook(geom) %>% 
           data.frame(), 
         buffer = st_queen(geom) %>% 
           data.frame())
  net <- data.frame(A = id[adj$row.id], B = id[adj$col.id]) %>%
    graph_from_data_frame(directed = TRUE)
  
  if(calc_dist){
    
    # calculate centroid coordinates
    suppressWarnings({
      xy <- geom %>% 
        st_centroid() %>% 
        st_coordinates() %>% 
        as.data.frame()
      xy$name <- id
    })
    
    V(net)$X <- xy$X[match(V(net)$name, xy$name)]
    V(net)$Y <- xy$Y[match(V(net)$name, xy$name)]
    
    # extract edge list 
    el <- igraph::as_edgelist(net, names=T) %>% 
      as.data.frame() %>% 
      setNames(c('from','to'))
    
    # calculate pairwise distances among dyads
    edge_attr(net) <- list(dist = proxy::dist(
      x = xy[match(el$from, xy$name),c('X','Y')], 
      y = xy[match(el$to, xy$name),c('X','Y')], 
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

threshold_func <- function(net, include, area_adjust = 0, objective_adjust = 0){
  V(net)$exclude <- !include
  V(net)$objective[!include] <- V(net)$objective[!include] * objective_adjust
  V(net)$area[!include] <- V(net)$area[!include] * area_adjust
  if(!is.null(V(net)$constraint))
    V(net)$constraint[!include] <- V(net)$constraint[!include] * 0
  return(net)
}

#.....................................................................

#' Build graph object used to calculate patch
#'
#' @param net igraph object
#' @param objective_field name of variable containing objective
#' @param sdw numeric between -1 and 1 controlling flexibility
#' @param epw numeric between -1 and 1 distance multiplier for traversing excluded stands
#' @details `epw` values less than 1 will preferentially select excluded stands
#'   while values greater than 1 avoids excluded stands.
#' @importFrom cppRouting makegraph
#' @return cpp graph object

dist_func <- function(net, objective_field, sdw=0, epw=0){
  
  # extract adjacency network edge list
  el <- igraph::as_edgelist(net, names=T) %>% 
    as.data.frame() %>% 
    setNames(c('from','to'))
  el$dist <- E(net)$dist
  
  # calculate average objective score for each dyad
  a <- vertex_attr(net, objective_field, match(el$from, V(net)$name)) 
  b <- vertex_attr(net, objective_field, match(el$to, V(net)$name)) 
  el$objective = range01((a + b)/2)
  
  # calculate exclude penalty score for each dyad
  a <- vertex_attr(net, 'include', match(el$from, V(net)$name)) 
  b <- vertex_attr(net, 'include', match(el$to, V(net)$name)) 
  el$exclude = ifelse(a | b, 0, 1)
  
  # modify distance based on objective 
  # (increase distance to lower objectives by factor of X)
  el$dist_adj <- el$dist * (2 - el$objective)^(10^sdw)

  # modify distance based on exclusion status
  # (increase distance to excluded areas by factor of X)
  el$dist_adj <- el$dist_adj * ifelse(el$exclude, 10^epw, 1)

  cpp_graph <- makegraph(el[,c('from','to','dist_adj')])
  return(cpp_graph)
}

#.....................................................................

#' Build patch of specified size while minimizing modified distance costs
#'
#' @param start_node character. Node id to start building patch
#' @param cpp_graph graph object built with cppRouting
#' @param net igraph Adjacency network with stand attributes
#' @param a_max numeric. Maximum size of patch
#' @param a_min numeric. Minimum size of patch
#' @param c_max numeric. Maximum secondary constraint of patch
#' @param c_min numeric. Minimum secondary constraint of patch
#' @param c_enforce logical. Should patches outside of constraint be excluded?
#' @importFrom cppRouting get_distance_matrix
#' @return data frame of nearest nodes arranged by distance

build_func <- function(
    start_node, 
    cpp_graph, 
    net, 
    a_max, 
    a_min=-Inf, 
    c_max=Inf, 
    c_min=-Inf, 
    c_enforce=TRUE
  ) {
  
  # calculate distance matrix using Dijkstra's algorithm
  dmat <- get_distance_matrix(
    Graph = cpp_graph, 
    from = start_node, 
    to = cpp_graph$dict$ref)[1,]
  
  # sort nodes by distance
  dist_df <- data.frame(
    node = names(dmat), 
    dist = dmat, 
    area = vertex_attr(net, 'area', match(cpp_graph$dict$ref, V(net)$name)), 
    include = vertex_attr(net, 'include', match(cpp_graph$dict$ref, V(net)$name)),
    objective = vertex_attr(net, 'objective', match(cpp_graph$dict$ref, V(net)$name)), 
    constraint_met = TRUE,
    row.names = NULL) 
  
  # accumulate constraints
  dist_df <- dist_df %>% 
    arrange(dist) %>%
    filter(!is.na(dist)) %>%
    mutate(threshold_met = include == 1) %>%
    mutate(area_cs = cumsum(area * threshold_met)) %>%
    mutate(area_met = (area_cs <= !!a_max ) & (area_cs >= !!a_min)) %>%
    mutate(objective_cs = cumsum(objective * threshold_met))

  dist_df <- dist_df %>% 
    relocate(node, dist, include, contains('objective'), contains('area'), 
             contains('constraint'), contains('threshold'))
  
  # select stands up to max patch size
  a_cs = dist_df$area_cs
  a_d <- ifelse(a_max - a_cs < 0, NA, a_max - a_cs)
  pnodes <- dist_df[1:which.min(a_d),]
  
  # evaluate secondary constraint if present
  if(!is.null(vertex_attr(net, 'constraint'))){
    c_v <- vertex_attr(net, 'constraint', match(pnodes$node, V(net)$name))
    c_cs <- cumsum(c_v * pnodes$include)
    pnodes$constraint <- c_v
    pnodes$constraint_cs <- c_cs
    pnodes$constraint_met <- (c_cs > c_min) & (c_cs < c_max)
    # remove stands that fail constraint 
    if(c_enforce){
      pnodes <- pnodes[1:max(which(pnodes$constraint_met == TRUE)),]
    }
  }
  
  return(pnodes)
}

#.....................................................................

#' Samples stands on regular spatial grid
#'
#' @param geom sf. Stand geometry
#' @param sample_frac numeric. Fraction of stands to evaluate
#' @param spatial_grid logical. Sample at regular spatial intervals?

sample_frac <- function(geom, sample_frac, spatial_grid = TRUE){
  
  # sample fraction of total nodes
  if(sample_frac == 1){
    nodes = geom$stand_id
  } else if(sample_frac > 0 & sample_frac < 1){
    sample_n = round(nrow(geom) * sample_frac)
    # sample using regular spatial grid or as a simple random sample
    if(spatial_grid){
      pt_grd = sf::st_sample(geom, size = sample_n, type = 'regular')
      nodes <- sf::st_join(st_as_sf(pt_grd), geom)$stand_id
    } else {
      nodes = geom$stand_id[sort(sample(1:nrow(geom), sample_n))]
    }
  }
  return(nodes)
}

#.....................................................................

#' Evaluate objective score for all or fraction of patch stand seeds
#'
#' @param cpp_graph cpp graph object
#' @param net igraph graph object
#' @param objective_field name of field containing objective values
#' @param a_max patch size
#' @param sample_frac fraction of stands to evaluate
#' @details Calculates potential patches for all or fraction of landscape stands
#'   in order to identify the initial seed that leads to the highest total
#'   objective score.

  search_func <- function(
    net, 
    cpp_graph, 
    nodes = NULL, 
    objective_field, 
    a_max, 
    a_min, 
    c_max=Inf, 
    c_min=-Inf,
    t_limit=0,
    return_all=FALSE, 
    show_progress=FALSE,
    print_errors=FALSE
    ){
  
    if(is.null(nodes)){
      nodes = V(net)$name
    }
    
    # calculate objective score for all potential patches
    out <- nodes %>% furrr::future_map_dbl(function(i){
      proj_obj <- NA
      tryCatch({
        patch <- build_func(i, cpp_graph, net, a_max, a_min, c_max, c_min)
        t_sum <- sum(patch$area * patch$threshold_met)/sum(patch$area)
        # t_sum <- sum(patch$threshold_met)/nrow(patch)
        if(!last(patch$area_met) | !last(patch$constraint_met))
          stop(paste0('Constaint(s) not met @', i))
        if(t_sum <= (1 - t_limit))
          stop(paste0('Threshold limit exceeded @', i))
        proj_obj = sum(patch$objective, na.rm=T)
        return(proj_obj)
      }, warning = function(w){}, 
      error = function(e){
        if(print_errors) print(e)
        return(NA)
        })
    }, .progress=show_progress, .options = furrr_options(seed = NULL))

    if(sum(!is.na(out)) == 0){
      stop('No patches possible')
    }
    
    names(out) <- nodes
    if(return_all){
      return(out)
    } else {
      return(out[which.max(out)])
    }
}

#.....................................................................

#' Helper function to normalize vector to between 0 and 1
#'
#' @param x number or vector to normalize
#' @export

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

#' Helper function to calculate rook adjacency in geometry
#'
#' @param geom sf-class geometry
#' @export

st_rook = function(a, b = a){
  st_relate(a, b, pattern = "F***1****")
}

#' Helper function to calculate queen adjacency in geometry
#'
#' @param geom sf-class geometry
#' @export

st_queen <- function(a, b = a){
  st_relate(a, b, pattern = "F***T****")
}

