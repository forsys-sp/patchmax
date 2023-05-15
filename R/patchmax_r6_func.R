#.....................................................................

#' Calculate stand adjacency and edgewise distances
#'
#' @param geom sf-type geometry with attributes 
#' @param id_field vector of sf feature ideas (i.e., the stand id)
#' @param method adjacency method to use: queen, rook, buffer
#' @importFrom proxy dist
#' @importFrom igraph as_edgelist graph_from_data_frame
#' @importFrom sf st_buffer st_overlaps st_centroid st_coordinates
#' @return adjacency network saved as an igraph network object

create_network <- function(
    geom, 
    id_field, 
    method = 'queen'
  ) {
  
  id = pull(geom, id_field)
  
  # identify adjacency
  adj <- switch(method, 
         rook = st_buffer(geom, dist = 1) %>% 
           st_overlaps(sparse = TRUE) %>% 
           data.frame(),
         queen = st_rook(geom) %>% 
           data.frame(), 
         buffer = st_queen(geom) %>% 
           data.frame())
  
  # build adjacency network
  vert <- geom %>% st_drop_geometry() %>% select(id_field)
  net <- data.frame(A = id[adj$row.id], B = id[adj$col.id]) %>%
    graph_from_data_frame(
      directed = TRUE, 
      vertices = vert)

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
  V(net)$..sample = 1
  
  # extract edge list 
  el <- as_edgelist(net, names=T) %>% 
    as.data.frame() %>% 
    setNames(c('from','to'))
  
  # calculate pairwise distances among dyads
  edge_attr(net) <- list(dist = proxy::dist(
    x = xy[match(el$from, xy$name),c('X','Y')], 
    y = xy[match(el$to, xy$name),c('X','Y')], 
    pairwise = T))
  
  return(net)
}


#.....................................................................

#' Samples stands on regular spatial grid
#'
#' @param geom sf. Stand geometry
#' @param sample_frac numeric. Fraction of stands to evaluate
#' @param spatial_grid logical. Sample at regular spatial intervals?
#' 
#' @importFrom sf st_sample st_join

sample_func <- function(
    geom, 
    sample_frac, 
    id_field, 
    spatial_grid = TRUE, 
    rng_seed = NULL
) {
  
  # sample fraction of total nodes
  if(sample_frac == 1){
    nodes = pull(geom, id_field)
  } else if(sample_frac > 0 & sample_frac < 1){
    sample_n = round(nrow(geom) * sample_frac)
    # sample using regular spatial grid or as a simple random sample
    if(spatial_grid){
      set.seed(rng_seed)
      pt_grd = st_sample(geom, size = sample_n, type = 'regular')
      nodes <- st_join(st_as_sf(pt_grd), geom) %>% pull(id_field)
    } else {
      nodes = geom[sort(sample(1:nrow(geom), sample_n)),] %>% pull(id_field)
    }
  }
  return(nodes)
}

#.....................................................................

#' Set threshold for patch inclusion
#'
#' @param net igraph adjacency network
#' @param include logical vector of same length as the count of network nodes
#' @param area_fadjust value 0-1 represent area 'cost' of excluded stands
#' @param objective_adjust value 0-1, percent contribution of excluded stands
#'   to patch score
#' @details By default, excluded stands don't count towards the total project
#'   objective or the total project size. Setting the area and objective adjust
#'   to a fraction greater than 0 changes this behavior, allowing for a fraction
#'   of the both values for excluded stands to be counted.
#' @return igraph adjacency network
#' @importFrom igraph V

threshold_func <- function(
    net, 
    include, 
    area_adjust = 0, 
    objective_adjust = 0
  ) {
  
  V(net)$..objective[!include] <- V(net)$..objective[!include] * objective_adjust
  V(net)$area[!include] <- V(net)$area[!include] * area_adjust
  if(!is.null(V(net)$..constraint)){
    V(net)$..constraint[!include] <- V(net)$..constraint[!include] * 0 
  }
  return(net)
}

#.....................................................................

#' Build graph object used to calculate patch
#'
#' @param net igraph object
#' @param objective_field name of variable containing objective
#' @param sdw numeric between 0 and 1 distance multiplier for traversing low objective stands
#' @param epw numeric between 0 and 1 distance multiplier for traversing excluded stands
#' @details `epw` and `sdw` have exponential effects. At 1, distance values are 10x greater than at 0. Setting either value to 0 leaves distances unmodified.
#' @importFrom cppRouting makegraph
#' @importFrom igraph as_edgelist
#' @return cpp graph object

distance_func <- function(
    net, 
    objective_field, 
    sdw = 0, 
    epw = 0
  ) {
  
  if(igraph::gsize(net) == 0){
    return(cpp_graph = NULL)
  }
  
  # extract adjacency network edge list
  el <- as_edgelist(net, names=T) %>% 
    as.data.frame() %>% 
    setNames(c('from','to'))
  el$dist <- E(net)$dist
  
  # calculate average objective score for each dyad
  a <- vertex_attr(net, '..objective', match(el$from, V(net)$name)) 
  b <- vertex_attr(net, '..objective', match(el$to, V(net)$name)) 
  el$objective = range01((a + b)/2)
  
  # calculate exclude penalty score for each dyad
  a <- vertex_attr(net, '..include', match(el$from, V(net)$name)) 
  b <- vertex_attr(net, '..include', match(el$to, V(net)$name)) 
  el$exclude = ifelse(a | b, 0, 1)
  
  # modify distance based on objective 
  # (increase distance to lower objectives by factor of X)
  el$dist_adj <- el$dist * (1+(1-el$objective)*(10^sdw))

  # modify distance based on exclusion status
  # (increase distance to excluded areas by factor of X)
  el$dist_adj <- el$dist_adj * ifelse(el$exclude, 10^epw, 1)

  cpp_graph <- makegraph(el[,c('from','to','dist_adj')])
  return(cpp_graph)
}

#.....................................................................

#' Build patch of specified size while minimizing modified distance costs
#'
#' @param seed character. Node id to start building patch
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
    seed, 
    edge_dat,
    node_dat,
    a_max, 
    a_min=-Inf, 
    c_max=Inf, 
    c_min=-Inf, 
    c_enforce=TRUE
  ) {
  
  dt <- node_dat
  
  # calculate distance matrix using Dijkstra's algorithm
  dt$dist <- get_distance_matrix(edge_dat, seed, edge_dat$dict$ref)[1,]
  # dt$dist <- calc_network_distance(edge_dat, seed, edge_dat$dict$ref, TRUE, max_search_dist)[1,]
  
  # sort nodes by distance, retain stands up to max size, evaluate secondary constraint
  dt <- dt[order(dist)
  ][,threshold_met := (include == 1)
  ][,area_cs := cumsum(area * threshold_met)
  ][,area_met := (area_cs <= a_max) & (area_cs >= a_min)
  ][,objective_cs := cumsum(objective * threshold_met)
  ][1:which.min(abs(a_max - area_cs))
  ][!is.na(dist)
  ][,constraint_cs := cumsum(constraint * include)
  ][,constraint_met := (constraint_cs > c_min) & (constraint_cs < c_max)
  ][1:max(which(constraint_met))]
  
  return(dt)
}

#.....................................................................

#' Evaluate objective score for all or fraction of patch stand seeds
#'
#' @param net igraph graph object
#' @param cpp_graph cpp graph object
#' @param nodes which nodes to evaluate as potential patches
#' @param objective_field name of field containing objective values
#' @param a_max maxmimum patch size
#' @param a_min minimum patch size (only applicable if constraint is present)
#' @param c_max maximum constraint value
#' @param c_min minimum constraint value
#' @param t_limit maximum portion (0-1) of patch area excluded by threshold
#' @param return_all logical Return search values for all nodes evaluated
#' @param show_progress logical Show progress bar 
#' @param print_errors logical Print reason for invalid search (for debugging)
#'
#' @details Calculates potential patches for all or fraction of landscape stands in order to identify the initial seed that leads to the highest total objective score.
#' 
#' @importFrom furrr future_map_dbl

  search_func <- function(
    net, 
    edge_dat,
    node_dat,
    nodes = NULL, 
    objective_field, 
    a_max, 
    a_min, 
    c_max=Inf, 
    c_min=-Inf,
    t_limit=0,
    show_progress=FALSE,
    print_errors=FALSE
    ){
  
    if(is.null(nodes)){
      nodes = V(net)$name
    }
    
    # calculate objective score for all potential patches
    search_out <- nodes %>% future_map_dbl(function(i) {
      
      proj_obj <- NA
      
      tryCatch({
        
        # build patch at node i
        patch <- build_func(i, edge_dat, node_dat, a_max, a_min, c_max, c_min)
        
        # calculate thershold exclusion limit
        t_sum <- sum(patch$area * patch$threshold_met)/sum(patch$area)
        
        # error if neither area or secondary constraint are not met
        if(!last(patch$area_met) | !last(patch$constraint_met)){
          stop(paste0('Constaint(s) not met @', i))
        }
        
        # error if exclusion rate greater than exclusion limit
        if(t_sum <= (1 - t_limit)){
          stop(paste0('Threshold limit exceeded @', i))
        }
        
        proj_obj = last(patch$objective_cs)
        return(proj_obj)
      }, 
      error = function(e){
        if(print_errors) print(e)
        return(NA)
      })
    }, .progress=show_progress, .options = furrr_options(seed = NULL))

    # label search output with unit IDs
    names(search_out) <- nodes
    
    return(search_out)
}

  
#' Helper for reporting patch statistics in build function
#'
#' @param patch patch data
#' @param verbose print statistics to screen?
#'
#' @return Dataframe with patch statistics
#'
calc_patch_stats <- function(patch, verbose = TRUE){
  
  stats = data.frame(
    seed = patch$node[1], 
    area = round(max(patch$area_cs),1),
    coverage = round(sum(patch$area),1),
    objective = round(sum(patch$objective * patch$threshold_met),1),
    constraint = round(max(patch$constraint_cs),1),
    excluded = 100-round(max(patch$area_cs)/sum(patch$area)*100))
  
  if(verbose){
    message(glue::glue('Patch stats: seed {stats$seed}, obj {stats$objective}, area {stats$area}, coverage {stats$coverage}, constraint {stats$constraint}, excluded {stats$excluded}%'))
  }

  return(stats)
}
  
  
#.....................................................................

#' Helper function to normalize vector to between 0 and 1
#'
#' @param x number or vector to normalize

range01 <- function(x){
  if(min(x) == max(x)){
    rep(0.5, length(x))
  } else {
    (x-min(x))/(max(x)-min(x))
  }
}

#' Helper function to calculate rook adjacency in geometry
#'
#' @param geom sf-class geometry
#' 
#' @importFrom sf st_relate

st_rook = function(geom){
  st_relate(geom, geom, pattern = "F***1****")
}

#' Helper function to calculate queen adjacency in geometry
#'
#' @param geom sf-class geometry
#' 
#' @importFrom sf st_relate

st_queen <- function(geom){
  st_relate(geom, geom, pattern = "F***T****")
}

