#' Simulate landscape projects
#' @param St_id Numeric vector of integer stands IDs
#' @param St_adj Adjacency input graph created through calculate_adj or read_adj functions. The graph vertices must be named.
#' @param St_area Numeric vector of stands area
#' @param St_objective Numeric vector of stands objective
#' @param St_seed Numeric vector of stands IDs seeds. If NULL, then stand seed is not applied.
#' @param P_size Project size
#' @param P_size_slack Project size slack, between 0 and 1, Where 0 means no deviation.
#' @param P_number Number of projects to simulate
#' @param St_threshold Numeric vector of stands threshold value.Coupled with St_threshold_value. If NULL, then stand threshold is not applied.
#' @param St_threshold_value Stands threshold lower value.
#' @param St_distances Stand distance table. Coupled with DW parameter. If NULL, then stand distance weight function is not applied.
#' @param SDW Stand distance weight parameter. If NULL, then the value = 1 is used by default.
#' @param P_constraint Numeric vector of stands value for the project constraint.Coupled with P_constraint_max_value and P_constraint_min_value. If NULL, then project constraint is not applied.
#' @param P_constraint_max_value Project constraint upper value.
#' @param P_constraint_min_value Project constraint lower value.
#' @param Candidate_min_size Minimal size for project coded with types 2 and 3. If NULL, then the value = ‘0.25*P_size’ is used by default. Project type codes interpret if the project is valid (i.e. if P_size apply), constrained (i.e. if P_constraint apply) or none of them. Where type 0 are valid non-constrained projects (i.e. only P_size apply), type 1 are valid constrained projects (i.e. P_size and P_constraint apply), type 2 are invalid constrained projects (i.e. P_size does not apply but P_constraint apply) and type 3 are non-optimized projects that can be invalid and/or non-constrained (i.e. P_size and/or P_constraint do not apply).
#'
#' @return matrix containing stand id, stand count, objective score, and type where type = 0 are valid non-contained projects, type = 1 are valid constrained projects, type = 2 are invalid constrained projects, and type == 3 are invalid and non-constrained
#' @export

simulate_projects <- function(
    geom,                             # new
    St_id,                            # add to geom / overwrite
    St_adj,                           # deleted
    St_area,                          # add to geom / overwrite
    St_objective,                     # add to geom / overwrite
    St_seed = NULL,                   # deleted
    P_size,                           # transfer
    P_size_slack = 0.05,              # mutate to min project area
    P_size_ceiling = Inf,             # not required: to delete
    P_number = 1,                     # transfer
    St_threshold = NULL,              # rework       
    St_threshold_value = NULL,        # rework
    St_distances = NULL,              # deleted
    SDW = NULL,                       # direct transfer
    P_constraint = NULL,              # add to geom / overwrite
    P_constraint_max_value = Inf,     # 
    P_constraint_min_value = -Inf,    # 
    Candidate_min_size = NULL         # deleted
){
  
  geom_s <- geom
  geom_s$St_id = St_id
  geom_s$St_area = St_area
  geom_s$St_objective = St_objective
  geom_s$P_constraint = P_constraint
  
  patchmax <- patchmax_generator$new(geom, 'St_id', 'St_objective', 'St_area', P_size)
  patchmax$params <- list(
    area_min = P_size - (P_size * P_size_slack),
    sdw = SDW,
    threshold = paste0(St_threshold, ' >= ', St_threshold_value),
    constraint_field = 'P_constraint', 
    constraint_max = P_constraint_max_value,
    constraint_min = P_constraint_min_value)
  
  patchmax$simulate(P_number)
}

#.....................................................................

#' Estimates stand adjacency and calculates edgewise distances
#'
#' @param shp sf-type geometry with attributes 
#' @param St_id vector of sf feature ideas (i.e., the stand id)
#' @param buf_dist numeric distance used to buffer geometry and calculate adjacency
#' @param calc_dist logical for calculating edgewise distance in adjacency network
#'
#' @return adjacency network saved as an igraph network object
#' @export

calc_adj_network_func <- function(shp, St_id, buf_dist = 1, calc_dist = FALSE) {
  
  # check overlap between buffered geometry to estimate adjacency
  # shp_buf <- shp %>% sf::st_buffer(dist = buf_dist)
  # adj <- sf::st_overlaps(shp_buf, sparse = TRUE) %>% data.frame()
  adj <- sf::st_touches(shp, sparse = TRUE) %>% data.frame()
  net <- data.frame(A = St_id[adj$row.id], B = St_id[adj$col.id]) %>%
    graph_from_data_frame(directed = TRUE)
  
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
  
  # message(glue::glue('{round(sum(include)/length(include)*100)}% stands met threshold'))
  
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
#'
#' @details `epw` values less than 1 will preferentially select excluded stands
#'   while values greater than 1 avoids excluded stands.
#'
#' @return cpp graph object

build_graph_func <- function(net, objective_field, sdw=0, epw=0){
  
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
  a <- vertex_attr(net, 'include', match(el$from, V(net)$name)) 
  b <- vertex_attr(net, 'include', match(el$to, V(net)$name)) 
  el$exclude = ifelse(a | b, 0, 1)
  
  # modify distance based on objective (except for excluded stands)
  el$dist_adj <- el$dist * (1 - el$objective)^(10^sdw)
                                               
  # modify distance based on exclusion status
  el$dist_adj <- el$dist_adj * ifelse(el$exclude, 10^epw, 1)

  # cppdistmat(to, from, dist, node_cnt, to_name, from_name)
  # cppdistmat(Graph$data[,2], Graph$data[,1], Graph$data[,3], Graph$nbnode, to_id, from_id)
  
  cpp_graph <- makegraph(el[,c('from','to','dist_adj')])
  return(cpp_graph)
}

#.....................................................................

#' Build patch of specified size while minimizing modified distance costs
#'
#' @param start_node character. Node id to start building patch
#' @param cpp_graph graph object built with cppRouting
#' @param net igraph Adjacency network with stand attributes
#' @param a_max numeric. Target (i.e., maximum) size of patch
#' @param a_min numeric. Mininmum size of patch
#' @param c_max numeric. Maximum secondary constraint total for patch
#' @param c_min numeric. Minimum secondary constraint total for patch
#' @param c_enforce logical. Should patches outside of constraint be excluded?
#'
#' @return data frame of nearest nodes arranged by distance

build_patch_func <- function(start_node, cpp_graph, net, a_max, a_min=-Inf, c_max=Inf, c_min=-Inf, c_enforce=TRUE){
  
  # calculate distance matrix using Dijkstra's algorithm
  dmat <- get_distance_matrix(cpp_graph, from=start_node, to=cpp_graph$dict$ref, allcores=FALSE)[1,]
  
  # sort nodes by distance
  dist_df <- data.frame(
    node = names(dmat), 
    dist = dmat, 
    area = vertex_attr(net, 'area', match(cpp_graph$dict$ref, V(net)$name)), 
    objective = vertex_attr(net, 'objective', match(cpp_graph$dict$ref,V(net)$name)), 
    threshold_met = vertex_attr(net, 'exclude', match(cpp_graph$dict$ref,V(net)$name)) != 1,
    row.names = NULL)
  
  # identify nearest nodes up to area limit
  dist_df <- dist_df %>% 
    arrange(dist) %>%
    filter(!is.na(dist)) %>%
    mutate(area_cs = cumsum(area)) %>%
    mutate(area_met = (area_cs <= !!a_max) & (area_cs >= !!a_min)) %>%
    mutate(objective_cs = cumsum(objective))

  dist_df <- dist_df %>% 
    dplyr::relocate(node, dist, 
                    contains('objective'), contains('area'), 
                    contains('constraint'), contains('threshold'))
  
  pnodes <- dist_df[1:which.min(abs(dist_df$area_cs - a_max)),]
  
  # evaluate secondary constraint if present
  if(!is.null(vertex_attr(net, 'constraint'))){
    pnodes$constraint <- vertex_attr(net, 'constraint', match(pnodes$node, V(net)$name))
    pnodes$constraint_cs <- cumsum(pnodes$constraint)
    pnodes$constraint_met <- (pnodes$constraint_cs > c_min) & (pnodes$constraint_cs < c_max)
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
  if(sample_frac > 0 & sample_frac <= 1){
    sample_n = round(nrow(geom) * sample_frac)
    # sample using regular spatial grid or as a simple random sample
    if(spatial_grid){
      pt_grd = sf::st_sample(geom, size = sample_n, type = 'regular')
      nodes <- sf::st_join(st_as_sf(pt_grd), geom)$stand_id
    } else {
      nodes = geom$stand_id[sort(sample(1:nrow(geom), sample_n))]
    }
  }
}

#.....................................................................

#' Evaluate objective score for all or fraction of patch stand seeds
#'
#' @param cpp_graph cpp graph object
#' @param net igraph graph object
#' @param objective_field name of field containing objective values
#' @param a_max patch size
#' @param sample_frac fraction of stands to evaluate
#'
#' @details Calculates potential patches for all or fraction of landscape stands
#'   in order to identify the initial seed that leads to the highest total
#'   objective score.
#'

  search_best_func <- function(net, cpp_graph, nodes = NULL, objective_field, a_max, a_min, c_max=Inf, c_min=-Inf, return_all=FALSE, show_progress=FALSE){
  
    if(is.null(nodes)){
      nodes = V(net)$name
    }
    
    # calculate objective score for all potential patches
    out <- nodes %>% furrr::future_map_dbl(function(i){
      proj_obj <- NA
      tryCatch({
        patch <- build_patch_func(i, cpp_graph, net, a_max, a_min, c_max=c_max, c_min=c_min)
        proj_obj = sum(patch$objective)
        # assess constraints
        last <- tail(patch,1)
        if(!is.null(last$constraint_met)){
          if(last$area_met & !last$constraint_met) 
            proj_obj = NA
          if(!last$area_met & last$constraint_met)
            proj_obj = NA
        }
        return(proj_obj)
      }, warning = function(w){}, 
      error = function(e) return(0))
    }, .progress=show_progress, .options = furrr_options(seed = NULL))

    if(sum(!is.na(out)) == 0){
      stop('No potential patches meet constraints')
    }
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
#' @export

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
  }
