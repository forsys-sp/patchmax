#' Dijkstra's Algorithm
#'
#'\code{dijkstra} takes a edgelist and an initial node and calculates the shortest path from the
#'initial node to every other node in the graph.
#'
#' @param edgelist Must be a data frame with three variables (from, to, dist)
#' @param start Must be numeric scalar or integer
#'
#' @return The shortest path from the initial node to every other node
#'
#' @export

dijkstra_r <- function(edgelist, start, max_aux=Inf, stop_type=2, verbose=F){
  
  #create vector that contains unvisited nodes
  unvisited <- unique(edgelist$from)
  
  #create data.frame with shortest distances from the initial node
  nl <- data.frame(
    vertex = unvisited, 
    dist = rep(Inf, length(unvisited)),
    aux = rep(Inf, length(unvisited)),
    flag = rep(0, length(unvisited)))
  
  # set start vertex distance to 0
  nl$dist[nl$vertex == start] <- 0
  nl$aux[nl$vertex == start] <- 0
  
  # set step count to 1
  step = 1
  
  # continue until all vertices have been visited
  while (length(unvisited) > 0){
    
    # filter node list to unvisited nodes
    nl_s <- nl[nl$flag == 0,]
    
    # find vertex with the shortest distance from start, name v
    v <- nl_s$vertex[which.min(nl_s$dist)]
    
    # if aux distance of v exceeds limit, flag as -1, set to visited, skip cycle
    if(stop_type != 1){
      if(nl_s$aux[which.min(nl_s$dist)] > max_aux){
        nl$flag[nl$vertex == v] <- -1
        unvisited <- unvisited[-which(unvisited == v)]
        next
      }
    }
    
    # identify neighbors of current vertex
    neighbors <- edgelist$to[edgelist$from == v]
    
    # for each neighbor n
    for (n in neighbors){
      
      # browser()
      # distance estimate from neighbor to start
      dist_to_n <- nl$dist[nl$vertex == n]
      
      # step distances
      step_dist_n <- edgelist$dist[edgelist$from == v & edgelist$to == n]
      step_aux_n <- edgelist$aux[edgelist$from == v & edgelist$to == n]
      
      # calculate distance going through current vertex
      alt_dist_to_n <- nl$dist[nl$vertex == v] + step_dist_n
      
      # update distance to start if shorter
      if (alt_dist_to_n < dist_to_n){
        nl$dist[nl$vertex == n] <- alt_dist_to_n
        nl$aux[nl$vertex == n] <- nl$aux[nl$vertex == v] + step_aux_n
      }
    }
    
    # flag current vertex as visited; remove from unvisited list
    nl$flag[nl$vertex == v] <- step
    unvisited <- unvisited[-which(unvisited == v)]
    
    # early stop check
    if(stop_type == 1){
      if(nl$aux[nl$flag == step] > max_aux){
        break
      }
    }
    
    # report intermediate data at step if verbose
    if(verbose){
      cat(paste0('\nStep ', step, '\n'))
      print(nl)
    }
    
    # increment step
    step <- step + 1
  }
  
  return(nl)
}
