#' Evaluate BFS, basic
#' @param r Stand root
#' @return
#' @keywords internal
#' @export

runbfs <- function(r) {
  runbfs_func(r)
}

#' Internal Evaluate BFS
#' @param r Stand root
#' @keywords internal

runbfs_func <- function(r) {
  
  tryCatch({
    # order stands by breadth first distance search
    BFS <- igraph::bfs(St_adj, root = r, unreachable = FALSE)
    BFS_Stands <- as.numeric(BFS$order[!is.na(BFS$order)]$name)
    BFS_Stands2 <- match(BFS_Stands, St_id)
    
    if(!is.null(St_distances) & length(BFS_Stands2) > 1){
      # order projects by distance
      SDW_objective <- St_distances[BFS_Stands2,BFS_Stands2[1]]
      SDW_objective_sorted <- c(SDW_objective[1],sort(SDW_objective[2:length(SDW_objective)], decreasing = TRUE))
      BFS_Stands2 <- as.numeric(names(SDW_objective_sorted))
    }
    
    # calculate cumulative sum of area in order of stand rank
    Areas <- cumsum(St_area[BFS_Stands2])
    Areas_table <- data.table::data.table(Csum = Areas, val = Areas)
    setattr(Areas_table, "sorted", "Csum")
    limit_position <- Areas_table[J(P_size), roll = "nearest", which = TRUE]
    contad <- limit_position
    Invalid <- NULL
    
    # exit if outside area slack limit
    if (Areas[limit_position] > P_size * (1 + P_size_slack)) {
      return ()
    }
    
    # identifying last position in project stand order where cumulative constraint 
    # is greater than the min and less than the max. Replace limit position with last 
    # position where both are true.
    if (!is.null(P_constraint)) {
      Constraint <- cumsum(P_constraint[BFS_Stands2[1:limit_position]])
      Constraint_table <- data.table::data.table(Csum = Constraint, step1 = 0, step2 = 0)
      Constraint_table[, step1 := c(Constraint_table$Csum > P_constraint_min_value)]
      Constraint_table[, step2 := c(Constraint_table$Csum < P_constraint_max_value)]
      f_position <-  tail(which(Constraint_table$step1 == TRUE &  Constraint_table$step2 == TRUE),1)
      if(length(f_position) == 0){
        Invalid <- 1
      } else if (Areas[f_position] > Candidate_min_size){
        limit_position <- f_position
      } else {
        return ()
      }
    }
    
    # record stand project stats
    Stands_block <- BFS_Stands2[1:limit_position]
    Block_area <- sum(St_area[Stands_block])
    N_vertices <- length(Stands_block)
    
    # tally objective 
    if(!is.null(St_distances) & length(Stands_block) > 1){
      Block_Objective <- sum(SDW_objective_sorted[1:limit_position])
    } else {
      Block_Objective <- sum(St_objective[Stands_block])
    }
    
    # constraint types:
    # 0: within area slack
    # 1: within area slick with constraint
    # 2: outside area slack due to constraint
    # 3: outside area slack
    
    # assign type constraints if no secondary constraint
    if (is.null(P_constraint)) {
      type_constraint <- 0 # valid & non-constrained
      # if min slack is less than project area and max slack is greater than project area
      if (P_size * ((P_size_slack - 1) * -1) < Block_area & P_size * (1 + P_size_slack) > Block_area) {
        return(list(r, N_vertices, Block_Objective,type_constraint))
      } else if (Block_area > Candidate_min_size) {
        type_constraint <- 3 # invalid & non-constrained
        return(list(r, N_vertices, Block_Objective,type_constraint))
      } else {
        return ()
      }
    }
    
    if (!is.null(Invalid)) {
      if (Block_area > Candidate_min_size){
        type_constraint <- 3 # invalid & non-constrained
        return(list(r, N_vertices, Block_Objective, type_constraint))
      } else {
        return ()
      }
    }
    
    # return project output if secondary constraint exists
    if (!is.null(P_constraint)) {
      if (P_size * ((P_size_slack - 1) * -1) < Block_area & P_size * (1 + P_size_slack) > Block_area) {
        type_constraint <- 1 # valid & constrained
        return(list(r, N_vertices, Block_Objective, type_constraint))
      } else {
        type_constraint <- 2 # invalid & constrained
        return(list(r, N_vertices, Block_Objective, type_constraint))
      }
    }
  }, error = function(e){
    cat(r)
  })
  
}

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
#'
#' @importFrom igraph bfs
#' @importFrom igraph delete.vertices
#' @importFrom igraph V
#'
#' @importFrom data.table data.table
#' @importFrom data.table setattr
#'
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel clusterExport
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#'

simulate_projects <- function(
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
  
  simulate_projects_func(
    St_id = St_id, 
    St_adj = St_adj, 
    St_area = St_area,
    St_objective = St_objective, 
    St_seed = St_seed,
    P_size = P_size, 
    P_size_slack = P_size_slack, 
    P_size_ceiling = P_size_ceiling,
    P_number = P_number,
    St_threshold = St_threshold, 
    St_threshold_value = St_threshold_value,
    St_distances = St_distances,
    SDW = SDW,
    P_constraint = P_constraint, 
    P_constraint_max_value = P_constraint_max_value, 
    P_constraint_min_value = P_constraint_min_value, 
    Candidate_min_size = Candidate_min_size
    )
  
}

#' Internal simulate landscape projects function
#' @keywords internal
simulate_projects_func <- function(
  St_id, 
  St_adj, 
  St_area, 
  St_objective, 
  St_seed,
  P_size, 
  P_size_slack, 
  P_size_ceiling,
  P_number,
  St_threshold, 
  St_threshold_value, 
  St_distances,
  SDW,
  P_constraint, 
  P_constraint_max_value, 
  P_constraint_min_value, 
  Candidate_min_size
) {
  
  # require(igraph)
  
  St_area2 <- St_area
  St_objective2 <- St_objective
  P_constraint2 <- P_constraint
  
  if(is.null(P_size_slack)){P_size_slack = 0}
  
  if(is.null(St_seed)){St_seed = St_id}
  
  # 1. set P_size_slack to 0 if null; 
  # 2. set St_seed to all St_id if null;
  # 3. zero out area, objective & constraint where stand less than threshold;
  # 4. set min project size to 25% desire if null;
  # 5. while projects are less than max
  #     a. search for best project
  #     b. build best project
  #     c. tally project area, objective, 
  
  if (!is.null(St_threshold_value)) {
    St_area[which(St_threshold < St_threshold_value)] <- 0
    St_objective[which(St_threshold < St_threshold_value)] <- 0
    if(!is.null(P_constraint)){
      P_constraint[which(St_threshold < St_threshold_value)] <- 0
    }
    St_threshold[which(St_threshold < St_threshold_value)] <- 0
  }
  
  if(is.null(Candidate_min_size)){Candidate_min_size = 0.25*P_size}
  
  if(!is.null(St_distances)){
    if(is.null(SDW)){
      SDW <- 1
    }
    St_distances <- St_objective + (1-St_distances)*SDW
    diag(St_distances) <- 0
  }
  
  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  
  no_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(getOption("cl.cores", no_cores))
  cat(paste0('Running PatchMax using ', no_cores, ' cores\n'))
  
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(
    cl = cl,
    varlist = c(
      "runbfs_func",
      "St_id",
      "St_adj",
      "St_area",
      "St_area2",
      "St_objective",
      "St_objective2",
      "P_size",
      "P_size_slack",
      "P_number",
      "St_threshold",
      "St_threshold_value",
      "P_constraint",
      "St_distances",
      "P_constraint_max_value",
      "P_constraint_min_value",
      "Candidate_min_size",
      "bfs",
      "data.table",
      "setattr",
      "V"
    ),
    envir = environment()
  )
  
  Blocks_table <- data.table::data.table()
  Stands_table <- data.table::data.table()
  
  b = 1
  s = 0
  
  # 
  while(b <= P_number & s <= P_size_ceiling){
    
    cat(paste0("\nProject #", b, '\n'))
    
    # search for best project among all remaining nodes
    if(b == 1){
      Seeds <- match(St_seed, as.numeric(V(St_adj)$name))
      Seeds <- Seeds[!is.na(Seeds)]
      result <- pbapply::pblapply(Seeds, runbfs, cl=cl)
    } else {
      # if not first project example feasible nodes
      result <- pbapply::pblapply(feasible_positions, runbfs, cl=cl)
    }
    
    # if valid result 
    if (!is.null(unlist(result))){
      
      output <- data.table::as.data.table(matrix(unlist(result), ncol = 4, byrow = TRUE))
      
      feasible_seeds <- output$V1
      feasible_vertices <- V(St_adj)[feasible_seeds]

      output2 <- subset(output, V4 %in% c(0,1,2))
      output3 <- subset(output, V4 %in% c(3))
      if(nrow(output2) >= 1) {
        output <- output2
      } else{
        output <- output3
      }
      
      # build project for best candidate
      best <- head(output[V3 == max(V3)], 1)
      best_r <- best$V1
      
      BFS <- igraph::bfs(St_adj, root = best_r, unreachable = FALSE)
      BFS_Stands <- as.numeric(BFS$order[!is.na(BFS$order)]$name)
      BFS_Stands2 <- match(BFS_Stands, St_id)
      
      if(!is.null(St_distances)){
        SDW_objective <- St_distances[BFS_Stands2,BFS_Stands2[1]]
        SDW_objective_sorted <- c(SDW_objective[1],sort(SDW_objective[2:length(SDW_objective)], decreasing = TRUE))
        BFS_Stands2 <- as.numeric(names(SDW_objective_sorted))
      }
      
      # tally stats for treated stands
      Stands_block <- BFS_Stands2[1:best$V2]
      Stands_treat <- BFS_Stands2[1:best$V2]
      
      Stands_ID <- St_id[Stands_block]
      Positions_BFS <- match(Stands_ID,BFS_Stands)
      Stands_area <- St_area2[Stands_block]
      Total_block_area <- sum(Stands_area)
      Block_area <- sum(St_area[Stands_block])
      
      Stands_Objective <- St_objective2[Stands_block]
      Block_Objective <- sum(St_objective[Stands_block])
      
      Stands_treat[which(St_area[Stands_treat] %in% 0)] <- 0
      Stands_treat[which(Stands_treat != 0)] <- 1
      
      Project_type <- best$V4
      
      Block_constraint <- NULL
      Stands_constraint <- NULL
      
      if (!is.null(P_constraint)) {
        Stands_constraint <-  P_constraint2[Stands_block]
        Block_constraint <- sum(P_constraint[Stands_block])
      }
      
      #here you have to define outputs with project constraint when P_constraint !is.null
      Stands_table2 <- data.table::data.table(Project = b, 
                                              Stands = Stands_ID, 
                                              DoTreat = Stands_treat, 
                                              Area = Stands_area, 
                                              Objective = Stands_Objective,
                                              Constraint = Stands_constraint)
      
      Stands_table <- rbind(Stands_table, Stands_table2)
      
      Blocks_table2 <- data.table::data.table(Project = b, 
                                              Area = Block_area, 
                                              TotalArea = Total_block_area,
                                              Objective = Block_Objective, 
                                              Constraint = Block_constraint,
                                              Type = Project_type)
      
      Blocks_table <- rbind(Blocks_table, Blocks_table2)
      
      # report key stats
      cat(paste0("  treated area: ", round(Block_area, 2),
                 "; total selected area:", round(Total_block_area, 2),
                 "; objective value: ", round(Block_Objective, 2),
                 "; constraint: ", Block_constraint,
                 "; project type: ",best$V4))
      
      # delete selects stands from adjacency graph
      St_adj <- igraph::delete.vertices(St_adj, BFS$order[1:best$V2])
      
      # eliminate unfeasible candidates
      feasible_vertices2 <- feasible_vertices[!feasible_vertices %in% BFS$order[Positions_BFS]]
      feasible_positions <- match(as.numeric(feasible_vertices2$name), as.numeric(V(St_adj)$name))
      
      # update cluster environment
      parallel::clusterExport(cl = cl, 
                              varlist = c("St_adj","feasible_positions"), 
                              envir = environment())
      
      b = b + 1
      s = s + Block_area
      
    } else {
      print(paste("There is no feasible projects"))
      break
    }
    
    if(b > P_number) message('\nProject count reached')
    
    if(s >= P_size_ceiling) message('\nProject size ceiling reached')
    
  } # end for loop
  
  parallel::stopCluster(cl = cl)
  results <- list(Blocks_table, Stands_table)
  return(results)
}
