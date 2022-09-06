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

runbfs_func <- function(r) {

  tryCatch({
    BFS <- igraph::bfs(St_adj, root = r, unreachable = FALSE)
    BFS_Stands <- as.numeric(BFS$order[!is.na(BFS$order)]$name)
    BFS_Stands2 <- match(BFS_Stands, St_id)
    Areas <- cumsum(St_area[BFS_Stands2])
  
    Areas_table <- data.table::data.table(Csum = Areas, val = Areas)
    setattr(Areas_table, "sorted", "Csum")
    limit_position <- Areas_table[J(P_size), roll = "nearest", which = TRUE]
    contad <- limit_position
    Invalid <- NULL
  
    if (Areas[limit_position] > P_size * (1 + P_size_slack)) {
        return ()
    }
  
    # if constraint is found
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
  
    Stands_block <- BFS_Stands2[1:limit_position]
    Block_area <- sum(St_area[Stands_block])
    N_vertices <- length(Stands_block)
  #
    if (is.null(P_constraint)) {
      type_constraint <- 0
      if (P_size * ((P_size_slack - 1)*-1) < Block_area & P_size * (1 + P_size_slack) > Block_area) {
        Block_NetRevenue <- sum(St_objective[Stands_block])
        return(list(r, N_vertices, Block_NetRevenue,type_constraint))
      } else if (Block_area > Candidate_min_size) {
      type_constraint <- 3
        Block_NetRevenue <- sum(St_objective[Stands_block])
        return(list(r, N_vertices, Block_NetRevenue,type_constraint))
      } else {
        return ()
      }
    }
  #
    if (!is.null(Invalid)) {
      if (Block_area > Candidate_min_size){
      type_constraint <- 3
        Block_NetRevenue <- sum(St_objective[Stands_block])
        return(list(r, N_vertices, Block_NetRevenue,type_constraint))
      } else {
        return ()
      }
    }
  
    #
    if (!is.null(P_constraint)) {
      if (P_size * ((P_size_slack - 1)*-1) < Block_area & P_size * (1 + P_size_slack) > Block_area) {
       type_constraint <- 1
        Block_NetRevenue <- sum(St_objective[Stands_block])
        return(list(r, N_vertices, Block_NetRevenue,type_constraint))
      } else {
       type_constraint <- 2
        Block_NetRevenue <- sum(St_objective[Stands_block])
        return(list(r, N_vertices, Block_NetRevenue,type_constraint))
      }
    }
  }, error = function(e){
    cat(r)
  })

}

#' Simulate landscape projects
#'
#' @param St_id Numeric vector of integer stands IDs
#' @param St_adj Adjacency input graph created through calculate_adj or read_adj functions. The graph vertices must be named.
#' @param St_area Numeric vector of stands area
#' @param St_objective Numeric vector of stands objective
#' @param P_size Project size
#' @param P_size_slack Project size slack, between 0 and 1, Where 0 means no deviation.
#' @param P_number Number of projects to simulate
#' @param St_threshold Numeric vector of stands threshold value.Coupled with St_threshold_value. If NULL, then stand threshold is not applied.
#' @param St_threshold_value Stands threshold lower value.
#' @param P_constraint Numeric vector of stands value for the project constraint.Coupled with P_constraint_max_value and P_constraint_min_value. If NULL, then project constraint is not applied.
#' @param P_constraint_max_value Project constraint upper value.
#' @param P_constraint_min_value Project constraint lower value.
#' @param Candidate_min_size Minimal size for project coded with types 2 and 3. If NULL, then the value = ‘0.25*P_size’ is used by default. Project type codes interpret if the project is valid (i.e. if P_size apply), constrained (i.e. if P_constraint apply) or none of them. Where type 0 are valid non-constrained projects (i.e. only P_size apply), type 1 are valid constrained projects (i.e. P_size and P_constraint apply), type 2 are invalid constrained projects (i.e. P_size does not apply but P_constraint apply) and type 3 are non-optimized projects that can be invalid and/or non-constrained (i.e. P_size and/or P_constraint do not apply).
#'
#' @return
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
  P_size, 
  P_size_slack = 0.05, 
  P_size_ceiling = Inf,
  P_number = 1,
  St_threshold = NULL, 
  St_threshold_value = NULL,
  P_constraint = NULL, 
  P_constraint_max_value = NULL, 
  P_constraint_min_value = NULL, 
  Candidate_min_size = NULL,
  Sample_n = NULL,
  Sample_seed = NULL
  ){

  simulate_projects_func(
    St_id = St_id, 
    St_adj = St_adj, 
    St_area = St_area, 
    St_objective = St_objective, 
    P_size = P_size, 
    P_size_slack = P_size_slack, 
    P_size_ceiling = P_size_ceiling,
    P_number = P_number,
    St_threshold = St_threshold, 
    St_threshold_value = St_threshold_value,
    P_constraint = P_constraint, 
    P_constraint_max_value = P_constraint_max_value, 
    P_constraint_min_value = P_constraint_min_value, 
    Candidate_min_size = Candidate_min_size,
    Sample_n = Sample_n,
    Sample_seed = Sample_seed
    )

}

#' Internal simulate landscape projects function

simulate_projects_func <- function(
  St_id, 
  St_adj, 
  St_area, 
  St_objective, 
  P_size, 
  P_size_slack, 
  P_size_ceiling,
  P_number,
  St_threshold, 
  St_threshold_value, 
  P_constraint, 
  P_constraint_max_value, 
  P_constraint_min_value, 
  Candidate_min_size,
  Sample_n,
  Sample_seed
  ) {
  
  # require(igraph)
  
  St_area2 <- St_area
  St_objective2 <- St_objective
  P_constraint2 <- P_constraint

  if(is.null(P_size_slack)){P_size_slack = 0}

  if (!is.null(St_threshold_value)) {
    St_area[which(St_threshold < St_threshold_value)] <- 0
    St_objective[which(St_threshold < St_threshold_value)] <- 0
    if(!is.null(P_constraint)){
      P_constraint[which(St_threshold < St_threshold_value)] <- 0
    }
    St_threshold[which(St_threshold < St_threshold_value)] <- 0
  }

  if(is.null(Candidate_min_size)){Candidate_min_size = 0.25*P_size}

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
  
  # for (b in 1:P_number) { # for b in P_number of projects
  while(b <= P_number & s <= P_size_ceiling){
  
    cat(paste0("\nProject #", b, '\n'))
    
    #####Eliminate unfeasible candidates
    if(b == 1){
      Vertices <- igraph::V(St_adj)
      
      if(!is.null(Sample_n)){
        if(!is.null(Sample_seed)) set.seed(Sample_seed)
        Vertices <- sample(Vertices, size = Sample_n)
      }
      # for(i in Vertices){
      #   runbfs(i)
      # }
      result <- pbapply::pblapply(Vertices, runbfs, cl=cl)
    } else {
      result <- pbapply::pblapply(feasible_positions, runbfs, cl=cl)
    }
    #####Eliminate seeds
    #result <- parallel::parLapply(cl, sample(1:length(Vertices), size = length(as.numeric(Vertices))/2), runbfs)
    #####
    if (!is.null(unlist(result))){
  
      parallel::clusterExport(cl = cl, varlist = c("result"), envir = environment())
  
      output <- data.table::as.data.table(matrix(unlist(result), ncol = 4, byrow = TRUE))
  
      output2 <- subset(output, V4 %in% c(0,1,2))
      output3 <- subset(output, V4 %in% c(3))
      if(nrow(output2) >= 1) {
        output <- output2
      } else{
        output <- output3
      }
  
      best <- head(output[V3 == max(V3)], 1)
      best_r <- best$V1
  
      #####Eliminate unfeasible candidates
      if(b == 1){
        feasible_seeds <- output$V1
        feasible_vertices <- V(St_adj)[feasible_seeds]
      }
      ##########
  
      BFS <- igraph::bfs(St_adj, root = best_r, unreachable = FALSE)
      BFS_Stands <- as.numeric(BFS$order[!is.na(BFS$order)]$name)
  
      BFS_Stands2 <- match(BFS_Stands, St_id)
  
      Stands_block <- BFS_Stands2[1:best$V2]
      Stands_treat <- BFS_Stands2[1:best$V2]
  
      Stands_ID <- St_id[Stands_block]
  
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
  
      cat(paste0("  treated area: ", round(Block_area, 2),
                 "; total selected area:", round(Total_block_area, 2),
                "; objective value: ", round(Block_Objective, 2),
                "; constraint: ", Block_constraint,
                "; project type: ",best$V4))
        
      St_adj <- igraph::delete.vertices(St_adj, BFS$order[1:best$V2])
  
      #####Eliminate unfeasible candidates
      feasible_vertices2 <- feasible_vertices[!feasible_vertices %in% BFS$order[1:best$V2]]
      feasible_positions <- which(V(St_adj) %in% feasible_vertices2)
      ##########
      
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