#' Simulate landscape projects
#' @param geom sf geometry representing stands
#' @param St_id Numeric vector of integer stands IDs
#' @param St_area Numeric vector of stands area
#' @param St_objective Numeric vector of stands objective
#' @param St_seed Numeric vector of stands IDs seeds. If NULL, then stand seed is not applied.
#' @param P_size Project size
#' @param P_
#' @param P_number Number of projects to simulate
#' @param St_threshold Threshold statement
#' @param SDW Stand distance weight parameter. If NULL, then the value = 1 is used by default.
#' @param P_constraint Numeric vector of stands value for the project constraint.Coupled with P_constraint_max_value and P_constraint_min_value. If NULL, then project constraint is not applied.
#' @param P_constraint_max_value Project constraint upper value.
#' @param P_constraint_min_value Project constraint lower value.
#' @return matrix containing stand id, stand count, objective score, and type where type = 0 are valid non-contained projects, type = 1 are valid constrained projects, type = 2 are invalid constrained projects, and type == 3 are invalid and non-constrained
#' @export

simulate_projects <- function(
    geom,                             # new param
    St_id,                            # add to geom
    St_area,                          # add to geom 
    St_objective,                     # add to geom 
    St_seed = NULL,                   # TODO
    P_size,                           # pass
    P_size_slack = 0.05,              # mutate to min project area
    P_number = 1,                     # pass
    St_threshold = NULL,              # takes threshold statement as input    
    SDW = NULL,                       # pass
    P_constraint = NULL,              # add to geom 
    P_constraint_max_value = Inf,     # pass
    P_constraint_min_value = -Inf     # pass
){
  
  geom_s <- geom
  geom_s$Id = St_id
  geom_s$Area = St_area
  geom_s$Objective = St_objective
  geom_s$Constraint = P_constraint
  
  pm <- patchmax$new(geom_s, 'Id', 'Objective', 'Area', P_size)
  pm$params <- list(
    area_min = P_size - (P_size * P_size_slack),
    sdw = SDW,
    threshold = St_threshold,
    constraint_field = 'Constraint', 
    constraint_max = P_constraint_max_value,
    constraint_min = P_constraint_min_value)
  
  pm$simulate(P_number)
  
  out_a <- pm$patch_stats %>% select(
    Project = patch_id,
    Area = area,
    TotalArea = coverage,
    Objective = objective,
    Constraint = constraint
  )
  
  out_b <- pm$patch_stands %>% select(
    Project = patch_id,
    Stands = node,
    DoTreat = include,
    Area = area,
    Objective = objective,
    Constraint = constraint
  )
  
  out <- list(out_a, out_b)
  return(out)
  
  #' patchmax outputs: list
  #' [[1]]
  #'    Project = patch_id, 
  #'    Area = area, 
  #'    TotalArea = coverage, 
  #'    Objective = objective, 
  #'    Constraint = constraint
  #'    Type = 0
  #' [[2]] 
  #'     Project = patch_id, 
  #'     Stands = node, 
  #'     DoTreat = include 
  #'     Area = area, 
  #'     Objective = objective, 
  #'     Constraint = constraint
}
