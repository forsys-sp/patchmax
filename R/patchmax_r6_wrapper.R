#' Simulate landscape projects (wrapper)
#'
#' @param geom sf. Geometry representing stands
#' @param St_id vector. stands IDs. Converted to character
#' @param St_area numeric vector. Stands area
#' @param St_objective numeric vector. Stands objective values
#' @param St_seed numeric vector of stands IDs seeds. 
#' @param P_size numeric. Project size
#' @param P_size_slack numeric fraction 0-1. Minimum size defined as percent
#'   less than project size (only used when constraint is provided)
#' @param P_number number integer. Number of patches to simulate
#' @param St_threshold character threshold statement (e.g., 'field > 0.5' or 'variable == 1')
#' @param SDW numeric fraction 0-1. Stand distance weight parameter. Default is 0.5
#' @param P_constraint numeric vector. Stands values for the project
#'   constraint.Coupled with P_constraint_max_value and P_constraint_min_value.
#'   If NULL, then project constraint is not applied.
#' @param P_constraint_max_value numeric. Project constraint upper value. Default is -Inf
#' @param P_constraint_min_value numeric. Project constraint lower value. Default is Inf
#' @param sample_frac
#'
#' @return list with first element describing patch-level stats and the second
#'   element secribing stand-level stats.
#'
#' @details This function is meant as a API for calling patchmax within ForSys
#'   (or another environment) using a minimal set of parameters.
#'   
#' @export

simulate_projects <- function(
    geom,                             # REQ
    St_id,                            # REQ
    St_area,                          # REQ
    St_objective,                     # REQ
    St_seed = NULL,                   # TODO
    P_size,                           # REQ
    P_size_slack = 0.05,
    P_number = 1,                     # REQ
    St_threshold = NULL, 
    SDW = NULL,
    P_constraint = NULL, 
    P_constraint_max_value = Inf,
    P_constraint_min_value = -Inf,
    sample_frac = 0.1
){
  
  geom$Id = St_id
  geom$Area = St_area
  geom$Objective = St_objective
  geom$Constraint = P_constraint

  pm <- patchmax$new(geom, 'Id', 'Objective', 'Area', P_size)
  pm$params <- list(
    area_min = P_size - (P_size * P_size_slack),
    sdw = SDW,
    threshold = St_threshold,
    constraint_field = 'Constraint', 
    constraint_max = P_constraint_max_value,
    constraint_min = P_constraint_min_value)
  
  for(i in 1:P_number){
    pm$search(sample_frac = sample_frac, show_progress = T)$build()$record() 
  }
  
  out_a <- pm$patch_stats %>% select(
    Project = patch_id,
    Area = area,
    TotalArea = coverage,
    Objective = objective,
    Constraint = constraint
  )
  
  out_b <- pm$patch_stands %>% select(
    Project = patch_id,
    Stands = 2,
    DoTreat = include,
    Area = area,
    Objective = objective,
    Constraint = constraint
  )
  
  out <- list(out_a, out_b)
  return(out)
}
