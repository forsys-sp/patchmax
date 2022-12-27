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
