library(patchmax)
library(dplyr)

# define elements to look like those seen within forsys
stands_available = patchmax::test_forest |> mutate(weightedPriority = p4)
stand_id_field = 'id'
stand_area_field = 'ha'
patchmax_proj_size = 1000
patchmax_proj_size_min = 500
patchmax_proj_number = 3
stand_threshold = "p3 >= 0.5"
patchmax_SDW = 0.5
constraint_field = 'p1'
proj_target_value = Inf
proj_target_min_value = -Inf


# run patchmax
plan(multisession(workers = 8))
patchmax_out <- patchmax::simulate_projects(
  geom = stands_available,
  St_id = stands_available |> dplyr::pull(!!stand_id_field), 
  St_area = stands_available |> dplyr::pull(!!stand_area_field), 
  St_objective = stands_available |> dplyr::pull(weightedPriority), 
  # St_seed = patchmax_st_seed,
  P_size = patchmax_proj_size, 
  P_size_min  = patchmax_proj_size_min, 
  P_number = patchmax_proj_number,
  St_threshold = stand_threshold,
  SDW = patchmax_SDW,
  P_constraint = stands_available |> dplyr::pull(!!constraint_field),
  P_constraint_max_value = proj_target_value,
  P_constraint_min_value = proj_target_min_value
)

patchmax_out


# List of 2
# $ :'data.frame':	3 obs. of  5 variables:
#   ..$ Project   : num [1:3] 1 2 3
# ..$ Area      : num [1:3] 1000 1000 1000
# ..$ TotalArea : num [1:3] 1000 1000 1000
# ..$ Objective : num [1:3] 8.7 8.41 8.53
# ..$ Constraint: num [1:3] 7.43 3.96 9.64
# $ :'data.frame':	30 obs. of  6 variables:
#   ..$ Project   : num [1:30] 1 1 1 1 1 1 1 1 1 1 ...
# ..$ Stands    : chr [1:30] "3489" "3490" "3389" "3589" ...
# ..$ DoTreat   : num [1:30] 1 1 1 1 1 1 1 1 1 1 ...
# ..$ Area      : num [1:30] 100 100 100 100 100 100 100 100 100 100 ...
# ..$ Objective : num [1:30] 0.89 0.95 0.93 0.83 0.81 0.96 0.9 0.9 0.75 0.78 ...
# ..$ Constraint: num [1:30] 0.75 0.74 0.73 0.76 0.76 0.73 0.74 0.75 0.75 0.72 ...

#' patchmax outputs: list
#' --- 1
#'    Project = patch_id, 
#'    Area = area, 
#'    TotalArea = coverage, 
#'    Objective = objective, 
#'    Constraint = constraint
#'    Type = 0
#' -- 1
#'     Project = patch_id, 
#'     Stands = node, 
#'     DoTreat = include 
#'     Area = area, 
#'     Objective = objective, 
#'     Constraint = constraint