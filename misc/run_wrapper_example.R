# define elements to look like those seen within forsys
stands_available = patchmax::test_forest %>% mutate(weightedPriority = p4)
stand_id_field = 'id'
stand_area_field = 'ha'
patchmax_proj_size = 1000
patchmax_proj_size_slack = 0.1
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
  St_id = stands_available %>% dplyr::pull(!!stand_id_field), 
  St_area = stands_available %>% dplyr::pull(!!stand_area_field), 
  St_objective = stands_available %>% dplyr::pull(weightedPriority), 
  # St_seed = patchmax_st_seed,
  P_size = patchmax_proj_size, 
  P_size_slack  = patchmax_proj_size_slack, 
  P_number = patchmax_proj_number,
  St_threshold = stand_threshold,
  SDW = patchmax_SDW,
  P_constraint = stands_available %>% dplyr::pull(!!constraint_field),
  P_constraint_max_value = proj_target_value,
  P_constraint_min_value = proj_target_min_value
)
