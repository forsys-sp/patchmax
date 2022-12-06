# scenario object generator (read this into memory)
patchmax_generator <- R6Class(
  classname = "patch_generator",
  
  # //////////////////////////////////////////////////////////////////////////
  # PRIVATE ELEMENTS
  # internal aspects of the patchmax generator
  # //////////////////////////////////////////////////////////////////////////  
  private = list(
    ..net = NULL,
    ..geom = NULL,
    ..cpp_graph = NULL,
    ..param_id_field = NULL,
    ..param_objective_field = NULL,
    ..param_area_field = NULL,
    ..param_patch_area = NULL,
    ..param_availability = NULL,
    ..param_availability_area_adjust = 0,
    ..param_availability_objective_adjust = 0,
    ..param_constraint_field = NULL,
    ..param_constraint_max = Inf,
    ..param_constraint_min = -Inf,
    ..param_sdw = 1,
    ..param_epw = 1,
    ..patch_mem = NULL,
    ..best_mem = NULL,

    # build cpp graph object     
    ..update_dist = function(){
      cpp_graph <- build_graph_func(
        net = delete_vertices(private$..net, V(private$..net)$patch_id > 0),
        objective_field = private$..param_objective_field, 
        sdw = private$..param_sdw, 
        epw = private$..param_epw)
      return(cpp_graph)
    },
    
    # update adjacency network
    ..update_net = function(){
      
      net = private$..net
      
      # modify area and distance based on availability 
      net <- set_threshold_func(
        net = net, 
        include = V(net)$available, 
        area_adjust = private$..param_availability_area_adjust, 
        objective_adjust = private$..param_availability_objective_adjust)
      
      # remove stands assigned a patch id
      net <- delete_vertices(net, V(net)$patch_id > 0)
      return(net)
    },
    
    # set availability provided by the user
    ..set_availability = function(){
      if(!is.null(private$..param_availability)){
        net <- private$..net
        s_txt = private$..param_availability
        id = private$..param_id_field
        all_ids = pull(vertex_attr(net), id)        
        available_ids = subset(vertex_attr(net), eval(parse(text = s_txt))) %>% pull(id)
        V(private$..net)$available = ifelse(all_ids %in% available_ids, 1, 0)
      } else
        V(private$..net)$available = 1
    },

    # check that required fields are provided
    ..check_req_fields = function(){
      if(is.null(private$..param_id_field))
        stop('id field is missing')
      if(is.null(private$..param_objective_field))
        stop('objective field is missing')
      if(is.null(private$..param_patch_area))
        stop('patch area is missing')
      if(is.null(private$..param_area_field))
        stop('area field is missing')
    },

    # (re)assign objective, area, and constraints to indicated fields
    ..refresh_net_attr = function(){
      if(!is.null(private$..param_objective_field)){
        vertex_attr(private$..net, name = 'objective') <- 
          vertex_attr(private$..net, private$..param_objective_field)
      }
      if(!is.null(private$..param_area_field)){
        vertex_attr(private$..net, name = 'area') <- 
          vertex_attr(private$..net, private$..param_area_field)
      }
      if(!is.null(private$..param_constraint_field)){
        vertex_attr(private$..net, name = 'constraint') <- 
          vertex_attr(private$..net, private$..param_constraint_field)
      }
    }
  ),
  
  # //////////////////////////////////////////////////////////////////////////
  # PUBLIC ELEMENTS
  # external methods for running patchmax
  # //////////////////////////////////////////////////////////////////////////
  
  public = list(
    
    # INITIALIZE METHOD --------------------------------------------------------
    initialize = function(geom, id_field=NULL, objective_field=NULL, area_field=NULL, patch_area=NULL){
      if(!missing(geom)){
        
        # save geometry
        private$..geom <- geom
        
        # parameters
        private$..param_id_field = id_field
        private$..param_objective_field = objective_field
        private$..param_area_field = area_field
        private$..param_patch_area = patch_area 
        
        # setup key fields in geometry object
        private$..geom[,id_field] = as.character(dplyr::pull(private$..geom, id_field))
        private$..geom$patch_id = 0
        private$..geom$available = 1
        
        # build adjacency network
        private$..net <- calc_adj_network_func(
          shp = geom, 
          St_id = dplyr::pull(geom, id_field), 
          calc_dist = TRUE)
        
        # add addition fields to adjacency network
        vertex_attr(private$..net) <- bind_cols(vertex_attr(private$..net), st_drop_geometry(geom))
        vertex_attr(private$..net, name = 'exclude') = 0
        vertex_attr(private$..net, name = 'patch_id') = 0
        vertex_attr(private$..net, name = 'constraint') = 1
        vertex_attr(private$..net, name = 'available') = 1
        
        private$..refresh_net_attr()
        
      } else {
        stop('Geometry required')
      }
    },
    
    # BUILD METHOD --------------------------------------------------------
    
    build = function(node=NULL){
      
      if(is.null(node)){
        node <- private$..best_mem
      }
      
      private$..check_req_fields()
      
      patch <- build_patch_func(
        cpp_graph = private$..update_dist(), 
        net = private$..update_net(),
        start_node = node, 
        patch_area = private$..param_patch_area)
      
      patch_dat <- vertex_attr(private$..net) %>% 
        dplyr::select(node = name, 
                      private$..param_objective_field, 
                      available)
      
      patch <- dplyr::left_join(patch, patch_dat, by='node')
      private$..patch_mem <- patch
      
      message(glue::glue('Patch stats: start {private$..patch_mem$node[1]}, area {round(sum(private$..patch_mem$area),3)}, score {round(sum(private$..patch_mem$objective),3)}, distance {round(sum(private$..patch_mem$dist),3)}'))
      
      return(invisible(self))
    },
    
    # SEARCH METHOD --------------------------------------------------------
    
    search = function(sample_frac=0.1, return_all=FALSE, show_progress=FALSE, plot_search=FALSE){
      
      private$..check_req_fields()
      private$..update_dist()
      
      if(plot_search){
        return_all = TRUE
      }
      
      search_out <- search_best_func(
        cpp_graph = private$..update_dist(), 
        net = private$..update_net(), 
        objective_field = private$..param_objective_field, 
        patch_area = private$..param_patch_area,
        sample_frac = sample_frac,
        return_all = return_all,
        show_progress = show_progress)
      
      # plot search results (mainly for debugging)
      if(plot_search){
        pdat <- left_join(
          x = private$..geom, 
          y = data.frame(search_out) %>% tibble::rownames_to_column(), 
          by=c('stand_id'='rowname'))
        plot(pdat[,'search_out'], border=NA, key.pos = NULL)
      }
      
      best_out = names(search_out)[which.max(search_out)]
      message(glue::glue('\nBest start: {best_out} ({round(sample_frac*100)}% searched)'))
      private$..best_mem <- best_out
      
      if(return_all)
        private$..geom$search_score <- search_out
      
      return(invisible(self))
    },
    
    simulate = function(n_projects = 1){
      for(i in 1:n_projects){
        self$search()$build()$record(patch_id = i) 
      }
    },
    
    # PLOT METHOD --------------------------------------------------------
    
    plot = function(plot_field = NULL){
      
      if(is.null(private$..patch_mem)){
        message('No patch currently selected')
      } else {
        patch = private$..geom %>% 
          dplyr::select(private$..param_id_field) %>% 
          dplyr::rename(node = 1) %>% 
          inner_join(private$..patch_mem, by='node') %>%
          mutate(available = factor(available))
      }
      
      plot_field <- ifelse(is.null(plot_field),  private$..param_objective_field, plot_field)
      patches = private$..geom %>% group_by(patch_id) %>% summarize() %>% filter(patch_id != 0)
      
      plot = ggplot() + 
        geom_sf(data=geom, aes(fill=get(plot_field)), linewidth=0) + 
        scale_fill_gradientn(colors = sf.colors(10)) +
        guides(fill = guide_legend(plot_field)) +
        theme(legend.position = 'bottom') +
        theme_void() 
      
      if(!is.null(private$..patch_mem)){
        plot = plot + 
          geom_sf(data=patch, aes(color=available), fill=NA, linewidth=.5) +
          scale_color_manual(values = c('black','red'), breaks = c(1, 0))
      }
      
      if(nrow(patches) > 0){
        plot = plot +
          geom_sf(data=patches, fill=NA, linewidth=.5, color='black') +
          geom_sf_text(data=patches, aes(label=patch_id))
      }
      
      print(plot)
      
      return(invisible(self))
    },
    
    # RECORD METHOD --------------------------------------------------------

    record = function(patch_id = NULL){
      if(is.null(private$..patch_mem))
        stop('No patch. Run search or select first.')

      if(is.null(patch_id)){
        patch_id = max(V(private$..net)$patch_id) + 1
      }

      V(private$..net)$patch_id[match(private$..patch_mem$node, V(private$..net)$stand_id)] = patch_id
      private$..geom$patch_id[match(private$..patch_mem$node, private$..geom$stand_id)] = patch_id

      message(glue::glue('Patch {patch_id} recorded'))
      private$..patch_mem <- NULL
      
      return(invisible(self))
    },
    
    # DESCRIBE METHOD --------------------------------------------------------
    
    describe = function(){
      private$..geom %>% st_drop_geometry() %>% group_by(patch_id) %>% summarize_if(is.numeric, sum)
    },
    
    # RESET METHOD --------------------------------------------------------
    
    reset = function(){
      message('Selected patches have been deleted')
      private$..geom$patch_id = 0
      V(private$..net)$patch_id = 0
      private$..patch_mem = NULL
      private$..best_mem = NULL
    }
  ),
  
  # //////////////////////////////////////////////////////////////////////////
  # ACTIVE BINDINGS 
  # used for getting and setting private elements
  # //////////////////////////////////////////////////////////////////////////
  
  active = list(
    net = function(){
      private$..net
    },
    geom = function(){
      private$..geom
    },
    best = function(){
      private$..best_mem
    },
    patch = function(){
      if(is.null(private$..patch_mem)){
        message('No project selected. Please run pm$build()')
      } else {
        private$..patch_mem
      }
    },
    id_field = function(value){
      if(missing(value)){
        private$..param_id_field
      } else {
        private$..param_id_field <- value
        private$..refresh_net_attr()
      }
    },
    objective_field = function(value){
      if(missing(value)){
        private$..param_objective_field
      } else {
        private$..param_objective_field <- value
        private$..refresh_net_attr()
      }
    },
    area_field = function(value){
      if(missing(value)){
        private$..param_area_field
      } else {
        private$..param_area_field <- value
        private$..refresh_net_attr()
      }
    },
    constraint_field = function(value){
      if(missing(value)){
        private$..param_constraint_field
      } else {
        private$..param_constraint_field <- value
        private$..refresh_net_attr()
      }
    },
    availability = function(value){
      if(missing(value)){
        private$..param_availability
      } else {
        private$..param_availability <- value
        private$..refresh_net_attr()
        private$..set_availability()
      }
    },
    availability_area_adjust = function(value){
      if(missing(value)){
        private$..param_availability_area_adjust
      } else {
        private$..param_availability_area_adjust <- value
        private$..refresh_net_attr()
        private$..set_availability()
      }
    },
    availability_objective_adjust = function(value){
      if(missing(value)){
        private$..param_availability_objective_adjust
      } else {
        private$..param_availability_objective_adjust <- value
        private$..refresh_net_attr()
        private$..set_availability()
      }
    },
    constraint_max = function(value){
      if(missing(value)){
        private$..param_constraint_max
      } else {
        private$..param_constraint_max <- value
        private$..refresh_net_attr()
      }
    },
    constraint_min = function(value){
      if(missing(value)){
        private$..param_constraint_min
      } else {
        private$..param_constraint_min <- value
        private$..refresh_net_attr()
      }
    },
    patch_area = function(value){
      if(missing(value)){
        private$..param_patch_area
      } else {
        private$..param_patch_area <- value
        private$..refresh_net_attr()
      }
    },
    epw = function(value){
      if(missing(value)){
        private$..param_epw
      } else {
        private$..param_epw <- value
      }
    },
    sdw = function(value){
      if(missing(value)){
        private$..param_sdw
      } else {
        private$..param_sdw <- value
      }
    }
  )
)
    