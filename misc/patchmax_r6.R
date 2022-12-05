# scenario object generator (read this into memory)
patchmax_generator <- R6Class(
  classname = "patch_generator",
  
  #############################################################################
  # PRIVATE ELEMENTS
  # internal aspects of the patchmax generator
  #############################################################################
  
  private = list(
    ..net = NULL,
    ..geom = NULL,
    ..cpp_graph = NULL,
    ..param_id_field = NULL,
    ..param_objective_field = NULL,
    ..param_area_field = NULL,
    ..param_patch_area = NULL,
    ..param_availability = NULL,
    ..param_constraint_field = NULL,
    ..param_constraint_max = Inf,
    ..param_constraint_min = -Inf,
    ..param_sdw = 1,
    ..param_epw = 1,
    ..patch_mem = NULL,
    ..best_mem = NULL,

    ..update_dist = function(){
      # build cpp graph object     
      cpp_graph <- build_graph_func(
        # net = private$..net, 
        net = delete_vertices(private$..net, V(private$..net)$patch_id > 0),
        objective_field = private$..param_objective_field, 
        sdw = private$..param_sdw, 
        epw = private$..param_epw)
      # private$..ccp_graph = cpp_graph
      return(cpp_graph)
    },
    
    ..update_net = function(){
      
      net <- private$..net
     
      # modify area and distance based on availability 
      net <- set_threshold_func(net, V(net)$available)
      
      # remove stands assigned a patch id
      net <- delete_vertices(net, V(net)$patch_id > 0)
      return(net)
    },
    
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

    ..refresh_net_attr = function(){
      # attribute adjacency network
      if(!is.null(private$..param_objective_field)){
        vertex_attr(private$..net, name = 'objective') <- 
          vertex_attr(private$..net, private$..param_objective_field)
      }
      if(!is.null(private$..param_constraint_field)){
        vertex_attr(private$..net, name = 'constraint') <- 
          vertex_attr(private$..net, private$..param_constraint_field)
      }
      if(!is.null(private$..param_area_field)){
        vertex_attr(private$..net, name = 'area') <- 
          vertex_attr(private$..net, private$..param_area_field)
      }
    }
  ),
  
  #############################################################################
  # PUBLIC ELEMENTS
  # external methods for running patchmax
  #############################################################################
  
  public = list(
    
    # INITIALIZE METHOD --------------------------------------------------------
    initialize = function(geom, id_field=NULL, objective_field=NULL, area_field=NULL, patch_area=NULL){
      if(!missing(geom)){
        
        # save geometry
        private$..geom <- geom
        private$..geom$patch_id = 0
        private$..geom$available = 1
        
        # parameters
        private$..param_id_field = id_field
        private$..param_objective_field = objective_field
        private$..param_area_field = area_field
        private$..param_patch_area = patch_area 
        
        # build adjacency network
        private$..net <- calc_adj_network_func(
          shp = geom, 
          St_id = dplyr::pull(geom, id_field), 
          calc_dist = TRUE)
        
        # add addition fields to adjacency network
        vertex_attr(private$..net) <- bind_cols(vertex_attr(private$..net), st_drop_geometry(geom))
        vertex_attr(private$..net, name = 'exclude') = 0
        vertex_attr(private$..net, name = 'patch_id') = 0
        vertex_attr(private$..net, name = 'available') = 1
        
        private$..refresh_net_attr()
        
      } else {
        stop('Geometry required')
      }
    },
    
    # GROW METHOD --------------------------------------------------------
    
    grow = function(node=NULL){
      
      if(is.null(node)){
        node <- private$..best_mem
      }
      
      # private$..update_dist()
      private$..check_req_fields()
      
      patch <- grow_patch_func(
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
      
      private$..update_dist()
      private$..check_req_fields()
      
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
        plot(pdat[,'search_out'])
      }
      
      best_out = names(search_out)[which.max(search_out)]
      message(glue::glue('\nBest start: {best_out} ({round(sample_frac*100)}% searched)'))
      private$..best_mem <- best_out
      
      if(return_all)
        private$..geom$search_score <- search_out
      
      return(invisible(self))
    },
    
    # PLOT METHOD --------------------------------------------------------
    
    plot = function(plot_field = NULL){
      
      if(is.null(private$..patch_mem)){
        message('No patch currently selected')
      }

      # set border colors
      selected <- private$..geom$stand_id %in% private$..patch_mem$node
      border_col <- rep(NA, length(selected))
      border_col[selected] <- 'black'
      unavailable <- private$..geom$stand_id %in% private$..patch_mem$node[private$..patch_mem$available == 0]
      border_col[unavailable] <- 'red'
        
      plot_field <- ifelse(is.null(plot_field),  private$..param_objective_field, plot_field)
      
      plot(private$..geom[,plot_field], 
             border=border_col, lwd=2,
             pal=sf.colors, key.pos = NULL)

      return(invisible(self))
    },
    
    # APPLY METHOD --------------------------------------------------------
    # (work in progress)
    
    record = function(patch_id = NULL){
      if(is.null(private$..patch_mem))
        stop('No patch. Run search or select first.')

      if(is.null(patch_id)){
        patch_id = max(V(private$..net)$patch_id) + 1
      }

      V(private$..net)$patch_id[match(private$..patch_mem$node, V(private$..net)$stand_id)] = patch_id
      private$..geom$patch_id[match(private$..patch_mem$node, private$..geom$stand_id)] = patch_id
      # private$..net <- subtract_patch_func(private$..net, private$..patch_mem)
      
      message(glue::glue('Patch {patch_id} recorded'))
      private$..patch_mem <- NULL
      
      return(invisible(self))
    },
    
    # DESCRIBE METHOD --------------------------------------------------------
    # (work in progress)
    
    describe = function(){
      private$..geom %>% st_drop_geometry() %>% group_by(patch_id) %>% summarize_if(is.numeric, sum)
    }
  ),
  
  #############################################################################
  # ACTIVE BINDINGS 
  # used for getting and setting private elements
  #############################################################################
  
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
      private$..patch_mem
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
    